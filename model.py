#!/usr/bin/env python

from __future__ import division

import argparse
import locale
import itertools
import os
import random
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor, export_graphviz
from sklearn.feature_extraction import DictVectorizer
import joblib

from sys import exit
from plot_funs import *

if __name__ != "__main__":
    exit(1)

locale.setlocale(locale.LC_NUMERIC, "")

def randomSplit(data, percentage):
    dataSize = len(data)
    splitSize = int(round(percentage * dataSize))
    for i in xrange(0, splitSize):
        idx = random.randint(0, dataSize - 1)
        dataSize -= 1
        data[-i], data[idx] = data[idx], data[-i]

    return data[:dataSize], data[dataSize:]

class Properties(object):
    def __init__(self):
        def createPropTable(table, labels, getGetProps, keys):
            class Content(object):
                def __init__(self, labels, getProps):
                    self.labels = labels
                    self.getProps = getProps

            props = OrderedDict()
            for k in keys:
                props[k] = Content(tuple(k + "-" + l for l in labels),
                                   getGetProps(table, k, labels))
                props['none'] = Content((), lambda graph, level: ())
            return props

        graphProps = Table(float, "graph", "sort", "property")
        self.frontiers = Table(int, "graph", "depth")
        self.visited = Table(int, "graph", "depth")
        self.props = dict()

        for path in glob('*.props'):
            with open(path) as f:
                for line in f:
                    graph, sort, prop, val = line.strip().split(':')
                    graphProps[graph,sort,prop] = float(val.replace(',',''))

        def lookupProps(table, k, labels):
            return lambda graph, _: tuple(table[graph, k, l] for l in labels)

        def lookupSize(table, k, _):
            def getSize(graph, level):
                div = 1 if k == 'abs' else graphProps[graph,'abs','vertex-count']
                return (table[graph, level] / div,)
            return getSize

        self.props['graph-properties'] = \
            createPropTable(graphProps, tuple(graphProps.keys(dim='property')),
                            lookupProps, ['abs', 'in', 'out'])

        self.props['frontier'] = createPropTable(self.frontiers, ["frontier"],
                                                 lookupSize, ['abs', 'rel'])

        self.props['visited'] = createPropTable(self.visited, ["visited"],
                                                lookupSize, ['abs', 'rel'])

    def __iter__(self):
        return iter(self.props)
    def __getitem__(self, key):
        return self.props[key]

    def keys(self):
        return self.props.keys()
    def values(self):
        return self.props.values()
    def items(self):
        return self.props.items()

    def loadPaths(self, paths):
        dims = [d for d in measurementDims if d != 'timer'] + ['depth']

        frontiers = loadData(int, dims, paths, ext=".frontier",
                            process_line=process_frontier_line) \
                            .collapseDim('device', 'TitanX') \
                            .collapseDim('algorithm', 'bfs') \
                            .collapseDim('sorting', 'normal') \
                            .transposeNamedDims('implementation', 'paths', 'graph')

        for k, frontier in frontiers.sparseitems():
            self.visited[k[2],k[3]] = frontier

        for k, frontier in self.visited.sparseitems():
            self.frontiers[k] = frontier

        for graph in self.visited:
            runningTotal = 0
            def depthToNum(s): return int(s[len("bfsLevel"):])
            for depth in sorted(self.visited[graph], key=depthToNum):
                frontier = self.visited[graph,depth]
                self.visited[graph,depth] = runningTotal
                runningTotal += frontier

def stringifyTuples(l):
    return ".".join(str(k) + ":" + str(v) for k,v in l)

properties = Properties()

class Result(object):
    def __init__(self, Estimator, name, props, data, percentage,
                 unload=False, cached=True, seed=None, encoder=None,
                 **settings):

        if encoder is None:
            encode = lambda x: x
            decode = lambda x: x
        else:
            encode = lambda x: encoder.transform(x).toarray()
            decode = lambda x: encoder.inverse_transform(x)

        self.labels = ()
        for k, v in props:
            self.labels += properties[k][v].labels

        loaded = False
        self.filename = name + "_"
        self.filename += Estimator.__name__ + ("." if settings else "") \
                      + stringifyTuples(settings) + "_" \
                      + str(percentage).replace('.',',') + "." + str(seed) \
                      + "_" + stringifyTuples(props) + ".model"

        try:
            with open(self.filename, "rb") as fileObj:
                try:
                    predictor = joblib.load(fileObj)
                    loaded = True
                except ValueError as e:
                    if not e.args[0].startswith("EOF:"):
                        raise e
        except IOError as e:
            if e.args[0] != 2:
                raise e

        if data is None and not loaded:
            raise Exception("Failed to load model from file: " + self.filename)
        elif not loaded:
            random.seed(seed)
            train, test = randomSplit(data, percentage)
            train = zip(*train)
            test = zip(*test)

            predictor = Estimator(**settings)
            predictor = predictor.fit(train[1], encode(train[2]))

            self.results = dict()
            for l, x, y in zip(test[0], decode(predictor.predict(test[1])), test[2]):
                if l not in self.results:
                    self.results[l] = (x, y)
                else:
                    print "Error:",filename,"label:",l
                    exit(1)

        self.feature_importances = list(predictor.feature_importances_)

        if cached and not loaded:
            with open(self.filename, "wb") as fileObj:
                joblib.dump(predictor, fileObj, compress=True)

        if not unload:
            self.predictor = predictor

    def __enter__(self):
        if hasattr(self, 'predictor'):
            return self.predictor
        else:
            with open(self.filename, "rb") as fileObj:
                return joblib.load(fileObj)

    def __exit__(self, exc_type, exc_value, tb):
        return None

    def errors(self):
        return list(x - y for x, y in self.results.values())

    def relErrors(self):
        return list(abs(x - y)/x for x,y in self.results.values())

def labelFloats(labels, values):
    return " ".join(l + ": {:.3f}".format(v) for l, v in zip(labels, values))

def loopProps(paths, props, fun):
    properties.loadPaths(paths)

    labelledProps = []
    for p in properties:
        labelledProps.append(map(lambda x: (p, x), props[p]))

    for categories in itertools.product(*labelledProps):
        params = []
        propGetters = []

        def getProps(graph, level):
            result = ()
            for get in propGetters:
                result += get(graph, level)
            return result

        skip = True
        for cat, version in categories:
            if version != 'none':
                skip = False

            propData = properties[cat][version]
            params.append((cat, version))
            propGetters.append(propData.getProps)

        if skip:
            continue

        fun(params, getProps)

def genModels(opts):
    runtimes = loadData(Measurement, measurementDims, [opts.path])
    runtimes = runtimes.filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer')
    runtimes = runtimes.collapseDim('device', 'TitanX') \
                       .collapseDim('algorithm', 'bfs') \
                       .collapseDim('sorting', 'normal') \
                       .collapseDim('paths', opts.path) \
                       .transposeNamedDims('implementation', 'graph')

    def work(params, getProps):
        results = dict()
        for impl in runtimes:
            data = []
            for key, time in runtimes[impl].sparseitems():
                graph, level = key
                data.append((key, getProps(graph, level), time.avg))

            filename = opts.path + "/" + impl
            results[impl] = Result(opts.classifier, filename, params,
                                   data, opts.percent, seed=opts.seed,
                                   unload=opts.unload, cached=opts.cached)

    loopProps([opts.path], opts.properties, work)

def genDirectModels(opts):
    runtimes = loadData(Measurement, measurementDims, [opts.path])
    runtimes = runtimes.filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer')
    runtimes = runtimes.collapseDim('device', 'TitanX') \
                       .collapseDim('algorithm', 'bfs') \
                       .collapseDim('sorting', 'normal') \
                       .collapseDim('paths', opts.path) \
                       .transposeNamedDims('graph', 'timer', 'implementation')

    enc = DictVectorizer()
    vals = sorted(runtimes.keys(dim='implementation'))
    enc.fit(map(lambda x: dict(algo=x), vals))

    def work(params, getProps):
        data = []
        for graph in runtimes:
            for level in runtimes[graph]:
                real = sorted(runtimes[graph,level].sparseitems(), key=lambda k: k[1].avg)
                real = {'algo':real[0][0][0]}
                data.append(((graph, level), getProps(graph, level), real))

        result = Result(opts.classifier, opts.path + "/", params, data,
                        opts.percent, seed=opts.seed, unload=opts.unload,
                        cached=opts.cached, encoder=enc)

    loopProps([opts.path], opts.properties, work)

def exportModels(opts):
    for path in opts.paths:
        with open(path, 'rb') as fileObj:
            outName = os.path.splitext(path)[0] + ".dot"
            export_graphviz(joblib.load(fileObj), out_file=outName)

def runPerGraphLevel(opts, getTask):
    runtimes = loadData(Measurement, measurementDims, [opts.path])
    runtimes = runtimes.filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer')
    runtimes = runtimes.collapseDim('device', 'TitanX') \
                       .collapseDim('algorithm', 'bfs') \
                       .collapseDim('sorting', 'normal') \
                       .collapseDim('paths', opts.path) \
                       .transposeNamedDims('graph', 'timer', 'implementation')

    if opts.direct:
        enc = DictVectorizer()
        vals = sorted(runtimes.keys(dim='implementation'))
        enc.fit(map(lambda x: dict(algo=x), vals))

        def getPredictor(params):
            result = Result(opts.classifier, opts.model_path + "/", params,
                            None, opts.percent, seed=opts.seed,
                            unload=opts.unload, cached=False, encoder=enc)

            def lookup(props):
                pred = result.predictor.predict([props])
                try:
                    pred = enc.inverse_transform(pred)
                    pred = pred[0].keys()[0].split('=')[1]
                except:
                    pred = []
                return [pred]

            return lookup

    else:
        def getPredictor(params):
            results = dict()
            for impl in runtimes.keys(dim='implementation'):
                filename = opts.model_path + "/" + impl
                results[impl] = Result(opts.classifier, filename, params,
                                    None, opts.percent, seed=opts.seed,
                                    unload=opts.unload, cached=False)
            def lookup(props):
                result = []
                for k, v in results.items():
                    result.append((k, v.predictor.predict([props])))

                result = list(x[0] for x in sorted(result, key=lambda x: x[1]))
                return result
            return lookup

    def work(params, getProps):
        getPrediction = getPredictor(params)

        task, finishTask = getTask(runtimes)
        for graph in runtimes:
            for level in runtimes[graph]:
                times = runtimes[graph,level].transform(lambda x: x.avg)
                real = sorted(times.sparseitems(), key=lambda x: x[1])
                pred = getPrediction(getProps(graph, level))
                real = OrderedDict((k[0], v) for k, v in real)
                task(graph, pred, real)

        finishTask(params)

    loopProps([opts.path], opts.properties, work)

def validateModels(opts):
    final = []
    def getTask(data):
        task = type('', (), {})()
        if opts.direct:
            length = 1
        else:
            length = len(list(data.keys(dim='implementation')))

        task.relErrors = length * [0]
        task.totalCorrect = 0
        task.firstCorrect = 0
        task.errors = 0
        task.total = 0

        def runTask(graph, pred, real):
            if pred == []:
                task.errors += 1
            elif pred[0] == list(real)[0]:
                task.firstCorrect +=1

                if not opts.direct and pred == list(real):
                    task.totalCorrect += 1
                else:
                    for i, (p, r) in enumerate(zip(pred, real)):
                        task.relErrors[i] += abs(real[p] - real[r])/real[r]
            task.total += 1

        def finishTask(params):
            val = (params, task.firstCorrect/task.total)
            val += ([e/task.total for e in task.relErrors],)
            if not opts.direct:
                val += (task.totalCorrect/task.total,)
            final.append(val)

        return runTask, finishTask

    runPerGraphLevel(opts, getTask)

    direction = False
    ordering = 1
    if not opts.direct and opts.sorting == 'overall':
        ordering = -1
    elif opts.sorting == 'rel-error':
        ordering = 2
        direction = True

    final = sorted(final, key=lambda k: k[ordering], reverse=direction)
    for tup in final:
        params, first, relErrors = tup[0], tup[1], tup[2]
        print "Inputs:", params
        print "First correctness:", first
        if not opts.direct:
            print "Correctness:", tup[-1]
        else:
            relErrors = relErrors[0]
        print "Average Relative Error:\n", relErrors

def computePrediction(opts):
    def getTask(data):
        results = defaultdict(lambda: defaultdict(int))

        def runTask(graph, pred, real):
            for impl, time in real.items():
                results[graph][impl] += time

            results[graph]['predicted'] += real[pred[0]]
            results[graph]['optimal'] += min(real.values())

        def finishTask(params):
            relError = 0
            relImprovement = 0
            count = 0
            print "Params:",params
            for graph, vals in results.items():
                print "Graph:", graph
                relError += abs(vals['optimal'] - vals['predicted']) / vals['optimal']
                count += 1
                best = vals['optimal']
                print "\tPredicted:", vals['predicted']/best
                del vals['optimal']
                del vals['predicted']
                times = sorted(vals.items(), key=lambda k: k[1])

                #fastest = [v for k, v in times][0]
                #relImprovement += (vals['predicted'] - fastest) / fastest

                for k, t in times:
                    print "\t" + k + ":", t/best
            print "Relative Error:", relError/count

        return runTask, finishTask

    runPerGraphLevel(opts, getTask)

parser = argparse.ArgumentParser(description='Model validation.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

common_args = argparse.ArgumentParser(add_help=False)
common_args.add_argument('path', metavar='PATH',
                        help="Path to read results from.")
common_args.add_argument('--unload', action='store_true',
                      help='Unload models from memory between use to save RAM.')

class Updater(argparse.Action):
    reset = dict()

    def __init__(self, action, option_strings, dest, nargs=None, **kwargs):
        if nargs is not None:
           raise ValueError("nargs not allowed")

        super(Updater, self).__init__(option_strings, dest, **kwargs)
        self.action = action
        Updater.reset[dest] = False

    def __call__(self, parser, namespace, values, option_string=None):
        if self.action == 'add':
            if not Updater.reset[self.dest]:
                namespace.properties[self.dest] = set()
                Updater.reset[self.dest] = True
            namespace.properties[self.dest] |= set([values])
        elif self.action == 'remove':
            namespace.properties[self.dest] -= set([values])
        else:
            raise ValueError("Invalid action!")

def SetUpdater(action):
    def _(*args, **kwargs):
        return Updater(action, *args, **kwargs)
    return _

class SetClassifier(argparse.Action):
    choices = { 'DecisionTreeClassifier': DecisionTreeClassifier
              , 'DecisionTreeRegressor': DecisionTreeRegressor
              }
    def __call__(self, parser, namespace, value, option_string=None):
        setattr(namespace, self.dest, SetClassifier.choices[value])

defaultProps = defaultdict(set)
for prop, keys in properties.items():
    keys = keys.keys()
    defaultProps[prop] = set(keys)
    common_args.add_argument('--add-' + prop, action=SetUpdater('add'),
                             choices=keys, dest=prop)
    common_args.add_argument('--remove-' + prop, action=SetUpdater('remove'),
                             choices=keys, dest=prop)

common_args.add_argument('--seed', default=1337, action='store', type=int,
                      help='Seed for work splitting.')
common_args.add_argument('--validation-percentage', default=0.25,
                         action='store', type=float, dest='percent',
                         help='Percent of work to set aside for validation.')
common_args.add_argument('--classifier', default=DecisionTreeClassifier,
                         action=SetClassifier, choices=SetClassifier.choices,
                         help='Select classifier to use.')

common_args.set_defaults(properties=defaultProps)

subparsers = parser.add_subparsers()
generate = subparsers.add_parser('generate', parents=[common_args])
generate.add_argument('--uncached', action='store_false', dest='cached',
                      help='Store models on disk.')
generate.add_argument('--direct', action='store_const', const=genDirectModels,
                      dest='func', help='')
generate.set_defaults(func=genModels)

export = subparsers.add_parser('export', parents=[common_args])
export.set_defaults(func=exportModels)

validate = subparsers.add_parser('validate', parents=[common_args])
validate.set_defaults(sorting='')
validate.add_argument('--overall', action='store_const', const='overall',
                      dest='sorting', help='Total ranking instead of first correct.')
validate.add_argument('--rel-error', '--relative-error', action='store_const',
                      const='rel-error', dest='sorting',
                      help='Sort by relative error.')
validate.add_argument('--direct', action='store_true',
                      help='One model instead of one model per implementation.')
validate.add_argument('model_path', metavar='MODEL-PATH',
                        help="Path to read models from.")
validate.set_defaults(func=validateModels)

computeTimes = subparsers.add_parser('times', parents=[common_args])
computeTimes.add_argument('--direct', action='store_true',
                      help='One model instead of one model per implementation.')
computeTimes.add_argument('model_path', metavar='MODEL-PATH',
                        help="Path to read models from.")
computeTimes.set_defaults(func=computePrediction)

options = parser.parse_args()
options.func(options)
