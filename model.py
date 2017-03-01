#!/usr/bin/env python

from __future__ import division

import argparse
from copy import deepcopy
import locale
import itertools
from math import isnan
import os
import random
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor, export_graphviz, _tree
from sklearn.feature_extraction import DictVectorizer
import joblib

from traceback import print_tb

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
    implementations = \
        [ 'edge-list-blockreduce', 'edge-list'
        , 'edge-list-warpreduce', 'rev-edge-list-blockreduce'
        , 'rev-edge-list', 'rev-edge-list-warpreduce'
        , 'vertex-pull-blockreduce'
        , 'vertex-pull', 'vertex-pull-warpreduce'
        , 'vertex-push-blockreduce', 'vertex-push'
        , 'vertex-push-warp-blockreduce', 'vertex-push-warp'
        , 'vertex-push-warp-warpreduce', 'vertex-push-warpreduce']

    def __init__(self):
        self.encoder = DictVectorizer()
        self.encoder.fit(map(lambda x: dict(algo=x), Properties.implementations))

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
            def getLookupProps(graph, _):
                graph = graph.split(':')[0]
                return tuple(table[graph, k, l] for l in labels)
            return getLookupProps

        def lookupSize(table, k, _):
            def getSize(graph, level):
                tmp = graph.split(':')[0]
                div = 1 if k == 'abs' else graphProps[tmp,'abs','vertex-count']
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

    def encode(self, data):
        return self.encoder.transform(data).toarray()

    def decode(self, data):
        def convert(d):
            unset = False
            value = 'unknown'
            for k, v in d.items():
                label, value = k.split('=')
                if v != 1 or label != 'algo' or unset:
                    print "Decode error:", d
                    exit(1)

                unset = True
            return value

        if len(data.shape) == 1:
            data = data.reshape(1, -1)
        return map(convert, self.encoder.inverse_transform(data))

    def loadPaths(self, paths):
        dims = [d for d in measurementDims if d != 'timer'] + ['depth']

        frontiers = loadData(int, dims, paths, ext=".frontier",
                            process_line=process_frontier_line,
                            store_paths=False) \
                            .filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer') \
                            .collapseDim('device', 'TitanX') \
                            .collapseDim('algorithm', 'bfs') \
                            .collapseDim('sorting', 'normal') \
                            .transposeNamedDims('implementation', 'graph')

        for k, frontier in frontiers.sparseitems():
            self.visited[k[1],k[2]] = frontier

        for k, frontier in self.visited.sparseitems():
            self.frontiers[k] = frontier

        for graph in self.visited:
            runningTotal = 0
            def depthToNum(s): return int(s[len("bfsLevel"):])
            for depth in sorted(self.visited[graph], key=depthToNum):
                frontier = self.visited[graph,depth]
                self.visited[graph,depth] = runningTotal
                runningTotal += frontier


    def getterFromParams(self, params):
        propGetters = []

        def getProps(graph, level):
            result = ()
            for get in propGetters:
                result += get(graph, level)
            return result

        for cat, version in params:
            propData = self[cat][version]
            propGetters.append(propData.getProps)

        return getProps

def stringifyTuples(l):
    return ".".join(str(k) + ":" + str(v) for k,v in l)

def tuplifyString(s):
    return map(lambda x: x.split(':'), s.split('.'))

class Range(object):
    def __init__(self, name):
        self.name = name
        self.min = float("-inf")
        self.max = float("inf")

    def setMin(self, newMin):
        if newMin > self.min:
            self.min = newMin

    def setMax(self, newMax):
        if newMax < self.max:
            self.max = newMax

    def __repr__(self):
        return self.name + ": " + str(self.min) + " - " + str(self.max)

class RangeDict(dict):
    def __init__(self, *names):
        super(RangeDict, self).__init__()
        for k in names:
            self[k] = Range(k)

    def __repr__(self):
        result = "RangeDict {\n"
        for k in self:
            result += "\t" + repr(self[k]) + "\n"
        result += "}"
        return result

def validatePrediction(result, sample, real):
    def recurse(tree, node):
        left_child = tree.children_left[node]
        right_child = tree.children_right[node]

        if left_child == _tree.TREE_LEAF:
            value = tree.value[node]
            proba = value / tree.weighted_n_node_samples[node]
            pred = np.argmax(proba, axis=1)

            if (pred != real).any():
                print "Sample:", sample
                print "Real:", real
                print "Found:", pred
        else:
            idx = tree.feature[node]

            if sample[idx] <= tree.threshold[node]:
                recurse(tree, left_child)
            else:
                recurse(tree, right_child)

    recurse(result.predictor.tree_, 0)

def computeRanges(result):
    feature_names = [
        result.labels[i]
        if i != _tree.TREE_UNDEFINED else "undefined!"
        for i in result.predictor.tree_.feature
    ]

    ranges = defaultdict(list)

    def recurse(tree, node, state):
        left_child = tree.children_left[node]
        right_child = tree.children_right[node]

        if left_child == _tree.TREE_LEAF:
            value = tree.value[node]
            proba = value / tree.weighted_n_node_samples[node]
            proba = tuple(proba[:,1])
            ranges[proba].append(state)

        else:
            name = feature_names[tree.feature[node]]
            threshold = tree.threshold[node]

            newState = deepcopy(state)
            newState[name].setMax(threshold)
            recurse(tree, left_child, newState)

            newState = deepcopy(state)
            newState[name].setMin(threshold)
            recurse(tree, right_child, newState)

    recurse(result.predictor.tree_, 0, RangeDict(*[n for n in feature_names if n != "undefined!"]))

    return ranges

properties = Properties()

class Result(object):
    def __init__(self, Estimator, path, name, props, data, percentage,
                 unload=False, cached=True, seed=None, **settings):

        if name:
            encode = lambda x: x
            decode = lambda x: x
        else:
            encode = properties.encode
            decode = properties.decode

        self.labels = ()
        self.params = props
        for k, v in props:
            self.labels += properties[k][v].labels

        loaded = False
        self.filename = path + "/"
        if name:
            self.filename += name + "_"

        self.filename += Estimator.__name__ + ("." if settings else "") \
                      + stringifyTuples(settings) + "_" \
                      + str(percentage).replace('.',',') + "_" + str(seed) \
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

    @staticmethod
    def splitPath(path, estimator):
        path, fileName = os.path.split(path)
        fileName, ext = os.path.splitext(fileName)
        if ext != ".model":
            print "Not a model file:", path
            exit(1)

        nameSplit = fileName.split('_')
        if len(nameSplit) == 4:
            name = ""
            est, percent, seed, props = nameSplit
        elif len(nameSplit) == 5:
            name, est, percent, seed, props = nameSplit
        else:
            print "Invalid naming format:", name
            exit(1)

        props = tuplifyString(props)
        percent = float(percent.replace(',', '.'))

        if est != estimator.__name__:
            print "Wrong estimator!"
            exit(1)

        est = estimator

        return ([est, path, name, props, None, percent], {'seed' : int(seed)})

    @staticmethod
    def resultsFromFile(path, estimator, unload=False, cached=True):
        args, kwargs = Result.splitPath(path, estimator)
        return Result(*args, unload=unload, cached=cached, **kwargs)

    @staticmethod
    def genericPredictor(predictor):
        if isinstance(predictor, dict):
            def lookup(props):
                result = []
                for k, v in predictor.items():
                    result.append((k, v.predictor.predict([props])))

                result = list(x[0] for x in sorted(result, key=lambda x: x[1]))
                return result

            lookup.params = predictor.values()[0].params
        else:
            def lookup(props):
                raw = predictor.predictor.predict([props])
                validatePrediction(predictor, props, raw)
                return properties.decode(raw)

            lookup.params = predictor.params

        return lookup

    @staticmethod
    def predictorFromFile(path, estimator, unload=False, cached=True):
        args, kwargs = Result.splitPath(path, estimator)

        if args[2]:
            result = dict()
            for impl in Properties.implementations:
                args[2] = impl
                result[impl] = Result(*args, unload=unload, cached=cached, **kwargs)
        else:
            result = Result(*args, unload=unload, cached=cached, **kwargs)

        return Result.genericPredictor(result)

def labelFloats(labels, values):
    return " ".join(l + ": {:.3f}".format(v) for l, v in zip(labels, values))

def loopProps(paths, props, fun):
    properties.loadPaths(paths)

    labelledProps = []
    for p in properties:
        labelledProps.append(map(lambda x: (p, x), props[p]))

    for params in itertools.product(*labelledProps):
        skip = True
        for _, val in params:
            if val != 'none':
                skip = False
                break

        if skip:
            continue

        fun(params, properties.getterFromParams(params))

def genModels(opts):
    if not opts.paths:
        opts.paths = [opts.model_path]

    runtimes = loadData(Measurement, measurementDims, opts.paths,
                        store_paths=False) \
                       .filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer') \
                       .collapseDim('device', 'TitanX') \
                       .collapseDim('algorithm', 'bfs') \
                       .collapseDim('sorting', 'normal')

    if opts.direct:
        runtimes = runtimes.transposeNamedDims('graph', 'timer', 'implementation')

        def work(params, getProps):
            data = []
            for graph in runtimes:
                for level in runtimes[graph]:
                    timings = runtimes[graph,level].sparseitems()
                    fastest = sorted(timings, key=lambda k: k[1].avg)
                    fastest = {'algo':fastest[0][0][0]}
                    data.append(((graph, level), getProps(graph, level), fastest))

            Result(opts.classifier, opts.model_path, "", params, data,
                    opts.percent, seed=opts.seed, unload=opts.unload)
    else:
        runtimes = runtimes.transposeNamedDims('implementation', 'graph')

        def work(params, getProps):
            for impl in runtimes:
                data = []
                for key, time in runtimes[impl].sparseitems():
                    graph, level = key
                    data.append((key, getProps(graph, level), time.avg))

                Result(opts.classifier, opts.model_path, impl, params,
                        data, opts.percent, seed=opts.seed,
                        unload=opts.unload)

    loopProps(opts.paths, opts.properties, work)

def plotModelRanges(result, props):
    predictionRanges = computeRanges(result)
    names = []
    minCoord = []
    maxCoord = []
    def computeMinMax(predictionDims):
        if not names:
            names.extend(predictionDims.keys())
            minCoord.extend([float("inf")] * len(names))
            maxCoord.extend([float("-inf")] * len(names))

        start = []
        end = []
        for i, n in enumerate(names):
            if predictionDims[n].min != float("-inf"):
                minCoord[i] = min(minCoord[i], predictionDims[n].min)

            if predictionDims[n].max != float("inf"):
                maxCoord[i] = max(maxCoord[i], predictionDims[n].max)

            start.append(predictionDims[n].min)
            end.append(predictionDims[n].max)

        return (start, end)

    for k, ranges in predictionRanges.items():
        predictionRanges[k] = map(computeMinMax, ranges)

    with Plot(stringifyTuples(props)) as ax:
        plotRectangles(ax, predictionRanges, minCoord, maxCoord, names)
        ax.set_yscale('linear')
        ax.set_xscale('linear')

def exportModels(opts):
    for path in opts.paths:
        result = Result.resultsFromFile(path, opts.classifier)
        plotModelRanges(result, result.params)

def exportAllModels(opts):
    def work(params, getProps):
        result = Result(opts.classifier, opts.model_path, "", params,
                        None, opts.percent, seed=opts.seed,
                        unload=opts.unload, cached=False)

        plotModelRanges(result, params)

    loopProps([opts.model_path], opts.properties, work)

def loadRuntimes(paths):
    runtimes = loadData(Measurement, measurementDims, paths,
                        store_paths=False) \
                       .filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer') \
                       .collapseDim('device', 'TitanX') \
                       .collapseDim('algorithm', 'bfs') \
                       .collapseDim('sorting', 'normal') \
                       .transposeNamedDims('graph', 'timer', 'implementation')

    def runPerGraphLevel(task, getProps, getPrediction):
        for graph in runtimes:
            for level in runtimes[graph]:
                times = runtimes[graph,level].transform(lambda x: x.avg)
                real = sorted(times.sparseitems(), key=lambda x: x[1])
                props = getProps(graph, level)
                pred = getPrediction(props)
                real = OrderedDict((k[0], v) for k, v in real)
                task(graph, pred, real)

    return runPerGraphLevel

def runTaskForAll(opts):
    runPerGraphLevel = loadRuntimes(opts.paths)

    if opts.direct:
        def getPredictor(params):
            result = Result(opts.classifier, opts.model_path, "", params,
                            None, opts.percent, seed=opts.seed,
                            unload=opts.unload, cached=False)
            return Result.genericPredictor(result)

    else:
        def getPredictor(params):
            results = dict()
            for impl in Properties.implementations:
                results[impl] = Result(opts.classifier, opts.model_path, impl,
                                    params, None, opts.percent,
                                    seed=opts.seed, unload=opts.unload,
                                    cached=False)
            return Result.genericPredictor(results)

    def work(params, getProps):
        predictor = getPredictor(params)
        with opts.task(params, opts) as task:
            runPerGraphLevel(task, getProps, predictor)

    loopProps(opts.paths, opts.properties, work)
    opts.task.finish(opts)

def runTaskForSome(opts):
    runPerGraphLevel = loadRuntimes(opts.paths)
    properties.loadPaths(opts.paths)

    predictor = Result.predictorFromFile(opts.model_path, opts.classifier)
    getProps = properties.getterFromParams(predictor.params)

    with opts.task(predictor.params, opts) as task:
        runPerGraphLevel(task, getProps, predictor)

    opts.task.finish(opts)

class Validate(object):
    ranking = []
    def __init__(self, params, opts):
        if opts.direct:
            length = 1
        else:
            length = len(list(Properties.implementations))

        self.params = params
        self.direct = opts.direct
        self.relErrors = length * [0]
        self.totalCorrect = 0
        self.firstCorrect = 0
        self.errors = 0
        self.unknown = 0
        self.total = 0

    def __enter__(self):
        def runTask(graph, pred, real):
            if pred == []:
                self.errors += 1
            elif pred[0] == list(real)[0]:
                self.firstCorrect += 1

                if not self.direct and pred == list(real):
                    self.totalCorrect += 1
                else:
                    for i, (p, r) in enumerate(zip(pred, real)):
                        self.relErrors[i] += abs(real[p] - real[r])/real[r]
            else:
                self.errors += 1
                for i, (p, r) in enumerate(zip(pred, real)):
                    if p == 'unknown':
                        self.unknown += 1
                    else:
                        self.relErrors[i] += abs(real[p] - real[r])/real[r]
            self.total += 1
        return runTask

    def __exit__(self, type, value, traceback):
        if type is not None:
            print "type:", type
            print "value:", value
            print "traceback:"
            print_tb(traceback)
            exit(1)

        val = (self.params, self.firstCorrect/self.total)
        val += ([e/self.total for e in self.relErrors],)
        val += (self.unknown/self.total,)
        if not self.direct:
            val += (self.totalCorrect/self.total,)
        self.ranking.append(val)

    @staticmethod
    def finish(opts):
        ordering = 1
        rev = False
        if not opts.direct and opts.sorting == 'overall':
            ordering = -1
        elif opts.sorting == 'rel-error':
            ordering = 2
            rev = True
        elif opts.sorting == 'unknown-pred':
            ordering = 3
            rev = True

        final = sorted(Validate.ranking, key=lambda k: k[ordering], reverse=rev)
        for tup in final:
            params, first, relErrors, unknown = tup[0], tup[1], tup[2], tup[3]
            print "Inputs:", params
            print "First correctness:", first
            if not opts.direct:
                print "Correctness:", tup[-1]
            else:
                relErrors = relErrors[0]
            print "Average Relative Error:\n", relErrors
            print "Unknown predictions:", tup[3]

class Predict(object):
    ranking = []
    def __init__(self, params, opts):
        self.params = params
        self.verbose = opts.verbose

    def __enter__(self):
        self.results = defaultdict(lambda: defaultdict(int))
        def runTask(graph, pred, real):
            for impl, time in real.items():
                self.results[graph][impl] += time

            if pred[0] == 'unknown':
                self.results[graph]['predicted'] == float("NaN")
            else:
                self.results[graph]['predicted'] += real[pred[0]]
            self.results[graph]['optimal'] += min(real.values())

        return runTask

    def __exit__(self, type, value, traceback):
        relError = 0
        relImprovement = 0
        count = 0
        predictionFailures = 0
        averagePredicted = 0
        averageBest = 0

        predictions = dict()

        for graph, vals in self.results.items():
            if not isnan(vals['predicted']):
                relError += abs(vals['optimal'] - vals['predicted']) / vals['optimal']
            else:
                predictionFailure += 1

            count += 1
            optimal = vals['optimal']
            predicted = vals['predicted']
            del vals['optimal']
            del vals['predicted']
            times = sorted(vals.items(), key=lambda k: k[1])

            if self.verbose:
                predictions[graph] = (optimal, predicted, times)

            averagePredicted += predicted/optimal
            averageBest += times[0][1]/optimal

        data = (self.params, predictionFailures/count, relError/count,
                averagePredicted/count, averageBest/count, predictions)
        self.ranking.append(data)

    @staticmethod
    def finish(opts):
        order = 2
        if opts.sorting == 'pred-fail':
            order = 1
        elif opts.sorting == 'rel-error':
            order = 2
        elif opts.sorting == 'avg-pred':
            order = 3

        final = sorted(Predict.ranking, key=lambda k: k[order], reverse=True)
        for params, predFailures, relError, avgPred, avgBest, preds in final:
            print "Params:", params
            print "Prediction failures:", predFailures
            print "Relative Error:", relError
            print "Average prediction:", avgPred
            print "Average best:", avgBest

            for (graph, (optimal, predicted, times)) in preds.items():
                print "Graph:", graph
                print "\tPredicted:", predicted/optimal

                for k, t in times:
                    print "\t" + k + ":", t/optimal

parser = argparse.ArgumentParser(description='Model validation.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

model_props = argparse.ArgumentParser(add_help=False)
model_props.add_argument('model_path', metavar='MODEL-PATH',
                         help="Path to models.")
model_props.add_argument('--unload', action='store_true',
                         help='Unload models from memory between use to save RAM.')
model_props.add_argument('--direct', action='store_true',
                         help='One model instead of one model per implementation.')
model_props.add_argument('--seed', default=1337, action='store', type=int,
                         help='Seed for work splitting.')
model_props.add_argument('--validation-percentage', default=0.25,
                         action='store', type=float, dest='percent',
                         help='Percent of work to set aside for validation.')

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

defaultProps = defaultdict(set)
for prop, keys in properties.items():
    keys = keys.keys()
    defaultProps[prop] = set(keys)
    model_props.add_argument('--add-' + prop, action=SetUpdater('add'),
                             choices=keys, dest=prop)
    model_props.add_argument('--remove-' + prop, action=SetUpdater('remove'),
                             choices=keys, dest=prop)

class SetClassifier(argparse.Action):
    choices = { 'DecisionTreeClassifier': DecisionTreeClassifier
              , 'DecisionTreeRegressor': DecisionTreeRegressor
              }
    def __call__(self, parser, namespace, value, option_string=None):
        setattr(namespace, self.dest, SetClassifier.choices[value])

model_props.add_argument('--classifier', default=DecisionTreeClassifier,
                         action=SetClassifier, choices=SetClassifier.choices,
                         help='Select classifier to use.')

model_props.set_defaults(properties=defaultProps)

subparsers = parser.add_subparsers()
generate = subparsers.add_parser('generate', parents=[model_props])
generate.add_argument('paths', nargs='*', metavar='PATH',
                      help="Path(s) to read timing results from.")
generate.set_defaults(func=genModels)

exportAll = subparsers.add_parser('export-all', parents=[model_props])
exportAll.set_defaults(func=exportAllModels)

export = subparsers.add_parser('export')
export.add_argument('paths', nargs='+', metavar='MODEL-PATH',
                      help="Model filepaths.")
export.add_argument('--classifier', default=DecisionTreeClassifier,
                    action=SetClassifier, choices=SetClassifier.choices,
                    help='Select classifier to use.')
export.set_defaults(func=exportModels)

validate = argparse.ArgumentParser(add_help=False)
validate.set_defaults(sorting='')
validate.add_argument('--overall', action='store_const', const='overall',
                          dest='sorting', help='Total ranking instead of first correct.')
validate.add_argument('--rel-error', '--relative-error', action='store_const',
                      const='rel-error', dest='sorting',
                      help='Sort by relative error.')
validate.add_argument('--unknown-predictions', '--unknown-pred', action='store_const',
                      const='unknown-pred', dest='sorting',
                      help='Sort by unknown predictions.')
validate.set_defaults(task=Validate)

validateAll = subparsers.add_parser('validate-all', parents=[model_props, validate])
validateAll.add_argument('paths', nargs='+', metavar='PATH',
                      help="Path(s) to read timing results from.")
validateAll.set_defaults(func=runTaskForAll)

validateOne = subparsers.add_parser('validate', parents=[validate])
validateOne.add_argument('model_path', metavar='MODEL-PATH', help="Path to model.")
validateOne.add_argument('paths', nargs='+', metavar='PATH',
                      help="Path(s) to read timing results from.")
# Obsolete classifier flag below
validateOne.add_argument('--classifier', default=DecisionTreeClassifier,
                         action=SetClassifier, choices=SetClassifier.choices,
                         help='Select classifier to use.')
validateOne.add_argument('--direct', action='store_true',
                         help='One model instead of one model per implementation.')
validateOne.set_defaults(func=runTaskForSome)

predict = argparse.ArgumentParser(add_help=False)
predict.set_defaults(sorting='')
predict.add_argument('--avg-pred', '--average-pred', '--avg-prediction',
                     '--average-prediction', action='store_const',
                     const='avg-pred', dest='sorting',
                     help='Sort by best average prediction')
predict.add_argument('--rel-error', '--relative-error', action='store_const',
                     const='rel-error', dest='sorting',
                     help='Sort by relative error.')
predict.add_argument('--pred-fail', '--pred-failures', '--prediction-fail',
                     '--prediction-failures', action='store_const',
                     const='pred-fail', dest='sorting',
                     help='Sort by relative error.')
predict.add_argument('--verbose', '-v', action='store_true',
                     help='Print output per graph.')
predict.set_defaults(task=Predict)

predictAll = subparsers.add_parser('predict-all', parents=[model_props, predict])
predictAll.add_argument('paths', nargs='+', metavar='PATH',
                        help="Path(s) to read timing results from.")
predictAll.set_defaults(func=runTaskForAll)

predictOne = subparsers.add_parser('predict', parents=[predict])
predictOne.add_argument('model_path', metavar='MODEL-PATH',
                        help="Path to model.")
predictOne.add_argument('paths', nargs='+', metavar='PATH',
                        help="Path(s) to read timing results from.")
# Obsolete classifier flag below
predictOne.add_argument('--classifier', default=DecisionTreeClassifier,
                     action=SetClassifier, choices=SetClassifier.choices,
                     help='Select classifier to use.')
predictOne.set_defaults(func=runTaskForSome)

options = parser.parse_args()
options.func(options)
