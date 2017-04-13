#!/usr/bin/env python

from __future__ import division

import argparse
from copy import deepcopy
import locale
import itertools
from math import isnan
import os
import random
import struct
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor, export_graphviz, _tree
from sklearn.feature_extraction import DictVectorizer
import joblib

from traceback import print_tb

from sys import exit, stderr
from plot_funs import *

if __name__ != "__main__":
    exit(1)

locale.setlocale(locale.LC_NUMERIC, "")
totalVisited = dict()

def isBipartite(graph):
    graph, level = graph.split(':')
    return graph in set(['actor-movie', 'amazon-ratings', 'brunson_revolution',
        'opsahl-collaboration', 'bibsonomy-2ti', 'bibsonomy-2ui',
        'bibsonomy-2ut', 'bookcrossing_full-rating', 'bookcrossing_rating',
        'citeulike-ti', 'citeulike-ui', 'citeulike-ut',
        'brunson_club-membership', 'brunson_corporate-leadership',
        'dbpedia-country', 'moreno_crime', 'dblp-author', 'dbpedia-genre',
        'delicious-ti', 'delicious-ui', 'delicious-ut', 'digg-votes',
        'discogs_affiliation', 'discogs_genre', 'discogs_style',
        'discogs_lgenre', 'discogs_lstyle', 'epinions-rating',
        'flickr-groupmemberships', 'github', 'jester1', 'jester2',
        'lastfm_band', 'lastfm_song', 'lkml_person-thread',
        'livejournal-groupmemberships', 'dbpedia-location',
        'movielens-100k_rating', 'movielens-10m_rating', 'movielens-1m',
        'movielens-10m_ti', 'movielens-10m_ui', 'movielens-10m_ut',
        'dbpedia-starring', 'dbpedia-occupation', 'orkut-groupmemberships',
        'dbpedia-producer', 'dbpedia-recordlabel', 'reuters',
        'gottron-reuters', 'escorts', 'brunson_south-africa',
        'opsahl-southernwomen', 'brunson_southern-women',
        'stackexchange-stackoverflow', 'dbpedia-team', 'gottron-trec',
        'dbtropes-feature', 'munmun_twitterex_ut', 'opsahl-ucforum',
        'unicodelang', 'pics_ti', 'pics_ui', 'pics_ut', 'trackers',
        'edit-enwikibooks', 'edit-frwikibooks', 'edit-enwikinews',
        'edit-frwikinews', 'edit-dewiki', 'edit-enwiki', 'gottron-excellent',
        'wiki-en-cat', 'edit-eswiki', 'edit-frwiki', 'edit-itwiki',
        'edit-enwikiquote', 'edit-dewiktionary', 'edit-enwiktionary',
        'edit-frwiktionary', 'dbpedia-writer', 'youtube-groupmemberships'])

def stringifyTuples(l):
    return ".".join(str(k) + ":" + str(v) for k,v in l)

def tuplifyString(s):
    return map(lambda x: x.split(':'), s.split('.'))

def labelFloats(labels, values):
    return " ".join(l + ": {:.3f}".format(v) for l, v in zip(labels, values))

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
        self.encoder.fit(map(lambda x: dict(algo=x), self.implementations))

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

        self.graphProps = Table(float, "graph", "sort", "property")
        self.frontiers = Table(int, "graph", "depth")
        self.visited = Table(int, "graph", "depth")
        self.props = dict()

        for fileName in glob('*.props'):
            with open(fileName) as f:
                for line in f:
                    graph, sort, prop, v = line.strip().split(':')
                    self.graphProps[graph,sort,prop] = float(v.replace(',',''))

        def lookupProps(table, k, labels):
            def getLookupProps(graph, _):
                graph = graph.split(':')[0]
                return tuple(table[graph, k, l] for l in labels)
            return getLookupProps

        def lookupSize(table, k, _):
            def getSize(graph, level):
                graphNoRoot = graph.split(':')[0]
                if k == 'abs':
                    div = 1
                else:
                    div = self.graphProps[graphNoRoot,'abs','vertex-count']
                return (table[graph, level] / div,)
            return getSize

        keys = tuple(self.graphProps.keys(dim='property'))
        self.props['graph-properties'] = \
            createPropTable(self.graphProps, keys,
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
            for depth in sorted(self.visited[graph], key=depthToNum):
                frontier = self.visited[graph,depth]
                self.visited[graph,depth] = runningTotal
                runningTotal += frontier
            totalVisited[graph] = runningTotal

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
    tree = result.predictor.tree_
    sample = np.asarray(sample, dtype=np.float32).reshape(1, -1)

    last = 0
    node = 0
    while True:
        left_child = tree.children_left[node]
        right_child = tree.children_right[node]

        idx = tree.feature[node]
        if left_child == _tree.TREE_LEAF:
            break
        elif sample[0,idx] <= tree.threshold[node]:
            node = left_child
        else:
            node = right_child

    proba = tree.value[node] / tree.weighted_n_node_samples[node]
    pred = np.argmax(proba, axis=1)

    if (pred != real).any():
        print >> stderr, "Sample:", sample
        print >> stderr, "Real:", real
        print >> stderr, "Pred:", pred

def traverseTree(tree, dfsState, leaf, branch):
    queue = [(0, dfsState)]

    while queue:
        node, state = queue.pop(0)
        left_child = tree.children_left[node]
        right_child = tree.children_right[node]

        featureIdx = tree.feature[node]
        threshold = tree.threshold[node]

        if left_child == _tree.TREE_LEAF:
            proba = tree.value[node] / tree.weighted_n_node_samples[node]
            pred = np.argmax(proba, axis=1)
            leaf(node, featureIdx, threshold, pred, state)
        else:
            leftState, rightState = branch(node, featureIdx, threshold,
                                           left_child, right_child, state)

            queue.append((left_child, leftState))
            queue.append((right_child, rightState))

def computeRanges(result):
    feature_names = [
        result.labels[i]
        if i != _tree.TREE_UNDEFINED else "undefined!"
        for i in result.predictor.tree_.feature
    ]

    ranges = defaultdict(list)

    def leaf(node, featureIdx, threshold, pred, state):
        ranges[pred].append(state)

    def branch(node, featureIdx, threshold, left_child, right_child, state):
        name = feature_names[featureIdx]
        leftState = deepcopy(state)
        leftState[name].setMax(threshold)

        rightState = deepcopy(state)
        rightState[name].setMin(threshold)

        return leftState, rightState

    initialState = RangeDict(*[n for n in feature_names if n != "undefined!"])
    traverseTree(result.predictor.tree_, initialState, leaf, branch)

    return ranges

def exportCppModel(result, decode, keepSource):
    tree = result.predictor.tree_
    fileName = stringifyTuples(result.params)
    exportCppModel.count = 0
    arrayTree = []
    names = { 'unknown' : 0, 'edge-list' : 0 }
    translation = { -1 : -1 }

    def leaf(node, featureIdx, threshold, pred, _):
        translation[node] = exportCppModel.count
        exportCppModel.count += 1
        name = names.setdefault(decode(pred)[0], len(names))
        arrayTree.append((threshold, featureIdx, -1, name))

    def branch(node, featureIdx, threshold, left, right, _):
        translation[node] = exportCppModel.count
        exportCppModel.count += 1
        arrayTree.append((threshold, featureIdx, left, right))
        return (), ()

    traverseTree(result.predictor.tree_, (), leaf, branch)

    def translate(data):
        if data[2] != -1:
            data = data[:2] + (translation[data[2]], translation[data[3]])

        return '{ ' + ', '.join(str(x) for x in data) + ' }'

    arrayTree = [translate(x) for x in arrayTree]
    del names['unknown']
    names = sorted(names.items(), key=lambda k: k[1])
    names = ['"' + name + '"' for name, _ in names]

    source = """#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "Timer.hpp"

typedef struct {
    double threshold;
    int idx, left, right;
} tree_t;

static const char *names[] = {
    %s
};

static tree_t tree[] = {
    %s
};

static int lookup(double *params)
{
    int node = 0, left;

    while ((left = tree[node].left) != -1) {
        if (params[tree[node].idx] <= tree[node].threshold) {
            node = left;
        } else {
            node = tree[node].right;
        }
    }
    return tree[node].right;
}

int main(int argc, char** argv)
{
    int numRepeats = 1;
    if (argc == 2) {
        numRepeats = std::stoi(argv[1]);
    }

    int fd = open("%s.binprops", O_RDONLY);
    if (fd == -1) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    struct stat statbuf;
    if (fstat(fd, &statbuf) != 0) {
        perror("stat");
        exit(EXIT_FAILURE);
    }


    void *ptr = mmap(nullptr, static_cast<size_t>(statbuf.st_size), PROT_READ,
                     MAP_SHARED, fd, 0);
    close(fd);

    TimerRegister::set_human_readable(true);
    Timer timer("lookup", 200000);
    unsigned long long *intData = static_cast<unsigned long long *>(ptr);
    double *doubleData = static_cast<double*>(ptr);
    size_t offset = 0;
    auto numParams = intData[offset++];
    auto numIndices = intData[offset++];
    std::vector<unsigned long long> indices;
    for (size_t i = 0; i < numIndices; i++) {
        indices.emplace_back(intData[offset++]);
    }

    double *params = new double[numParams];

    int result = 0;
    auto numGraphs = intData[offset++];
    int dataStart = offset;

    for (int a = 0; a < numRepeats; a++) {
        for (size_t i = 0; i < numGraphs; i++) {
            auto numLookups = intData[offset++];
            timer.reserve(numLookups);
            for (size_t j = 0; j < numParams; j++) {
                params[j] = doubleData[offset++];
            }

            while (1) {
                timer.start();
                result += lookup(params);
                timer.stop();

                if (--numLookups == 0) break;
                for (auto idx : indices) {
                    params[idx] = doubleData[offset++];
                }
            }
        }
        offset = dataStart;
    }
    delete[] params;
    TimerRegister::print_results();

    return result;
}""" % (',\n    '.join(names), ",\n    ".join(arrayTree), fileName)

    with open(fileName + ".cpp", 'w+') as output:
        output.write(source)

    os.system(
        "clang++ --gcc-toolchain=/cm/shared/package/gcc/4.9.3/ -march=native" +
        " -O3 -std=c++14 {0}.cpp Timer.cpp -o {0}".format(fileName))

    if not keepSource:
        os.remove(fileName + '.cpp')

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

        labels = []
        if args[2]:
            result = dict()
            for impl in Properties.implementations:
                args[2] = impl
                result[impl] = Result(*args, unload=unload, cached=cached, **kwargs)
                labels = zip(result[impl].labels,
                             result[impl].feature_importances)
        else:
            result = Result(*args, unload=unload, cached=cached, **kwargs)
            labels = zip(result.labels, result.feature_importances)

        return Result.genericPredictor(result), labels

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

    if opts.skipbipartite:
        filterGraphs = isBipartite
    elif opts.skipnonbipartite:
        filterGraphs = lambda k: not isBipartite(k)
    else:
        filterGraphs = lambda _: False

    if opts.skiplong:
        skipGraphs = lambda n: n > 15
    else:
        skipGraphs = lambda n: False

    runtimes = loadData(Measurement, measurementDims, opts.paths,
                        store_paths=False) \
            .filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer') \
            .filterKeys(filterGraphs, dim='graph') \
            .collapseDim('device', 'TitanX') \
            .collapseDim('algorithm', 'bfs') \
            .collapseDim('sorting', 'normal')

    if opts.direct:
        runtimes = runtimes.transposeNamedDims('graph', 'timer', 'implementation')

        def work(params, getProps):
            data = []
            for graph in runtimes:
                if skipGraphs(len(list(runtimes[graph]))):
                    continue

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

def dumpProperties(opts):
    properties.loadPaths(["."])
    def work(params, getter):
        labels = ()
        for k, v in params:
            labels += properties[k][v].labels

        indices = []
        for i, k in enumerate(labels):
            if k.endswith('-visited') or k.endswith('-frontier'):
                indices.append(i)

        indices.sort()

        with open(stringifyTuples(params) + ".binprops", 'wb+') as output:
            output.write(struct.pack("QQ", len(labels), len(indices)))
            for i in indices:
                output.write(struct.pack("Q", i))

            output.write(struct.pack("Q", len(list(properties.frontiers))))
            for graph in properties.frontiers:
                output.write(struct.pack("Q", len(list(properties.frontiers[graph]))))
                first = True
                for level in properties.frontiers[graph]:
                    props = getter(graph,level)
                    if first:
                        first = False
                        for p in props:
                            output.write(struct.pack("d", p))
                    else:
                        for i in indices:
                            output.write(struct.pack("d", props[i]))

    loopProps([opts.model_path], opts.properties, work)

def exportModels(opts):
    for path in opts.paths:
        result = Result.resultsFromFile(path, opts.classifier)
        exportCppModel(result, properties.decode, opts.keep)

def exportAllModels(opts):
    def work(params, getProps):
        result = Result(opts.classifier, opts.model_path, "", params,
                        None, opts.percent, seed=opts.seed,
                        unload=opts.unload, cached=False)

        exportCppModel(result, properties.decode, opts.keep)

    loopProps([opts.model_path], opts.properties, work)

def plotModels(opts):
    for path in opts.paths:
        result = Result.resultsFromFile(path, opts.classifier)
        plotModelRanges(result, result.params)

def plotAllModels(opts):
    def work(params, getProps):
        result = Result(opts.classifier, opts.model_path, "", params,
                        None, opts.percent, seed=opts.seed,
                        unload=opts.unload, cached=False)

        plotModelRanges(result, params)

    loopProps([opts.model_path], opts.properties, work)

def loadRuntimes(paths, filterGraphs=lambda _: False):
    runtimes = loadData(Measurement, measurementDims, paths,
                        store_paths=False) \
            .filterKeys(lambda k: not k.startswith("bfsLevel"), dim='timer') \
            .filterKeys(filterGraphs, dim='graph') \
            .collapseDim('device', 'TitanX') \
            .collapseDim('algorithm', 'bfs') \
            .collapseDim('sorting', 'normal') \
            .transposeNamedDims('graph', 'timer', 'implementation')

    def runPerGraphLevel(task, getProps, getPrediction):
        for graph in runtimes:
            last = 'edge-list'
            for level in sorted(runtimes[graph], key=depthToNum):
                times = runtimes[graph,level].transform(lambda x: x.avg)
                real = sorted(times.sparseitems(), key=lambda x: x[1])
                props = getProps(graph, level)
                pred = getPrediction(props)
                real = OrderedDict((k[0], v) for k, v in real)
                last = task(graph, pred, real, last)

    return runPerGraphLevel

def runTaskForAll(opts):
    if opts.skipbipartite:
        filterGraphs = isBipartite
    elif opts.skipnonbipartite:
        filterGraphs = lambda k: not isBipartite(k)
    else:
        filterGraphs = lambda _: False

    runPerGraphLevel = loadRuntimes(opts.paths, filterGraphs=filterGraphs)

    if opts.direct:
        def getPredictor(params):
            result = Result(opts.classifier, opts.model_path, "", params,
                            None, opts.percent, seed=opts.seed,
                            unload=opts.unload, cached=False)
            labels = zip(result.labels, result.feature_importances)
            return Result.genericPredictor(result), labels

    else:
        def getPredictor(params):
            results = dict()
            for impl in Properties.implementations:
                results[impl] = Result(opts.classifier, opts.model_path, impl,
                                    params, None, opts.percent,
                                    seed=opts.seed, unload=opts.unload,
                                    cached=False)
                labels = zip(result[impl].labels,
                             result[impl].feature_importances)
            return Result.genericPredictor(results), labels

    def work(params, getProps):
        predictor, labels = getPredictor(params)
        with opts.task(params, labels, opts) as task:
            runPerGraphLevel(task, getProps, predictor)

    loopProps(opts.paths, opts.properties, work)
    opts.task.finish(opts)

def runTaskForSome(opts):
    if opts.skipbipartite:
        filterGraphs = isBipartite
    elif opts.skipnonbipartite:
        filterGraphs = lambda k: not isBipartite(k)
    else:
        filterGraphs = lambda _: False

    runPerGraphLevel = loadRuntimes(opts.paths, filterGraphs=filterGraphs)
    properties.loadPaths(opts.paths)

    predictor, labels = Result.predictorFromFile(opts.model_path, opts.classifier)
    getProps = properties.getterFromParams(predictor.params)

    with opts.task(predictor.params, labels, opts) as task:
        runPerGraphLevel(task, getProps, predictor)

    opts.task.finish(opts)

class Validate(object):
    ranking = []
    def __init__(self, params, labels, opts):
        if opts.direct:
            length = 1
        else:
            length = len(list(Properties.implementations))

        self.params = params
        self.labels = labels
        self.direct = opts.direct
        self.relErrors = length * [0]
        self.totalCorrect = 0
        self.firstCorrect = 0
        self.errors = 0
        self.unknown = 0
        self.total = 0

    def __enter__(self):
        def runTask(graph, pred, real, last):
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
    def __init__(self, params, labels, opts):
        self.params = params
        self.labels = labels
        self.verbose = opts.verbose

    def __enter__(self):
        self.results = defaultdict(lambda: defaultdict(int))
        def runTask(graph, pred, real, last):
            for impl, time in real.items():
                self.results[graph][impl] += time

            self.results[graph]['optimal'] += min(real.values())

            if pred[0] == 'unknown':
                if last == 'unknown':
                    self.results[graph]['predicted'] = float("NaN")
                else:
                    self.results[graph]['predicted'] += real[last]
                return last
            else:
                self.results[graph]['predicted'] += real[pred[0]]

            return pred[0]

        return runTask

    def __exit__(self, type, value, traceback):
        if type is not None:
            print "type:", type
            print "value:", value
            print "traceback:"
            print_tb(traceback)
            exit(1)

        relError = 0
        relImprovement = 0
        count = 0
        successfulPredictionCount = 0
        predictionFailures = 0
        averagePredicted = 0
        averageBest = 0

        predictions = dict()

        for graph, vals in self.results.items():
            optimal = vals['optimal']
            predicted = vals['predicted']
            del vals['optimal']
            del vals['predicted']

            if not isnan(predicted):
                relError += abs(optimal - predicted) / optimal
            else:
                predictionFailures += 1

            count += 1
            times = sorted(vals.items(), key=lambda k: k[1])

            if self.verbose:
                predictions[graph] = (optimal, predicted, times)

            if not isnan(predicted):
                averagePredicted += predicted/optimal
                averageBest += times[0][1]/optimal
                successfulPredictionCount += 1

        if successfulPredictionCount != 0:
            data = (self.params, self.labels, predictionFailures/count,
                    relError/successfulPredictionCount,
                    averagePredicted/successfulPredictionCount,
                    averageBest/successfulPredictionCount, predictions)
        else:
            data = (self.params, self.labels, predictionFailures/count,
                    float("inf"),
                    float("inf"),
                    float("inf"), predictions)
        self.ranking.append(data)

    @staticmethod
    def finish(opts):
        order = 3
        if opts.sorting == 'pred-fail':
            order = 1
        elif opts.sorting == 'avg-pred':
            order = 3

        def graphToOrder(tup):
            optimal, predicted, times = tup
            if opts.verbose_rel_error:
                return predicted / times.values()[0]
            return predicted/optimal

        final = sorted(Predict.ranking, key=lambda k: k[order])
        for params, labels, predFail, relErr, avgPred, avgBest, preds in final:
            print "Params:", params
            print "Labels:", sorted(labels, key=lambda k: k[1], reverse=True)
            print "Prediction failures:", predFail
            print "Relative Error:", relErr
            print "Average prediction:", avgPred
            print "Average best:", avgBest

            preds = sorted(preds.items(), key=lambda k: graphToOrder(k[1]))
            for (graphRoot, (optimal, predicted, times)) in reversed(preds):
                graph, root = graphRoot.split(':')
                vertexCount = properties.graphProps[graph,'abs','vertex-count']
                lvls = len(list(properties.visited[graph]))
                visited = totalVisited[graphRoot]
                info = "levels:" + str(lvls) + ", visited:" + str(visited)
                info += ", optimal: " + str(optimal) + ", best: "
                info += str(times[0][1]) + ", predicted: "
                info += str(predicted)
                print "Graph:", graphRoot, "(" + info + ")"
                print "\tPredicted:", predicted/optimal, "(" + str(predicted) +")"

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
generate.add_argument('--skip-bipartite', action='store_true',
                      dest='skipbipartite',
                      help='Skip bipartite graphs.')
generate.add_argument('--skip-non-bipartite', action='store_true',
                      dest='skipnonbipartite',
                      help='Skip nonbipartite graphs.')
generate.add_argument('--skip-long', action='store_true',
                      dest='skiplong',
                      help='Skip graphs with BFS depths greater than 15.')
generate.set_defaults(func=genModels)

exportAll = subparsers.add_parser('export-all', parents=[model_props])
exportAll.add_argument('--keep', action='store_true',
                       help='Don\'t remove source.')
exportAll.set_defaults(func=exportAllModels)

export = subparsers.add_parser('export')
export.add_argument('paths', nargs='+', metavar='MODEL-PATH',
                      help="Model filepaths.")
export.add_argument('--classifier', default=DecisionTreeClassifier,
                    action=SetClassifier, choices=SetClassifier.choices,
                    help='Select classifier to use.')
export.add_argument('--keep', action='store_true',
                    help='Don\'t remove source.')
export.set_defaults(func=exportModels)

plotAll = subparsers.add_parser('plot-all', parents=[model_props])
plotAll.set_defaults(func=plotAllModels)

plot = subparsers.add_parser('plot')
plot.add_argument('paths', nargs='+', metavar='MODEL-PATH',
                   help="Model filepaths.")
plot.add_argument('--classifier', default=DecisionTreeClassifier,
                  action=SetClassifier, choices=SetClassifier.choices,
                  help='Select classifier to use.')
plot.set_defaults(func=plotModels)

validate = argparse.ArgumentParser(add_help=False)
validate.set_defaults(sorting='')
validate.add_argument('--overall', action='store_const', const='overall',
                      dest='sorting',
                      help='Total ranking instead of first correct.')
validate.add_argument('--rel-error', '--relative-error', action='store_const',
                      const='rel-error', dest='sorting',
                      help='Sort by relative error.')
validate.add_argument('--unknown-predictions', '--unknown-pred',
                      action='store_const', const='unknown-pred',
                      dest='sorting', help='Sort by unknown predictions.')
validate.add_argument('--skip-bipartite', action='store_true',
                     dest='skipbipartite',
                     help='Skip bipartite graphs.')
validate.add_argument('--skip-non-bipartite', action='store_true',
                      dest='skipnonbipartite',
                      help='Skip nonbipartite graphs.')
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
predict.add_argument('--pred-fail', '--pred-failures', '--prediction-fail',
                     '--prediction-failures', action='store_const',
                     const='pred-fail', dest='sorting',
                     help='Sort by relative error.')
predict.add_argument('--verbose', '-v', action='store_true',
                     help='Print output per graph.')
predict.add_argument('--skip-bipartite', action='store_true',
                     dest='skipbipartite',
                     help='Skip bipartite graphs.')
predict.add_argument('--skip-non-bipartite', action='store_true',
                     dest='skipnonbipartite',
                     help='Skip nonbipartite graphs.')
predict.add_argument('--verbose-relative-error', '--verbose-rel-error',
                     '--verbose-relative-err', '--verbose-rel-err',
                     action='store_true', dest='verbose_rel_error',
                     help='Sort by best average prediction')
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

dumpProps = subparsers.add_parser('dump-properties', parents=[model_props])
dumpProps.set_defaults(func=dumpProperties)

options = parser.parse_args()
options.func(options)
