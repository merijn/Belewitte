#!/usr/bin/env python

import locale

from collections import defaultdict, OrderedDict
from copy import copy
from fractions import Fraction
from glob import glob
import itertools
from math import sqrt
from os.path import basename
from sys import exit

import numpy as np
from colorsys import hsv_to_rgb
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
import matplotlib.markers as marks

from names import Naming, names
from measurements import Measurement
from table import Table

def isGenerated(name):
    return (name.startswith("chain") or name.startswith("star")
         or name.startswith("degree") or name.startswith("mesh"))

def isWarp(name):
    return "warp" in name

locale.setlocale(locale.LC_NUMERIC, "")

params = {'legend.fontsize': 20 }
plt.rcParams.update(params)

def colours():
    def fractions(value):
        for v in [Fraction(8,10), Fraction(5,10)]:
            yield tuple(float(f) for f in hsv_to_rgb(value, Fraction(6,10), v))

    for c in fractions(0):
        yield c

    for i in (2**k for k in itertools.count()):
        for j in xrange(1,i,2):
            for c in fractions(Fraction(j,i)):
                yield c

class Plot(object):
    def __init__(self, filename):
        self.filename = filename
        self.handles = []
        self.labels = []

    def __enter__(self):
        self.fig, self.ax = plt.subplots(figsize=(16, 5), dpi=300)
        def register(handle, label):
            self.handles.append(handle)
            self.labels.append(label)
        self.ax.register_label = register
        return self.ax

    def __exit__(self, exc_type, exc_value, tb):
        hs, ls = self.ax.get_legend_handles_labels()

        legend = self.ax.legend(hs + self.handles, ls + self.labels,
                loc='upper left', bbox_to_anchor=(1,1), markerscale=2,
                numpoints=1, scatterpoints=1)
        self.fig.savefig(self.filename + '.pdf', bbox_extra_artists=(legend,),
                         bbox_inches='tight')
        plt.close(self.fig)
        return None

def plotBars(ax, normalise, data, groupNames=Naming(), columnNames=Naming()):
    dims = data.dims()

    groups = sorted(data.keys(dim=dims[0]))
    numGroups = len(groups)

    columns = sorted(data.keys(dim=dims[1]))
    numBars = len(columns) + 1

    fun = lambda m: m.avg
    if normalise:
        for k in data:
            maxVal = max(data[k].values())
            data[k].map(lambda m: m.normalise(maxVal))

        fun = lambda m: m.normalised

    data = data.transform(fun)
    ind = np.arange(0, numBars * numGroups, numBars)

    for i, (column, colour) in enumerate(zip(columns, colours())):
        values = [data[group][column] for group in groups]

        bar = ax.bar(ind + i, values, 1, color=colour)
        ax.register_label(bar, columnNames[column])

    fontsize=25
    if normalise:
        ax.set_ylabel('Normalised runtime', fontsize=fontsize)
    else:
        ax.set_ylabel('Runtime (ns)', fontsize=fontsize)

    ax.set_xticks(ind + (numBars / 3))
    ax.set_xticklabels([groupNames[n] for n in groups], fontsize=fontsize,
            rotation=-35, ha='left', va='top')
    ax.set_yticklabels(ax.get_yticklabels(), fontsize=fontsize)

    ySettings = {'ymin' : 0}
    if normalise:
        ySettings['ymax'] = 1

    ax.set_ylim(**ySettings)
    ax.set_xlim(xmin = 0, xmax = numBars * numGroups)

def plotPoints(ax, data, marks=('.',), dotNames=Naming()):
    colouredMarks = ((c,m) for c in colours() for m in marks)
    for k, (colour, mark) in zip(sorted(data), colouredMarks):
        ax.scatter(*zip(*data[k]), marker=mark, s=50, color=colour, label=k)

def plotLines(ax, data, lineNames=Naming()):
    for k, c in zip(sorted(data), colours()):
        if isinstance(data[k], LineCollection):
            data[k].set(label=k, linewidth=2, color=c)
            ax.add_collection(data[k])
        else:
            ax.plot(*zip(*data[k]), label=k, linewidth=2, color=colour)

def plotDataSet(dims, group, column, measurements, normalise):
    def plotHelper(data, order, fileName=''):
        if len(order) == 2:
            groupNames = names[order[0][0]]
            columnNames = names[order[1][0]]
            with Plot(fileName) as ax:
                plotBars(ax, normalise, data, groupNames, columnNames)
        elif order[0][1] == '':
            for k in data:
                if fileName:
                    newFile = fileName + '.' + k
                else:
                    newFile = k
                plotHelper(data[k], order[1:], fileName=newFile)
        else:
            data = data.collapseDim(order[0][0], order[0][1])
            plotHelper(data, order[1:], fileName=fileName)

    transpose = []
    defaults = []
    for i, (k, v) in enumerate(dims.items()):
        if k == group:
            group = (i, k)
        elif k == column:
            column = (i, k)
        elif v == '':
            transpose.append((i, k))
        else:
            defaults.append((i,k))

    transpose += defaults + [group, column]
    measurements = measurements.transposeDims(*[i for i, _ in transpose])

    plotHelper(measurements, [(k,dims[k]) for _, k in transpose])

def parseFiles(path, table, ext=".timings", process_line=None):
    if not path.endswith('/'):
        path += '/'

    if not ext.startswith('.'):
        ext = "." + ext

    def default_process_line(line, ns):
        ns["timer"], timings = line.strip().split(':')
        return (Measurement(timings), ns)

    namespace = dict()
    basedims = ["algorithm", "implementation", "device"]
    for input_file in glob(path + "*" + ext):
        split = basename(input_file)[:-len(ext)].split('.')
        if len(split) == 3:
            for k, v in zip(basedims, split):
                namespace[k] = v

            if process_line is None:
                def process_line(line, ns):
                    ns["graph"], ns["timer"], timings = line.strip().split(':')
                    split = ns["graph"].split('.')
                    if len(split) == 1:
                        ns["sorting"] = "normal"
                    else:
                        ns["graph"], ns["sorting"] = split
                    return (Measurement(timings), ns)

        elif len(split) == 4:
            for k, v in zip(["graph"] + basedims, split):
                namespace[k] = v

            namespace["sorting"] = "normal"

        elif len(split) == 5:
            for k, v in zip(["graph", "sorting"] + basedims, split):
                namespace[k] = v

        else:
            print "Ach mein leben!"
            print input_file
            exit(1)

        if process_line is None:
            process_line = default_process_line

        with open(input_file) as file:
            for line in file:
                result, ns = process_line(line, copy(namespace))
                table[tuple(ns[k] for k in table.dims())] = result

def plotPerformance(opts):
    data = loadData(Measurement, opts.dims, opts.paths, filters=opts.filters)
    plotDataSet(opts.dims, opts.group, opts.column, data, opts.normalise)

def plotFrontier(opts):
    runtimeData = loadData(Measurement, opts.dims, opts.paths,
            filters=opts.filters)

    dims = [d for d in opts.dims if d != 'timer'] + ['depth']

    frontierData = loadData(int, dims, opts.paths, ext=".levels",
            process_line=process_frontier_line, filters=opts.filters)

    plotData = Table(list, "frontier", "implementation", "depth")
    for k, v in frontierData.sparseitems():
        m = runtimeData[k]
        plotData[v, k[5] + "-" + k[0], int(k[6][len("bfsLevel"):])] += [m]

    stddevData = plotData.transform(lambda x: stdDev(x)[1]).filterKeys(lambda x: x < 100 or x > 1000, dim='frontier')

    prevImpl = None
    segment = []
    lines = defaultdict(list)
    dots = defaultdict(list)
    for frontier in sorted(stddevData):
        (impl, graph), time = min(stddevData[frontier].sparseitems(), key=lambda x: x[1])
        if prevImpl is None:
            prevImpl = impl
        elif prevImpl != impl:
            lines[prevImpl] += [segment + [(frontier, time)]]
            segment = []
            prevImpl = impl

        dots[graph] += [(frontier, time)]
        segment += [(frontier, time)]
    lines[prevImpl] += [segment]

    lines = { k:LineCollection(v) for k, v in lines.items() }
    with Plot("frontier") as ax:
        plotLines(ax, lines)
        ms = ['o', 'v', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', '+', 'x', 'D', 'd', marks.TICKLEFT, marks.TICKRIGHT, marks.TICKUP, marks.TICKDOWN, marks.CARETLEFT, marks.CARETRIGHT, marks.CARETUP, marks.CARETDOWN]
        plotPoints(ax, dots, marks=ms)
        ax.autoscale()
        ax.set_yscale('log')
        ax.set_xlim(100, 1000)

measurementDims = OrderedDict()
measurementDims['paths'] = ''
measurementDims['device'] = ''
measurementDims['algorithm'] = ''
measurementDims['graph'] = ''
measurementDims['sorting'] = 'normal'
measurementDims['implementation'] = ''
measurementDims['timer'] = 'computation'

def loadData(default, dims, paths, ext=".timings", process_line=None, filters=dict()):
    data = Table(default, *dims)
    for path in paths:
        path = path.rstrip('/')
        parseFiles(path, data[path], ext=ext, process_line=process_line)

    for dim in set(filters).intersection(data.dims()):
        data = data.filterKeys(filters[dim], dim=dim)

    return data

def process_frontier_line(line, ns):
    depth, frontier = line.strip().split('\t')
    ns["depth"] = "bfsLevel" + str(depth)
    return (int(frontier.replace(',', '')), ns)

def stdDev(l):
    if isinstance(l[0], Measurement):
        l = map(lambda x: x.avg, l)

    minimum = min(l)
    maximum = max(l)
    total = sum(l)
    avg = total/len(l)
    stddev = 0
    for t in l:
        stddev += (t - avg) ** 2

    if len(l) > 1:
        stddev *= 1.0 / (len(l) - 1)
        stddev = sqrt(stddev)
    else:
        stddev = 0

    return (minimum, avg, maximum, stddev)
