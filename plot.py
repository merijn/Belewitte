#!/usr/bin/env python

import argparse
import locale

from collections import defaultdict, OrderedDict
from glob import glob
from os.path import basename
from sys import argv, exit, exc_info, stdout

import numpy as np
from numpy.random import rand
from colorsys import hls_to_rgb
import matplotlib.pyplot as plt

from names import Naming, names
from measurements import Measurement
from table import Table

def isGenerated(name):
    return (name.startswith("chain") or name.startswith("star")
         or name.startswith("degree") or name.startswith("mesh"))

locale.setlocale(locale.LC_NUMERIC, "")

params = {'legend.fontsize': 20 }
plt.rcParams.update(params)

def plotGraph(fileName, normalise, data, groupNames=Naming(), columnNames=Naming()):
    dims = data.dims()

    groups = sorted(data.keys(dim=dims[0]))
    numGroups = len(groups)

    columns = sorted(data.keys(dim=dims[1]))
    numBars = len(columns) + 1

    colours = []
    for i in np.linspace(0, 1, numBars):
        colours.append(hls_to_rgb(i, 0.25 + rand() * 0.5, 0.25 + rand() * 0.5))

    if normalise:
        for k in data:
            maxVal = max(data[k].values())
            data[k].map(lambda m: m.normalise(maxVal))

        data = data.transform(lambda m: m.normalised)
    else:
        data = data.transform(lambda m: m.avg)

    fig, ax = plt.subplots(figsize=(16, 5), dpi=300)
    ind = np.arange(0, numBars * numGroups, numBars)

    rects = []
    for i, column in enumerate(columns):
        values = [data[group][column] for group in groups]

        rects.append(ax.bar(ind + i, values, 1, color=colours[i]))

    fontsize=25
    if normalise:
        ax.set_ylabel('Normalised runtime', fontsize=fontsize)
    else:
        ax.set_ylabel('Runtime (ns)', fontsize=fontsize)

    ax.set_xticks(ind + (numBars / 3))
    ax.set_xticklabels([groupNames[n] for n in groups], fontsize=fontsize)
    plt.setp(ax.get_xticklabels(), fontsize=fontsize, rotation=-35, ha='left',
             va='top')
    plt.setp(ax.get_yticklabels(), fontsize=fontsize)

    legend = ax.legend((rect[0] for rect in rects),
                       (columnNames[n] for n in columns), loc='upper left',
                       bbox_to_anchor=(1,1))

    ySettings = {'ymin' : 0}
    if normalise:
        ySettings['ymax'] = 1

    plt.ylim(**ySettings)
    plt.xlim(xmin = 0, xmax = numBars * numGroups)

    fig.savefig(fileName + '.pdf', bbox_extra_artists=(legend,),
                bbox_inches='tight')
    plt.close(fig)

def plotDataSet(dims, group, column, measurements, normalise):
    def plotHelper(data, order, fileName=''):
        if len(order) == 2:
            groupNames = names[order[0][0]]
            columnNames = names[order[1][0]]
            plotGraph(fileName, normalise, data, groupNames, columnNames)
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

def parseFiles(path, table):
    if not path.endswith('/'):
        path += '/'

    def process_file(f):
        for line in file:
            timer, timings = line.strip().split(':')
            table[dev,algo,graph,sorting,impl,timer] = Measurement(timings)

    for input_file in glob(path + "*.timings"):
        split = basename(input_file)[:-len(".timings")].split('.')
        if len(split) == 3:
            algo, impl, dev = split
            def process_file(f):
                for line in file:
                    graph, timer, timings = line.strip().split(':')
                    split = graph.split('.')
                    if len(split) == 1:
                        sorting = "normal"
                    else:
                        graph, sorting = split
                    table[dev,algo,graph,sorting,impl,timer] = Measurement(timings)
        elif len(split) == 4:
            graph, algo, impl, dev = split
            sorting = "normal"
        elif len(split) == 5:
            graph, sorting, algo, impl, dev = split
        else:
            print "Ach mein leben!"
            print input_file
            exit(1)

        with open(input_file) as file:
            process_file(file)

def plotPerformance(opts):
    data = Table(Measurement, *options.dims)
    for path in opts.paths:
        parseFiles(path, data[path])

    for dim in opts.filters:
        data = data.filterKeys(opts.filters[dim], dim=dim)

    plotDataSet(opts.dims, opts.group, opts.column, data, opts.normalise)

measurementDims = OrderedDict()
measurementDims['paths'] = ''
measurementDims['device'] = ''
measurementDims['algorithm'] = ''
measurementDims['graph'] = ''
measurementDims['sorting'] = 'normal'
measurementDims['implementation'] = ''
measurementDims['timer'] = 'computation'

class SetDim(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        namespace.dims[self.dest] = values

parser = argparse.ArgumentParser(description='Plot results.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

common_args = argparse.ArgumentParser(add_help=False)
common_args.set_defaults(dims=measurementDims)
common_args.set_defaults(filters=defaultdict(lambda: lambda x: False))
common_args.add_argument('-n', '--normalise', action='store_true',
                         help='Normalise runtimes.')
common_args.add_argument('--filter-generated', action='store_true',
                         help='Filter out generated graphs.')
common_args.add_argument('--filter-real', action='store_true',
                         help='Filter out real graphs.')
common_args.add_argument('--numeric', action='store_true',
                         help='Numeric graph labels.')
common_args.add_argument('paths', metavar='PATH', nargs='+',
                         help="Path(s) to read results from.")

for dim in measurementDims:
    common_args.add_argument('--' + dim, action=SetDim, metavar='VAL',
                             default=measurementDims[dim],
                             help=('Set default value to use for dimension. ' +
                                   'Empty value iterates over all entries.'))

common_args.add_argument('--group', default='graph', metavar='DIM',
                         choices=measurementDims,
                         help='Dimension to use for column grouping.')

subparsers = parser.add_subparsers()
plotPerf = subparsers.add_parser('plot-perf', parents=[common_args])

plotPerf.add_argument('--column', default='implementation', metavar='DIM',
                      choices=measurementDims,
                      help='Dimension to use for column plotting.')
plotPerf.set_defaults(func=plotPerformance)

options = parser.parse_args()
fn = lambda x: x[0]
if options.numeric:
    fn = lambda x: x[1]

if len(options.paths) == 1:
    options.dims['paths'] = options.paths[0]

if options.filter_generated and options.filter_real:
    print "Mutually exclusive options: --filter-generated and --filter-real."
    exit(1)
elif options.filter_generated:
    options.filters['graph'] = isGenerated
elif options.filter_real:
    options.filters['graph'] = lambda x: not isGenerated(x)

names['graph'] = names['graph'].map(fn)
options.func(options)
