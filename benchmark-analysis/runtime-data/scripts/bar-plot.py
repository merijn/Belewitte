#!/usr/bin/env python

from __future__ import division
from sys import stdin

import locale

from collections import OrderedDict
from fractions import Fraction
import itertools
from sys import argv, exit

import numpy as np
from colorsys import hsv_to_rgb
import matplotlib as mpl
mpl.use('pdf')
import matplotlib.pyplot as plt

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
        old_bar = self.ax.bar
        self.ax.extra_axes = OrderedDict()

        def replaceBar(fn):
            def newFn(*args, **kwargs):
                if 'label' in kwargs:
                    self.labels.append(kwargs['label'])
                    del kwargs['label']
                result = fn(*args, **kwargs)
                self.handles.append(result)
                return result
            return newFn

        def replaceTwin(fn):
            def updateTwin(name):
                newAx = fn()
                self.ax.extra_axes[name] = newAx
                newAx.bar = replaceBar(newAx.bar)
                newAx.twinx = replaceTwin(newAx.twinx)
                newAx.twiny = replaceTwin(newAx.twiny)
                return newAx
            return updateTwin

        def replaceAddCollection(fn):
            def addCollection(coll, *args, **kwargs):
                if 'label' in kwargs:
                    self.labels.append(kwargs['label'])
                    del kwargs['label']
                    if 'proxy' in kwargs:
                        self.handles.append(kwargs['proxy'])
                        del kwargs['proxy']
                    else:
                        self.handles.append(handle)
                fn(coll, *args, **kwargs)
            return addCollection

        self.ax.bar = replaceBar(self.ax.bar)
        self.ax.twinx = replaceTwin(self.ax.twinx)
        self.ax.twiny = replaceTwin(self.ax.twiny)
        self.ax.add_collection = replaceAddCollection(self.ax.add_collection)
        return self.ax

    def __exit__(self, exc_type, exc_value, tb):
        step = 1.0
        hs, ls = self.ax.get_legend_handles_labels()
        for name, ax in self.ax.extra_axes.items():
            h, l = ax.get_legend_handles_labels()
            hs += h
            ls += l

            ax.spines['right'].set_position(('axes', step))
            step += 0.1
            ax.set_frame_on(True)
            ax.patch.set_visible(False)

        legend = self.ax.legend(hs + self.handles, ls + self.labels,
                loc='lower center', bbox_to_anchor=(0.5,1), markerscale=2,
                numpoints=1, scatterpoints=1, ncol=5, edgecolor="black")
        self.fig.savefig(self.filename + '.pdf', bbox_extra_artists=(legend,),
                         bbox_inches='tight')
        plt.close(self.fig)
        return None

def plotBars(ax, xAxisName, columnNames, groups, normalised):
    groups = list(sorted(groups, key=lambda k: k[0]))
    numGroups = len(groups)

    columns = list(enumerate(columnNames))
    numBars = len(columns) + 1

    ind = np.arange(0, numBars * numGroups, numBars)

    hatching = itertools.cycle(['/','\\','o','*','-','+','|','O','.','x'])
    colours = ["#ac9c3d", "#7f63b8", "#56ae6c", "#b84c7d", "#ba543d", "#FF0000", "#00FF00"]
    decoratedColumns = zip(columns, colours, hatching)
    for i, ((colIdx, column), colour, hatch) in enumerate(decoratedColumns):
        values = [group[1][colIdx] for group in groups]

        ax.bar(ind + i, values, 1, hatch=hatch, edgecolor="black",
               color=colour, label=column)

    fontsize=25
    if normalised:
        ax.set_ylabel("Normalised Runtime", fontsize=fontsize)
    else:
        ax.set_ylabel("Runtime (ns)", fontsize=fontsize)

    ax.set_xlabel(xAxisName, fontsize=fontsize)
    ax.set_xticks(ind + (numBars // 3))
    ax.set_xticklabels([group[0] for group in groups], fontsize=fontsize,
            rotation=-35, ha='left', va='top')
    ax.set_yticklabels(ax.get_yticklabels(), fontsize=fontsize)

    ySettings = {'ymin' : 0}
    if normalised:
        ySettings['ymax'] = 1

    ax.set_ylim(**ySettings)

if __name__ != "__main__":
    exit(1)

if len(argv) != 4:
    print >>stderr, "Not enough arguments!"
    exit(1)

with Plot(argv[1]) as ax:
    lines = stdin.readlines()
    columns = lines[0].strip().split(':')
    lines = [line.split(':') for line in lines[1:]]
    groups = [(k, map(float, vals.split())) for k, vals in lines]
    plotBars(ax, argv[2], columns, groups, argv[3] == "True")
