#!/usr/bin/env python
# -*- coding: utf-8 -*-

from sys import argv, exit, stdin, stderr

import locale

from collections import OrderedDict
from fractions import Fraction
from math import isinf, inf
import itertools

import numpy as np
from colorsys import hsv_to_rgb
import matplotlib as mpl
from matplotlib.ticker import ScalarFormatter, FuncFormatter
mpl.use('pdf')
import matplotlib.pyplot as plt

locale.setlocale(locale.LC_NUMERIC, "")

params = {'legend.fontsize': 20 }
plt.rcParams.update(params)

def isBool(val):
    if val == "True":
        return True
    elif val == "False":
        return False
    else:
        return val

def labelise(val):
    try:
        return int(val)
    except:
        return val

def superscriptNumber(val):
    mapping = {
        '0' : u'⁰', '1' : u'¹', '2' : u'²', '3' : u'³', '4' : u'⁴',
        '5' : u'⁵', '6' : u'⁶', '7' : u'⁷', '8' : u'⁸', '9' : u'⁹'
    }

    return ''.join(mapping[c] for c in str(val))

def prettyFormat(val, pos):
    order = len(str(int(val))) - 1
    divider = pow(10, order)
    return "{0:0.1f}".format(val/divider) + u'×10' + superscriptNumber(order)

def colour_gen():
    def fractions(value):
        for v in [Fraction(8,10), Fraction(5,10)]:
            yield tuple(float(f) for f in hsv_to_rgb(value, Fraction(6,10), v))

    for c in fractions(0):
        yield c

    for i in (2**k for k in itertools.count()):
        for j in range(1,i,2):
            for c in fractions(Fraction(j,i)):
                yield c

class Plot(object):
    def __init__(self, filename, slideFormat):
        self.filename = filename
        if slideFormat:
            self.figsize = (20, 12)
        else:
            self.figsize = (16, 5)
        self.handles = []
        self.labels = []

    def __enter__(self):
        self.fig, self.ax = plt.subplots(figsize=self.figsize, dpi=300)
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
                loc='lower center', bbox_to_anchor=(-0.05,1.02,1.1,1),
                markerscale=2, numpoints=1, scatterpoints=1, edgecolor="black",
                mode="expand", ncol=3)
        self.fig.savefig(self.filename, bbox_extra_artists=(legend,),
                         bbox_inches='tight', metadata={'CreationDate': None})
        plt.close(self.fig)
        return None

def plotBars(ax, xAxisName, columnNames, groups, normalised, rotate, numberedGroups):
    groups = list(sorted(groups, key=lambda k: k[0]))
    numGroups = len(groups)

    columns = list(enumerate(columnNames))
    numBars = len(columns) + 1

    ind = np.arange(0, numBars * numGroups, numBars)

    hatching = itertools.cycle(['/','\\','o','*','-','+','|','O','.','x'])
    colours = ["#ac9c3d", "#7f63b8", "#56ae6c", "#b84c7d", "#ba543d"]
    if len(columns) > len(colours):
        colours = colour_gen()
    decoratedColumns = zip(columns, colours, hatching)
    for i, ((colIdx, column), colour, hatch) in enumerate(decoratedColumns):
        values = [group[1][colIdx] for group in groups]

        if numberedGroups:
            label = str(i)
        else:
            label = column

        ax.bar(ind + i, values, 1, hatch=hatch, edgecolor="black",
               color=colour, label=column)

    fontsize=25
    labelsize=20

    ax.tick_params(axis='both', which='major', labelsize=labelsize,
                   right=True, labelright=True)

    ySettings = {'ymin' : 0}

    if normalised:
        ySettings['ymax'] = 1
        yTicks = [0, 0.2, 0.4, 0.6, 0.8, 1]
        yLabels = ["0%", "20%", "40%", "60%", "80%", "100%"]
        ax.set_yticks(yTicks)
        ax.set_yticklabels(yLabels)
        ax.set_ylabel("Normalised Runtime", fontsize=fontsize)

    else:
        maxWithoutInf = lambda l: max(filter(lambda d: not isinf(d), l))
        maxVal = max([maxWithoutInf(grp[1]) for grp in groups])

        ax.set_ylabel("Runtime (ns)", fontsize=fontsize)
        # FIXME: better logic for yticks
        #yTicks = np.arange(1e6, maxVal, 2e6)
        #ax.set_yticks(yTicks)

        formatter = FuncFormatter(prettyFormat)
        ax.yaxis.set_major_formatter(formatter)

    ax.set_xlabel(xAxisName, fontsize=fontsize)
    ax.set_xticks(ind + (numBars // 3))

    if rotate:
        rotation = -45
        ha = 'left'
    else:
        rotation = 0
        ha = 'center'

    if numberedGroups:
        groupNames = [i for i, group in enumerate(groups, 1)]
    else:
        groupNames = [group[0] for i, group in enumerate(groups, 1)]

    ax.set_xticklabels(groupNames, rotation=rotation, ha=ha, va='top')

    ax.set_ylim(**ySettings)

if __name__ != "__main__":
    exit(1)

if len(argv) < 7:
    print("Not enough arguments!", file=stderr)
    exit(1)
elif len(argv) > 7:
    print("Too many arguments!", file=stderr)
    exit(1)

_, outputPDF, xAxisName, normalise, slideFormat, rotate, numberedGroups = map(isBool, argv)

def convert_values(nColumns, values):
    padColumns = [inf] * max(0, nColumns - len(values))
    return [float(v) for v in values] + padColumns

with Plot(outputPDF, slideFormat) as ax:
    lines = stdin.readlines()
    columns = lines[0].strip().split(':')
    numColumns = len(columns)
    lines = [[s.strip() for s in line.split(':')] for line in lines[1:]]
    groups = [(labelise(k), convert_values(numColumns, vals.split())) for k, vals in lines]
    plotBars(ax, xAxisName, columns, groups, normalise, rotate, numberedGroups)
