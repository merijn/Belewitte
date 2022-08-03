#!/usr/bin/env python
# -*- coding: utf-8 -*-

from sys import argv, exit, stdin, stderr

import argparse
from math import isinf
from os import fdopen

import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

if __name__ != "__main__":
    exit(1)

parser = argparse.ArgumentParser(
    description='Generate a heatmap from performance data.',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument(
    metavar='FD',
    type=int,
    action='store',
    dest='dataFd',
    help='File descriptor to read result values from.')

parser.add_argument(
    metavar='GRAPHS',
    type=int,
    action='store',
    dest='rowCount',
    help='Number of graphs to plot.')

parser.add_argument(
    metavar='IMPLS',
    type=int,
    action='store',
    dest='numImpls',
    help='Number of implementations.')

parser.add_argument(
    metavar='OUT-FILE',
    action='store',
    dest='outputPDF',
    help='Output PDF filename.')

opts = parser.parse_args()

sns.set()

columns = [line.strip() for line in stdin.readlines()]
with fdopen(opts.dataFd, 'rb') as dataFile:
    elemCount = opts.rowCount * opts.numImpls
    rawData = dataFile.read(elemCount * np.float64().itemsize)
    data = np.frombuffer(rawData, dtype=np.float64, count=elemCount)
    data = data.reshape(opts.rowCount, opts.numImpls)

ax = sns.heatmap(data, vmin=1.0, vmax=50, norm=LogNorm(), xticklabels=columns)

fig = ax.get_figure()
fig.set_size_inches(30, 10)
fig.savefig(opts.outputPDF + '.pdf', bbox_inches='tight', metadata={'CreationDate': None})
