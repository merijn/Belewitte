#!/usr/bin/env python
# -*- coding: utf-8 -*-

from sys import stdin, stdout, stderr

import argparse
import numpy as np
from collections import defaultdict
from os import fdopen
from scipy.sparse import csr_matrix
import struct
import threading
import warnings

from sklearn.tree import DecisionTreeClassifier, _tree
from sklearn.preprocessing import OneHotEncoder

if __name__ != "__main__":
    exit(1)

parser = argparse.ArgumentParser(
    description='Generate a binary decision tree from input training data.',
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('--properties', metavar='N', type=int, action='store',
                    dest='numProps', help='Number of properties to train on.')
parser.add_argument('--entries', metavar='N', type=int, action='store',
                    dest='numEntries', help='Number of datapoints.')
parser.add_argument('--results-fd', metavar='FD', type=int, action='store',
    dest='resultsFd', help='File descriptor to read result values from.')

parser.add_argument('--unknowns-fd', metavar='FD', type=int, action='store',
    dest='unknownsFd', help='File descriptor to read result values from.')

opts = parser.parse_args()

ohe = OneHotEncoder(categories='auto')

class ResultReader(threading.Thread):
    def __init__(self, fd):
        super(ResultReader,self).__init__()
        self.fd = fd

    def run(self):
        with fdopen(self.fd, 'rb') as inputFile:
            data = inputFile.read(opts.numEntries * np.int64().itemsize)
            self.outputs = np.frombuffer(data, dtype=np.int64, count=opts.numEntries)
            # one hot encoding
            self.outputs = ohe.fit_transform(self.outputs.reshape(-1,1)).toarray()

thread = ResultReader(opts.resultsFd)
thread.daemon = True
thread.start()

propCount = opts.numProps * opts.numEntries
data = stdin.buffer.read(propCount * np.float64().itemsize)
inputs = np.frombuffer(data, dtype=np.float64, count=propCount)
inputs = inputs.reshape(opts.numEntries, opts.numProps)

thread.join()

predictor = DecisionTreeClassifier()
predictor = predictor.fit(inputs, thread.outputs)

del inputs
del thread.outputs

tree = predictor.tree_
count = 0
arrayTree = []
translation = { -1 : -1 }
queue = [0]

# Start from 2 to distinguish from older unknown predictions
uniqueSetId = 1
def newCountAndId():
    global uniqueSetId
    uniqueSetId += 1
    return { 'id' : uniqueSetId, 'count' : 0 }

def lookupImpl(n):
    arr = predictor.n_outputs_ * [0]
    arr[n] = 1
    arr = np.array(arr).reshape(1, -1)
    return ohe.inverse_transform(arr)[0][0]

unknown = defaultdict(newCountAndId)
while queue:
    node = queue.pop(0)
    left_child = tree.children_left[node]
    right_child = tree.children_right[node]

    featureIdx = tree.feature[node]
    threshold = tree.threshold[node]

    if left_child == _tree.TREE_LEAF:
        proba = tree.value[node] / tree.weighted_n_node_samples[node]
        pred = np.argmax(proba, axis=1).nonzero()[0]

        if len(pred) == 1:
            pred = lookupImpl(pred[0])
        else:
            key = tuple(lookupImpl(k) for k in np.flatnonzero(proba[:,1]))
            unknown[key]['count'] += 1
            pred = -unknown[key]['id']
        translation[node] = count
        count += 1
        arrayTree.append((-1, -1, -1, pred))
    else:
        translation[node] = count
        count += 1
        arrayTree.append((threshold, featureIdx, left_child, right_child))

        queue.append(left_child)
        queue.append(right_child)

total = sum(pred['count'] for pred in unknown.values())

with fdopen(opts.unknownsFd, 'w') as unknownsFile:
    print(total, file=unknownsFile)
    print(len(unknown), file=unknownsFile)
    for key, val in sorted(unknown.items(), key=lambda x: x[1]['count'], reverse=True):
        print(val['id'], ":", key, ":", val['count'], file=unknownsFile)

def translate(data):
    if data[2] != -1:
        data = data[:2] + (translation[data[2]], translation[data[3]])
    return data + (0,)

for val in predictor.feature_importances_:
    stdout.buffer.write(struct.pack("d", val))

for vals in map(translate, arrayTree):
    stdout.buffer.write(struct.pack("diiii", *vals))
