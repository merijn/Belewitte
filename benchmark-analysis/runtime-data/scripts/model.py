#!/usr/bin/env python

from sys import stdin, stdout, stderr

import argparse
import numpy as np
from collections import defaultdict
from os import fdopen
from scipy.sparse import csr_matrix
import struct
import threading
import warnings

with warnings.catch_warnings():
    warnings.filterwarnings("ignore",category=DeprecationWarning)
    from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor, _tree
    from sklearn.preprocessing import OneHotEncoder, LabelEncoder

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

lble = LabelEncoder()
ohe = OneHotEncoder()

class ResultReader(threading.Thread):
    def __init__(self, fd):
        super(ResultReader,self).__init__()
        self.fd = fd

    def run(self):
        with fdopen(self.fd, 'rb') as inputFile:
            self.outputs = np.fromfile(inputFile, dtype=np.int64, count=opts.numEntries)
            self.outputs = lble.fit_transform(self.outputs).reshape(-1, 1) #normalise indices
            self.outputs = ohe.fit_transform(self.outputs).toarray() # one hot encoding
            self.decode_table = lble.inverse_transform(ohe.active_features_)

thread = ResultReader(opts.resultsFd)
thread.start()

inputs = np.fromfile(stdin, dtype=np.float64, count=opts.numProps * opts.numEntries)
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

unknown = defaultdict(lambda: 0)
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
            pred = thread.decode_table[pred[0]]
        else:
            key = tuple(thread.decode_table[k] for k in np.flatnonzero(proba[:,1]))
            unknown[key] += 1
            pred = key[0]
        translation[node] = count
        count += 1
        arrayTree.append((threshold, featureIdx, -1, pred))
    else:
        translation[node] = count
        count += 1
        arrayTree.append((threshold, featureIdx, left_child, right_child))

        queue.append(left_child)
        queue.append(right_child)

total = sum(unknown.values())

with fdopen(opts.unknownsFd, 'wb') as unknownsFile:
    print >>unknownsFile, total
    for key, val in sorted(unknown.items(), key=lambda x: x[1], reverse=True):
        print >>unknownsFile, val, ":", key

def translate(data):
    if data[2] != -1:
        data = data[:2] + (translation[data[2]], translation[data[3]])
    return data + (0,)

for val in predictor.feature_importances_:
    stdout.write(struct.pack("d", val))

for vals in map(translate, arrayTree):
    stdout.write(struct.pack("diiii", *vals))
