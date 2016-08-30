#!/usr/bin/env python

import numbers

from collections import defaultdict, OrderedDict
from itertools import product

class TableError(Exception):
    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return "Table manipulation error: " + str(self.msg)

def detranspose(transposition, vals):
    return tuple(t[1] for t in sorted(zip(transposition, vals)))

class View:
    def __init__(self, table, idx, transpose=None, prefix=None, transform=lambda x: x, **filters):
        self.view = transform
        self.table = table
        self.idx = idx
        self.transposition = transpose if transpose else ()
        self.keyPrefix = prefix if prefix else ()
        self.filters = defaultdict(set)

        if len(self.transposition) < len(self.keyPrefix):
            raise TableError("Transposition shorter than prefix.")

        for dim in table.mapping:
            if dim in filters:
                self.filters[dim] = filters[dim]

        for dim, key in zip(table.mapping, idx):
            self.filters[dim] = key

        for pos, key in zip(self.transposition, self.keyPrefix):
            self.filters[self.table.mapping.keys()[pos + len(self.idx)]] = key

    def __iter__(self):
        return self.keys(dim=self.dims()[0])

    def __setitem__(self, key, value):
        if not isinstance(key, tuple):
            key = (key,)

        if self.transposition:
            key = self.keyPrefix + key

            if len(key) == len(self.transposition):
                key = self.detranspose(key)
            else:
                raise TableError("Incorrect index length.")

        idx = self.idx + key

        if len(idx) != len(self.table.dims()):
            raise TableError("Incorrect index length.")

        self.table[idx] = value

    def __getitem__(self, key):
        if not isinstance(key, tuple):
            key = (key,)

        if self.transposition:
            key = self.keyPrefix + key

            if len(key) == len(self.transposition):
                key = self.detranspose(key)
            else:
                return View(self.table, self.idx, transpose=self.transposition,
                            prefix=key, transform=self.view,
                            **self.filters)

        idx = self.idx + key

        if len(idx) > len(self.table.dims()):
            raise TableError("Index too long.")

        return self.view(self.table[idx])

    def detranspose(self, vals):
        return detranspose(self.transposition, vals)

    def dims(self):
        dims = self.table.mapping.keys()[len(self.idx):]
        if self.transposition:
            dims = [dims[i] for i in self.transposition]
            dims = dims[len(self.keyPrefix):]

        return dims

    def keys(self, dim=None):
        def gen(dimFilter, keys):
            for name, deps in keys.items():
                if name in dimFilter:
                    continue

                for dim in deps:
                    if isinstance(self.filters[dim], set):
                        if not (deps[dim] - self.filters[dim]):
                            break
                    else:
                        if self.filters[dim] not in deps[dim]:
                            break
                else:
                    yield name

        def gengen():
            for key in self.dims():
                yield gen(self.filters[key], self.table.mapping[key])

        if dim:
            return gen(self.filters[dim], self.table.mapping[dim])
        else:
            return product(*gengen())

    def values(self):
        return (self[k] for k in self.keys())

    def items(self):
        return ((k, self[k]) for k in self.keys())

    def sparsekeys(self):
        for key in self.keys():
            tmpKey = key
            if self.transposition:
                tmpKey = self.keyPrefix + tmpKey
                tmpKey = self.detranspose(tmpKey)

            if self.idx + tmpKey in self.table:
                yield key

    def sparsevalues(self):
        return (self[key] for key in self.sparsekeys())

    def sparseitems(self):
        return ((key, self[key]) for key in self.sparsekeys())

    def map(self, fn):
        for k in self.sparsekeys():
            self[k] = fn(self[k])

    def transform(self, fn):
        return View(self.table, self.idx, transpose=self.transposition,
                    prefix=self.keyPrefix, transform=fn, **self.filters)

    def addDimension(self, dim, defaultKey):
        self.filters[dim] = set()
        self.table.addDimension(dim, defaultKey)

    def transposeDims(self, *pos):
        posSet = set(pos)
        prefixLen = len(self.idx) + len(self.keyPrefix)

        if len(pos) > len(self.table.mapping) - prefixLen:
            raise TableError("Transposition too long.")
        elif posSet != set(range(0, len(pos))) or len(pos) != len(posSet):
            raise TableError("Invalid transposition: " + str(pos))

        if self.transposition:
            start = len(self.keyPrefix)
            end = start + len(pos)
            mid = tuple(self.transposition[start:end][i] for i in pos)
            pos = self.transposition[:start] + mid + self.transposition[end:]

        pos += tuple(xrange(len(pos), len(self.table.mapping) - len(self.idx)))

        return View(self.table, self.idx, transpose=pos, prefix=self.keyPrefix,
                    transform=self.view, **self.filters)

    def collapseDim(self, dim, defaultKey):
        idx = self.dims().index(dim)
        pos = [idx] + range(0, idx)
        return self.transposeDims(*pos)[defaultKey]

    def filterKeys(self, fn, dim=None):
        newFilters = defaultdict(set, **self.filters)
        for k, v in self.table.mapping.iteritems():
            if dim is None or k == dim:
                newFilters[k] |= set(x for x in v if fn(x))

        return View(self.table, self.idx, transpose=self.transposition,
                    prefix=self.keyPrefix, transform=self.view, **newFilters)

class SkipZeros(defaultdict):
    def __iter__(self):
        return self.keys()

    def __nonzero__(self):
        for k in self.items():
            return True
        return False

    def items(self):
        return ((k,v) for k, v in super(SkipZeros,self).items() if v)

    def keys(self):
        return (k for k, _ in self.items())

    def values(self):
        return (v for _, v in self.items())

class Table(dict):
    def __init__(self, factory, *dimNames):
        super(Table, self).__init__()
        self.default = factory
        self.mapping = OrderedDict()
        for n in dimNames:
            self.mapping[n] = SkipZeros(lambda: SkipZeros(set))

    def __iter__(self):
        return iter(View(self, ()))

    def __getitem__(self, key):
        if not isinstance(key, tuple):
            key = (key,)

        if len(key) == len(self.mapping):
            if key in self:
                return super(Table, self).__getitem__(key)
            else:
                return self.default()
        elif len(key) < len(self.mapping):
            return View(self, key)
        else:
            raise TableError("Index too long.")

    def __delitem__(self, key):
        raise NotImplementedError

    def __setitem__(self, key, value):
        if not isinstance(key, tuple):
            key = (key,)

        if len(key) != len(self.mapping):
            raise TableError("Incorrect key length.")

        pairs = set(zip(self.mapping, key))
        for dim, name in pairs:
            for d,x in pairs.difference(set([(dim,name)])):
                self.mapping[dim][name][d].add(x)

        super(Table, self).__setitem__(key, value)

    def addDimension(self, dim, defaultKey):
        self.mapping[dim] = SkipZeros(lambda: SkipZeros(set))
        items = super(Table,self).items()
        self.clear()

        for counts in self.mapping.values():
            counts.clear()

        for key,v in items:
            self[key + (defaultKey,)] = v

    def dims(self):
        return self.mapping.keys()

    def keys(self, dim=None):
        return View(self, ()).keys(dim=dim)

    def values(self):
        return (self[k] for k in self.keys())

    def items(self):
        return ((k, self[k]) for k in self.keys())

    def sparsekeys(self):
        return View(self, ()).sparsekeys()

    def sparsevalues(self):
        return View(self, ()).sparsevalues()

    def sparseitems(self):
        return View(self, ()).sparseitems()

    def map(self, fn):
        View(self, ()).map(fn)

    def transform(self, fn):
        return View(self, ()).transform(fn)

    def transposeDims(self, *pos):
        return View(self, ()).transposeDims(*pos)

    def collapseDim(self, dim, defaultKey):
        return View(self, ()).collapseDim(dim, defaultKey)

    def filterKeys(self, fn, dim=None):
        return View(self, ()).filterKeys(fn, dim=dim)
