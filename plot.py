#!/usr/bin/env python

import argparse
import locale

from sys import exit
from plot_funs import *

if __name__ != "__main__":
    exit(1)

locale.setlocale(locale.LC_NUMERIC, "")

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
common_args.add_argument('--filter-old', action='store_true',
                        help='Filter out old results.')
common_args.add_argument('--filter-warp', action='store_true',
                        help='Filter out warp implementations.')
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

subparsers = parser.add_subparsers()
plotPerf = subparsers.add_parser('plot-perf', parents=[common_args])

plotPerf.add_argument('--group', default='graph', metavar='DIM',
                      choices=measurementDims,
                      help='Dimension to use for column grouping.')

plotPerf.add_argument('--column', default='implementation', metavar='DIM',
                    choices=measurementDims,
                    help='Dimension to use for column plotting.')
plotPerf.set_defaults(func=plotPerformance)

plotFront = subparsers.add_parser('plot-frontier', parents=[common_args])
plotFront.set_defaults(func=plotFrontier)

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

if options.filter_old:
    options.filters['paths'] = lambda x: x.endswith('-old')

if options.filter_warp:
    options.filters['implementation'] = isWarp

names['graph'] = names['graph'].map(fn)
options.func(options)
