#include "bfs.hpp"

template<typename BFSVariant>
__global__ void
revEdgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->edge_count;
    BFSVariant bfs;
    int newDepth = depth + 1;

    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
         idx < size && levels[graph->outEdges[idx]] == depth;
         idx += blockDim.x * gridDim.x)
    {
        if (atomicMin(&levels[graph->inEdges[idx]], newDepth) > newDepth) {
            bfs.update();
        }
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
revEdgeListBfs<Reduction<normal>>(EdgeList<unsigned> *, int *, int);

template __global__ void
revEdgeListBfs<Reduction<bulk>>(EdgeList<unsigned> *, int *, int);

template __global__ void
revEdgeListBfs<Reduction<warpreduce>>(EdgeList<unsigned> *, int *, int);

template __global__ void
revEdgeListBfs<Reduction<blockreduce>>(EdgeList<unsigned> *, int *, int);
#endif
