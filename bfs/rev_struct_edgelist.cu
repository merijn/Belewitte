#include "bfs.hpp"

template<typename BFSVariant>
__global__ void
revStructEdgeListBfs(StructEdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t startIdx = blockIdx.x * blockDim.x + threadIdx.x;
    uint64_t size = graph->edge_count;
    BFSVariant bfs;
    int newDepth = depth + 1;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x)
    {
        edge<unsigned> myEdge = graph->edges[idx];
        if (levels[myEdge.in] == depth) {
            if (atomicMin(&levels[myEdge.out], newDepth) > newDepth) {
                bfs.update();
            }
        }
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
revStructEdgeListBfs<Reduction<normal>>
(StructEdgeList<unsigned> *, int *, int);

template __global__ void
revStructEdgeListBfs<Reduction<bulk>>
(StructEdgeList<unsigned> *, int *, int);

template __global__ void
revStructEdgeListBfs<Reduction<warpreduce>>
(StructEdgeList<unsigned> *, int *, int);

template __global__ void
revStructEdgeListBfs<Reduction<blockreduce>>
(StructEdgeList<unsigned> *, int *, int);
#endif
