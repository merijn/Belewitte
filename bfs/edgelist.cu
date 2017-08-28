#include "bfs.hpp"

template<typename BFSVariant>
__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->edge_count;
    BFSVariant bfs;
    int newDepth = depth + 1;

    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
         idx < size && levels[graph->inEdges[idx]] == depth;
         idx += blockDim.x * gridDim.x)
    {
        if (atomicMin(&levels[graph->outEdges[idx]], newDepth) > newDepth) {
            bfs.update();
        }
    }
    bfs.finalise();
}

template __global__ void
edgeListBfs<BFS<normal>>(EdgeList<unsigned> *, int *, int);

template __global__ void
edgeListBfs<BFS<bulk>>(EdgeList<unsigned> *, int *, int);

template __global__ void
edgeListBfs<BFS<warpreduce>>(EdgeList<unsigned> *, int *, int);

template __global__ void
edgeListBfs<BFS<blockreduce>>(EdgeList<unsigned> *, int *, int);
