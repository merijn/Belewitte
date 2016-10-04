#include "bfs.h"

__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->edge_count;
    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
         idx < size && levels[graph->inEdges[idx]] == depth;
         idx += blockDim.x * gridDim.x)
    {
        atomicMin(&levels[graph->outEdges[idx]], depth + 1);
        finished = false;
    }
}
