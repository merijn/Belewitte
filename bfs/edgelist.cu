#include "bfs.h"

__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count && levels[graph->inEdges[idx]] == depth) {
        atomicMin(&levels[graph->outEdges[idx]], depth + 1);
        finished = false;
    }
}
