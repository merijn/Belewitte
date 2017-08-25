#include "bfs.hpp"

__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->edge_count;
    unsigned count = 1U;
    int newDepth = depth + 1;

    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
         idx < size && levels[graph->inEdges[idx]] == depth;
         idx += blockDim.x * gridDim.x)
    {
        if (atomicMin(&levels[graph->outEdges[idx]], newDepth) > newDepth) {
            updateFrontier(count);
        }
    }
}
