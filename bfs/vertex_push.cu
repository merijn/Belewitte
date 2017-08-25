#include "bfs.hpp"

__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->vertex_count;
    unsigned count = 0U;
    unsigned newDepth = depth + 1;
    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
         idx < size && levels[idx] == depth;
         idx += blockDim.x * gridDim.x)
    {
        unsigned *vertices = graph->vertices;
        unsigned start = vertices[idx];
        unsigned end = vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            if (atomicMin(&levels[graph->edges[i]], newDepth) > newDepth) {
                count++;
            }
        }
        updateFrontier(count);
    }
}
