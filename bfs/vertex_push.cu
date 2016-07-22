#include "bfs.h"

__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->vertex_count && levels[idx] == depth) {
        unsigned *vertices = graph->vertices;
        unsigned start = vertices[idx];
        unsigned end = vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            atomicMin(&levels[graph->edges[i]], depth + 1);
        }
        finished = false;
    }
}
