#include "bfs.h"

__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->vertex_count && levels[idx] == depth) {
        for (int i = graph->vertices[idx]; i < graph->vertices[idx + 1]; i++) {
            atomicMin(&levels[graph->edges[i]], depth + 1);
        }
        finished = false;
    }
}
