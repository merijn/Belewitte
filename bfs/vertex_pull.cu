#include "bfs.h"

__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    int newDepth = depth + 1;

    if (idx < graph->vertex_count && levels[idx] > newDepth) {
        unsigned *reverse_vertices = graph->vertices;
        unsigned start = reverse_vertices[idx];
        unsigned end = reverse_vertices[idx + 1];

        unsigned *reverse_edges = graph->edges;

        for (unsigned i = start; i < end; i++) {
            if (levels[reverse_edges[i]] == depth) {
                levels[idx] = newDepth;
                finished = false;
                break;
            }
        }
    }
}
