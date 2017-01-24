#include "bfs.h"

__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->vertex_count;
    int newDepth = depth + 1;
    unsigned count = 0U;

    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
        idx < size && levels[idx] > newDepth;
        idx += blockDim.x * gridDim.x)
    {
        unsigned *reverse_vertices = graph->vertices;
        unsigned start = reverse_vertices[idx];
        unsigned end = reverse_vertices[idx + 1];

        unsigned *reverse_edges = graph->edges;

        for (unsigned i = start; i < end; i++) {
            if (levels[reverse_edges[i]] == depth) {
                levels[idx] = newDepth;
                count++;
                break;
            }
        }
    }
    updateFrontier(count);
}
