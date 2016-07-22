#include "bfs.h"

__global__ void
vertexPullBfs(ReverseCSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->vertex_count) {
        unsigned *reverse_vertices = graph->reverse_vertices;
        unsigned start = reverse_vertices[idx];
        unsigned end = reverse_vertices[idx + 1];

        unsigned *reverse_edges = graph->reverse_edges;

        for (unsigned i = start; i < end; i++) {
            if (levels[reverse_edges[i]] == depth) {
                int newDepth = depth + 1;
                if (levels[idx] > newDepth) {
                    levels[idx] = newDepth;
                    finished = false;
                }
                break;
            }
        }
    }
}
