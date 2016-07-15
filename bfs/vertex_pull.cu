#include "bfs.h"

__global__ void
vertexPullBfs(ReverseCSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    bool updated = false;
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->vertex_count) {
        for (int i = graph->reverse_vertices[idx]; i < graph->reverse_vertices[idx + 1]; i++) {
            if (levels[graph->reverse_edges[i]] == depth) {
                updated = true;
                break;
            }
        }

        int newDepth = depth + 1;
        if (updated && levels[idx] > newDepth) {
            levels[idx] = newDepth;
            finished = false;
        }
    }
}
