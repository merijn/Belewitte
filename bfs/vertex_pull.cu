#include "bfs.h"

__global__ void vertexPullBfs(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, int *levels, int depth)
{
    bool updated = false;
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < vertex_count) {
        for (int i = rev_nodes[idx]; i < rev_nodes[idx + 1]; i++) {
            if (levels[rev_edges[i]] == depth) {
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
