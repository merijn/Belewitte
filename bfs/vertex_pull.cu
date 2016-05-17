#include "bfs.h"

__global__ void vertexPullBfs(unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, size_t size, int *levels, int depth)
{
    bool updated = false;
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < size) {
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
