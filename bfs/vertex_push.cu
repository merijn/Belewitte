#include "bfs.h"

__global__ void vertexPushBfs(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges, int *levels, int depth)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < vertex_count && levels[idx] == depth) {
        for (int i = nodes[idx]; i < nodes[idx + 1]; i++) {
            atomicMin(&levels[edges[i]], depth + 1);
        }
        finished = false;
    }
}
