#include "bfs.h"

__global__ void vertexPushBfs(unsigned *nodes, unsigned *edges, size_t size, int *levels, int depth)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < size && levels[idx] == depth) {
        for (int i = nodes[idx]; i < nodes[idx + 1]; i++) {
            atomicMin(&levels[edges[i]], depth + 1);
        }
        finished = false;
    }
}
