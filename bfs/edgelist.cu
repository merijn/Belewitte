#include "bfs.h"

__global__ void edgeListBfs(unsigned *nodes, unsigned *edges_in, unsigned *edges_out, size_t edge_count, int *levels, int depth)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < edge_count && levels[edges_in[idx]] == depth) {
        atomicMin(&levels[edges_out[idx]], depth + 1);
        finished = false;
    }
}
