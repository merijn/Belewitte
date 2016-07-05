#include "bfs.h"

__global__ void edgeListBfs(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges_in, unsigned *edges_out, int *levels, int depth)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < edge_count && levels[edges_in[idx]] == depth) {
        atomicMin(&levels[edges_out[idx]], depth + 1);
        finished = false;
    }
}
