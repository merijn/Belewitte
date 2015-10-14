#include "pagerank.h"

__global__ void updateRankEdgeList(unsigned *nodes, unsigned *edges_in, unsigned *edges_out, size_t edge_count, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < edge_count) {
        int degree = nodes[edges_in[idx] + 1] - nodes[edges_in[idx]];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[edges_in[idx]] / degree;
        atomicAdd(&new_pagerank[edges_out[idx]], new_rank);
    }
}
