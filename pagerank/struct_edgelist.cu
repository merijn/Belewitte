#include "pagerank.h"

struct edge {
    int in, out;
};

__global__ void updateRankStructEdgeList(int *nodes, struct edge *edges, int size, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < size) {
        int degree = nodes[edges[idx].in + 1] - nodes[edges[idx].in];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[edges[idx].in] / degree;
        atomicAdd(&new_pagerank[edges[idx].out], new_rank);
    }
}
