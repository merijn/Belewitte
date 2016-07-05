#include "pagerank.h"

__global__ void
vertexPush
    ( size_t vertex_count
    , size_t edge_count
    , unsigned *nodes
    , unsigned *edges
    , float *pagerank
    , float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float outgoingRank = 0.0f;

    if (idx < vertex_count) {
        degree = nodes[idx + 1] - nodes[idx];

        if (degree != 0) outgoingRank = pagerank[idx] / degree;

        for (int i = nodes[idx]; i < nodes[idx + 1]; i++) {
            atomicAdd(&new_pagerank[edges[i]], outgoingRank);
        }
    }
}
