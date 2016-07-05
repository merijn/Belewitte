#include "pagerank.h"

__global__ void
vertexPull
    ( size_t vertex_count
    , size_t edge_count
    , unsigned *rev_nodes
    , unsigned *rev_edges
    , unsigned *nodes
    , float *pagerank
    , float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float incomingRank = 0.0f;
    float newRank = 0.0f;

    if (idx < vertex_count) {
        for (int i = rev_nodes[idx]; i < rev_nodes[idx + 1]; i++) {
            degree = nodes[rev_edges[i] + 1] - nodes[rev_edges[i]];

            incomingRank = pagerank[rev_edges[i]] / degree;
            newRank += incomingRank;
        }
    }

    new_pagerank[idx] = newRank;
}
