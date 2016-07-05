#include "pagerank.h"

__global__ void
vertexPullNoDiv
    ( size_t vertex_count
    , size_t edge_count
    , unsigned *rev_nodes
    , unsigned *rev_edges
    , unsigned *nodes
    , float *pagerank
    , float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    float newRank = 0.0f;

    if (idx < vertex_count) {
        for (int i = rev_nodes[idx]; i < rev_nodes[idx + 1]; i++) {
            newRank += pagerank[rev_edges[i]];
        }
    }

    new_pagerank[idx] = newRank;
}
