#include "pagerank.h"

__global__ void vertexPullNoDiv(unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, size_t size, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    float newRank = 0.0f;

    if (idx < size) {
        for (int i = rev_nodes[idx]; i < rev_nodes[idx + 1]; i++) {
            newRank += pagerank[rev_edges[i]];
        }
    }

    new_pagerank[idx] = newRank;
}
