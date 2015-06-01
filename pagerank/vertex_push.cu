#include "pagerank.h"

__global__ void vertexPush(int *nodes, int *edges, int size, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float outgoingRank = 0.0f;

    if (idx < size) {
        degree = nodes[idx + 1] - nodes[idx];

        if (degree != 0) outgoingRank = pagerank[idx] / degree;

        for (int i = nodes[idx]; i < nodes[idx + 1]; i++) {
            atomicAdd(&new_pagerank[edges[i]], outgoingRank);
        }
    }
}
