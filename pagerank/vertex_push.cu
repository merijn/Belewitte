#include "pagerank.h"

__global__ void
vertexPush(CSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float outgoingRank = 0.0f;

    if (idx < graph->vertex_count) {
        degree = graph->vertices[idx + 1] - graph->vertices[idx];

        if (degree != 0) outgoingRank = pagerank[idx] / degree;

        for (int i = graph->vertices[idx]; i < graph->vertices[idx + 1]; i++) {
            atomicAdd(&new_pagerank[graph->edges[i]], outgoingRank);
        }
    }
}
