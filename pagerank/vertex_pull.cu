#include "pagerank.h"

__global__ void
vertexPull(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float incomingRank = 0.0f;
    float newRank = 0.0f;

    if (idx < graph->vertex_count) {
        for (int i = graph->reverse_vertices[idx]; i < graph->reverse_vertices[idx + 1]; i++) {
            degree = graph->vertices[graph->reverse_edges[i] + 1] - graph->vertices[graph->reverse_edges[i]];

            incomingRank = pagerank[graph->reverse_edges[i]] / degree;
            newRank += incomingRank;
        }
    }

    new_pagerank[idx] = newRank;
}
