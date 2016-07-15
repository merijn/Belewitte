#include "pagerank.h"

__global__ void
vertexPullNoDiv(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    float newRank = 0.0f;

    if (idx < graph->vertex_count) {
        for (int i = graph->reverse_vertices[idx]; i < graph->reverse_vertices[idx + 1]; i++) {
            newRank += pagerank[graph->reverse_edges[i]];
        }
    }

    new_pagerank[idx] = newRank;
}
