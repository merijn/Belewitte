#include "pagerank.h"

__global__ void
vertexPullNoDiv
    ( ReversedCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    float newRank = 0.0f;

    if (idx < graph->vertex_count) {
        unsigned *rev_vertices = &graph->reverse_vertices[idx];
        unsigned start = rev_vertices[0];
        unsigned end = rev_vertices[1];

        unsigned *rev_edges = graph->reverse_edges;

        for (unsigned i = start; i < end; i++) {
            newRank += pagerank[rev_edges[i]];
        }
    }

    new_pagerank[idx] = newRank;
}
