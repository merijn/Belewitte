#include "pagerank.hpp"

__global__ void
vertexPull
    ( ReversedCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
   uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    float degree;
    float newRank = 0.0f;

    if (idx < graph->vertex_count) {
        unsigned *rev_vertices = graph->reverse_vertices;
        unsigned *vertices = graph->vertices;
        unsigned *reverse_edges = graph->reverse_edges;

        unsigned start = rev_vertices[idx];
        unsigned end = rev_vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            uint64_t rev_edge = reverse_edges[i];

            unsigned startRev = vertices[rev_edge];
            unsigned endRev =  vertices[rev_edge + 1];

            degree = endRev - startRev;

            newRank += pagerank[rev_edge] / degree;
        }
    }

    new_pagerank[idx] = newRank;
}
