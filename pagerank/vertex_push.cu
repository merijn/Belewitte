#include "pagerank.hpp"

__global__ void
vertexPush
    ( CSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    int degree;
    float outgoingRank = 0.0f;

    if (idx < graph->vertex_count) {
        unsigned *vertices = graph->vertices;
        unsigned *edges = graph->edges;
        unsigned start = vertices[idx];
        unsigned end = vertices[idx + 1];

        degree = end - start;

        if (degree != 0) outgoingRank = pagerank[idx] / degree;

        for (unsigned i = start; i < end; i++) {
            atomicAdd(&new_pagerank[edges[i]], outgoingRank);
        }
    }
}
