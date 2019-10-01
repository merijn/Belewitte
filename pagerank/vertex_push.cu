#include "pagerank.hpp"

__global__ void
vertexPushPageRank
( CSR<unsigned,unsigned> *graph
, unsigned *
, float *pagerank
, float *new_pagerank
)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->vertex_count;

    unsigned degree;
    float outgoingRank = 0.0f;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
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
