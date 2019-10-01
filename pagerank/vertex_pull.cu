#include "pagerank.hpp"

__global__ void
vertexPullPageRank
( CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->vertex_count;

    float degree;
    float newRank = 0.0f;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        unsigned *rev_vertices = graph->vertices;
        unsigned *reverse_edges = graph->edges;

        unsigned start = rev_vertices[idx];
        unsigned end = rev_vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            uint64_t rev_edge = reverse_edges[i];

            degree = degrees[rev_edge];

            newRank += pagerank[rev_edge] / degree;
        }

        new_pagerank[idx] = newRank;
    }
}
