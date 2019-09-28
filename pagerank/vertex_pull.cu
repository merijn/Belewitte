#include "pagerank.hpp"

__global__ void
vertexPull
( InverseVertexCSR<unsigned,unsigned> *graph
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
        unsigned *vertices = graph->inverse_vertices;
        unsigned *reverse_edges = graph->edges;

        unsigned start = rev_vertices[idx];
        unsigned end = rev_vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            uint64_t rev_edge = reverse_edges[i];

            unsigned startRev = vertices[rev_edge];
            unsigned endRev =  vertices[rev_edge + 1];

            degree = endRev - startRev;

            newRank += pagerank[rev_edge] / degree;
        }

        new_pagerank[idx] = newRank;
    }
}
