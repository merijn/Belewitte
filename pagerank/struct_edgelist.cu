#include "pagerank.hpp"

__global__ void
structEdgeListCSR
    ( StructEdgeListCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count) {
        edge<unsigned> *edge = &graph->edges[idx];
        unsigned origin = edge->in;
        unsigned destination = edge->out;
        unsigned *vertices = &graph->vertices[origin];

        unsigned degree = vertices[1] - vertices[0];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[origin] / degree;
        atomicAdd(&new_pagerank[destination], new_rank);
    }
}
