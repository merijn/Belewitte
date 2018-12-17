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
        unsigned inEdge = edge->in;
        unsigned outEdge = edge->out;
        unsigned *vertices = &graph->vertices[inEdge];

        unsigned degree = vertices[1] - vertices[0];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[inEdge] / degree;
        atomicAdd(&new_pagerank[outEdge], new_rank);
    }
}
