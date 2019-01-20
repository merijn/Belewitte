#include "pagerank.hpp"

__global__ void
edgeListCSR
    ( EdgeListCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count) {
        int64_t origin = graph->inEdges[idx];
        unsigned *vertices = &graph->vertices[origin];

        unsigned start = vertices[0];
        unsigned end = vertices[1];
        unsigned degree = end - start;
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[origin] / degree;
        atomicAdd(&new_pagerank[graph->outEdges[idx]], new_rank);
    }
}
