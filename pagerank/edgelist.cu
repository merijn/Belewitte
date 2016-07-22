#include "pagerank.h"

__global__ void
updateRankEdgeList
    ( EdgeListCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count) {
        int64_t inEdge = graph->inEdges[idx];
        unsigned *vertices = &graph->vertices[inEdge];

        unsigned start = vertices[0];
        unsigned end = vertices[1];
        unsigned degree = end - start;
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[inEdge] / degree;
        atomicAdd(&new_pagerank[graph->outEdges[idx]], new_rank);
    }
}
