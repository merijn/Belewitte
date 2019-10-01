#include "pagerank.hpp"

__global__ void
revStructEdgeListPageRank
( StructEdgeList<unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        edge<unsigned> *edge = &graph->edges[idx];
        unsigned origin = edge->out;
        unsigned destination = edge->in;

        unsigned degree = degrees[origin];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[origin] / degree;
        atomicAdd(&new_pagerank[destination], new_rank);
    }
}
