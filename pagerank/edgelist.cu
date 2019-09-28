#include "pagerank.hpp"

__global__ void
edgeListCSR
(EdgeListCSR<unsigned,unsigned> *graph , float *pagerank , float *new_pagerank)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
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
