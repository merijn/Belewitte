#include "pagerank.h"

__global__ void
updateRankStructEdgeList(StructEdgeListCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count) {
        int degree = graph->vertices[graph->edges[idx].in + 1] - graph->vertices[graph->edges[idx].in];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[graph->edges[idx].in] / degree;
        atomicAdd(&new_pagerank[graph->edges[idx].out], new_rank);
    }
}
