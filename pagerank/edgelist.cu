#include "pagerank.h"

__global__ void
updateRankEdgeList(EdgeListCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < graph->edge_count) {
        int degree = graph->vertices[graph->inEdges[idx] + 1] - graph->vertices[graph->inEdges[idx]];
        float new_rank = 0.0f;
        if (degree != 0) new_rank = pagerank[graph->inEdges[idx]] / degree;
        atomicAdd(&new_pagerank[graph->outEdges[idx]], new_rank);
    }
}
