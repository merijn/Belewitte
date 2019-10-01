#include "utils/cuda_utils.hpp"
#include "pagerank.hpp"

__device__ float diff = 0.0;

void resetDiff()
{
    const float val = 0.0;
    CUDA_CHK(cudaMemcpyToSymbol(diff, &val, sizeof val));
}

float getDiff()
{
    float val;
    CUDA_CHK(cudaMemcpyFromSymbol(&val, diff, sizeof val));
    return val;
}

__global__ void
zeroInitDegreesKernel(size_t count, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    for (uint64_t idx = startIdx; idx < count; idx += blockDim.x * gridDim.x) {
        degrees[idx] = 0;
    }
}

static __device__ __forceinline__
void updateDiff(float val)
{
    int lane = threadIdx.x % warpSize;

    for (int offset = warpSize/2; offset > 0; offset /= 2) {
        val += __shfl_down_sync(0xffffffff, val, offset);
    }

    if (lane == 0) atomicAdd(&diff, val);
}

__global__ void
edgeListComputeDegrees(EdgeList<unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;
    unsigned *edgeOrigins = graph->inEdges;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        unsigned v = edgeOrigins[idx];
        atomicAdd(&degrees[v], 1);
    }
}

__global__ void
structEdgeListComputeDegrees
(StructEdgeList<unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;
    edge<unsigned> *edges = graph->edges;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        atomicAdd(&degrees[edges[idx].in], 1);
    }
}

__global__ void
CSRComputeDegrees(CSR<unsigned,unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->vertex_count;
    unsigned *vertices = graph->vertices;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        unsigned degree = vertices[idx + 1] - vertices[idx];
        atomicAdd(&degrees[idx], degree);
    }
}

__global__ void
reverseEdgeListComputeDegrees
(EdgeList<unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;
    unsigned *edgeOrigins = graph->outEdges;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        unsigned v = edgeOrigins[idx];
        atomicAdd(&degrees[v], 1);
    }
}

__global__ void
reverseStructEdgeListComputeDegrees
(StructEdgeList<unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->edge_count;
    edge<unsigned> *edges = graph->edges;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        atomicAdd(&degrees[edges[idx].out], 1);
    }
}

__global__ void
reverseCSRComputeDegrees(CSR<unsigned,unsigned> *graph, unsigned *degrees)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t size = graph->vertex_count;
    unsigned *vertices = graph->vertices;
    unsigned *edges = graph->edges;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        unsigned start = vertices[idx];
        unsigned end = vertices[idx + 1];

        for (unsigned i = start; i < end; i++) {
            unsigned v = edges[i];
            atomicAdd(&degrees[v], 1);
        }
    }
}

__global__ void
consolidateRank
(uint64_t size, unsigned*, float *pagerank, float *new_pagerank, bool)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        float new_rank = ((1.0 - dampening) / size) + (dampening * new_pagerank[idx]);
        float my_diff = fabsf(new_rank - pagerank[idx]);

        pagerank[idx] = new_rank;
        new_pagerank[idx] = 0.0f;

        updateDiff(my_diff);
    }
}

__global__ void
consolidateRankNoDiv
( uint64_t size
, unsigned* degrees
, float *pagerank
, float *new_pagerank
, bool notLast
)
{
    uint64_t startIdx = (blockIdx.x * blockDim.x) + threadIdx.x;

    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x) {
        float new_rank = ((1 - dampening) / size) + (dampening * new_pagerank[idx]);
        float my_diff = fabsf(new_rank - pagerank[idx]);

        unsigned degree = degrees[idx];

        if (degree != 0 && notLast) new_rank = new_rank / degree;
        pagerank[idx] = new_rank;
        new_pagerank[idx] = 0.0f;

        updateDiff(my_diff);
    }
}
