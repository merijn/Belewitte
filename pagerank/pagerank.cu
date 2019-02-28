#include "pagerank.hpp"

#define CUDA_CHK(ans) { \
    cudaError_t code = ans; \
    if (code != cudaSuccess) { \
        cudaAssert(code, __FILE__, __LINE__); \
    } \
}
void __attribute__((noreturn))
cudaAssert(const cudaError_t code, const char *file, const int line);

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

static __device__ __forceinline__
void updateDiff(float val)
{
    int lane = threadIdx.x % warpSize;

    for (int offset = warpSize/2; offset > 0; offset /= 2) {
        val += __shfl_down(val, offset);
    }

    if (lane == 0) atomicAdd(&diff, val);
}

__global__ void
consolidateRank(size_t size, float *pagerank, float *new_pagerank, bool)
{
    size_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;

    if (idx < size) {
        float new_rank = ((1.0 - dampening) / size) + (dampening * new_pagerank[idx]);
        float my_diff = fabsf(new_rank - pagerank[idx]);

        pagerank[idx] = new_rank;
        new_pagerank[idx] = 0.0f;

        updateDiff(my_diff);
    }
}

__global__ void
consolidateRankNoDiv
    ( InverseVertexCSR<unsigned,unsigned> *graph
    , float *pagerank
    , float *new_pagerank
    , bool notLast
    )
{
    uint64_t idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    uint64_t vertex_count = graph->vertex_count;

    if (idx < vertex_count) {
        unsigned *outgoing_vertices = &graph->inverse_vertices[idx];

        float new_rank = ((1 - dampening) / vertex_count) + (dampening * new_pagerank[idx]);
        float my_diff = fabsf(new_rank - pagerank[idx]);

        unsigned start = outgoing_vertices[0];
        unsigned end = outgoing_vertices[1];
        unsigned degree = end - start;

        if (degree != 0 && notLast) new_rank = new_rank / degree;
        pagerank[idx] = new_rank;
        new_pagerank[idx] = 0.0f;

        updateDiff(my_diff);
    }
}
