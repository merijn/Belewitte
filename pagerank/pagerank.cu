#include "pagerank.h"

#define CUDA_CHK(ans) { \
    cudaError_t code = ans; \
    if (code != cudaSuccess) { \
        cudaAssert(code, __FILE__, __LINE__); \
    } \
}
void __attribute__((noreturn))
cudaAssert(const cudaError_t code, const char *file, const int line);

__device__ float diff = 0.0;

__device__ size_t size_min(size_t x, size_t y) { return min(static_cast<unsigned long long>(x), static_cast<unsigned long long>(y)); }

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

__global__ void setArrayFloat(float *array, size_t size, float val)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) array[idx] = val;
}

__global__ void consolidateRank(size_t size, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) {
        float new_rank = ((1 - dampening) / size) + (dampening * new_pagerank[idx]);
        float my_diff = abs(new_rank - pagerank[idx]);
        atomicAdd(&diff, my_diff);
        pagerank[idx] = new_rank;
        new_pagerank[idx] = 0.0;
    }
}

__global__ void consolidateRankPull(unsigned *nodes, size_t size, float *pagerank, float *new_pagerank)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) {
        float new_rank = ((1 - dampening) / size) + (dampening * new_pagerank[idx]);
        float my_diff = abs(new_rank - pagerank[idx]);
        atomicAdd(&diff, my_diff);
        int degree = nodes[idx+1] - nodes[idx];
        if (degree != 0) pagerank[idx] = new_rank / degree;
        new_pagerank[idx] = 0.0;
    }
}
