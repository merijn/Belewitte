#include <algorithm>
#include "bfs.h"

#define CUDA_CHK(ans) { \
    cudaError_t code = ans; \
    if (code != cudaSuccess) { \
        cudaAssert(code, __FILE__, __LINE__); \
    } \
}
void __attribute__((noreturn))
cudaAssert(const cudaError_t code, const char *file, const int line);

__device__ unsigned frontier = 0;

void resetFrontier()
{
    const unsigned val = 0;
    CUDA_CHK(cudaMemcpyToSymbol(frontier, &val, sizeof val));
}

unsigned getFrontier()
{
    unsigned val;
    CUDA_CHK(cudaMemcpyFromSymbol(&val, frontier, sizeof val));
    return val;
}

__global__ void setArray(int *array, size_t size, int val)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) array[idx] = val;
}

__global__ void set_root(int *array, unsigned idx)
{ array[idx] = 0; }
