#include <algorithm>

#include "bfs.h"
#include "../WarpDispatch.hpp"

#define CUDA_CHK(ans) { cudaAssert((ans), __FILE__, __LINE__); }
void cudaAssert(const cudaError_t code, const char *file, const int line);

__device__ bool finished = true;

void resetFinished()
{
    const bool val = true;
    CUDA_CHK(cudaMemcpyToSymbol(finished, &val, sizeof val));
}

bool getFinished()
{
    bool val;
    CUDA_CHK(cudaMemcpyFromSymbol(&val, finished, sizeof val));
    return val;
}

__device__ size_t size_min_bfs(size_t x, size_t y) { return min(static_cast<unsigned long long>(x), static_cast<unsigned long long>(y)); }

__global__ void setArray(int *array, size_t size, int val)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) array[idx] = val;
}

__global__ void set_root(int *array, int idx)
{ array[idx] = 0; }

