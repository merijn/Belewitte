#include <algorithm>
#include "bfs.hpp"

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
