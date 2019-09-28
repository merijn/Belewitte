#include <algorithm>
#include "CUDA.hpp"
#include "bfs.hpp"

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
