#ifndef __BFS_H__
#define __BFS_H__

#include <algorithm>
#include <cuda_runtime.h>
#include "GraphRep.hpp"

void resetFinished();
bool getFinished();

template<size_t chunk_sz>
struct push_warp_mem_t {
    int levels[chunk_sz];
    unsigned vertices[chunk_sz + 1];
};

template<int chunk_sz>
struct pull_warp_mem_t {
    unsigned vertices[chunk_sz + 1];
};

extern __device__ bool finished;

__global__ void
setArray(int *array, size_t size, int val);

__global__ void
set_root(int *depths, int root);

__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

__global__ void
vertexPullBfs(ReverseCSR<unsigned,unsigned> *graph, int *levels, int depth);

template<size_t, size_t>
__global__ void
vertexPushWarpBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

template<size_t, size_t>
__global__ void
vertexPullWarpBfs(ReverseCSR<unsigned,unsigned> *graph, int *levels, int depth);

__global__ void
edgeListBfs(EdgeListCSR<unsigned,unsigned> *graph, int *levels, int depth);
#endif
