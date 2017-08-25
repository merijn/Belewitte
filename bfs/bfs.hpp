#ifndef __BFS_H__
#define __BFS_H__

#include <algorithm>
#include <cuda_runtime.h>
#include "GraphRep.hpp"

void resetFrontier();
unsigned getFrontier();

template<size_t chunk_sz>
struct push_warp_mem_t {
    int levels[chunk_sz];
    unsigned vertices[chunk_sz + 1];
};

template<int chunk_sz>
struct pull_warp_mem_t {
    unsigned vertices[chunk_sz + 1];
};

#ifdef __CUDACC__
extern __device__ unsigned frontier;

static __device__ __forceinline__
void updateFrontier(unsigned val)
{ atomicAdd(&frontier, val); }
#endif

__global__ void
setArray(int *array, size_t size, int val);

__global__ void
set_root(int *depths, unsigned root);

__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

template<size_t, size_t>
__global__ void
vertexPushWarpBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);

__global__ void
revEdgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);
#endif
