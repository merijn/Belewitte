#ifndef __BFS_H__
#define __BFS_H__

#include <algorithm>
#include <cuda_runtime.h>

#include "../Graph.hpp"

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

__device__ size_t size_min_bfs(size_t x, size_t y);

__global__ void setArray(int *array, size_t size, int val);
__global__ void set_root(int *depths, int root);

__global__ void vertexPushBfs(unsigned *nodes, unsigned *edges, size_t size, int *levels, int depth);
__global__ void vertexPullBfs(unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, size_t size, int *levels, int depth);

template<size_t, size_t>
__global__ void vertexPushWarpBfs(unsigned *nodes, unsigned *edges, size_t size, int *levels, int depth);

template<size_t, size_t>
__global__ void vertexPullWarpBfs(unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, size_t size, int *levels, int depth);

__global__ void edgeListBfs(unsigned *nodes, unsigned *edges_in, unsigned *edges_out, size_t edge_count, int *levels, int depth);
#endif
