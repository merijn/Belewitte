#ifndef __BFS_H__
#define __BFS_H__

#include <algorithm>
#include <cuda_runtime.h>

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

__global__ void vertexPushBfs(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges, int *levels, int depth);
__global__ void vertexPullBfs(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, int *levels, int depth);

template<size_t, size_t>
__global__ void vertexPushWarpBfs(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges, int *levels, int depth);

template<size_t, size_t>
__global__ void vertexPullWarpBfs(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, int *levels, int depth);

__global__ void edgeListBfs(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges_in, unsigned *edges_out, int *levels, int depth);
#endif
