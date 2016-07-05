#ifndef __PAGERANK_H__
#define __PAGERANK_H__

#include <algorithm>
#include <cuda_runtime.h>

#include "../Graph.hpp"

const float dampening = 0.85f;
const float epsilon = 0.001f;
const int max_iterations = 10;

template<int chunk_sz>
struct pull_warp_mem_t {
    unsigned vertices[chunk_sz + 1];
};

template<int chunk_sz>
struct push_warp_mem_t {
    unsigned vertices[chunk_sz + 1];
    float ranks[chunk_sz];
};

__device__ size_t size_min(size_t x, size_t y);

void resetDiff();
float getDiff();

__global__ void setArrayFloat(float *array, size_t size, float val);
__global__ void consolidateRank(size_t, float *pagerank, float *new_pagerank);
__global__ void consolidateRankPull(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, float *pagerank, float *new_pagerank);

__global__ void vertexPush(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges, float *pagerank, float *new_pagerank);
__global__ void vertexPull(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, float *pagerank, float *new_pagerank);
__global__ void vertexPullNoDiv(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, float *pagerank, float *new_pagerank);


template<size_t, size_t>
__global__ void vertexPushWarp(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges, float *pagerank, float *new_pagerank);

template<size_t, size_t>
__global__ void vertexPullWarp(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, float *pagerank, float *new_pagerank);
template<size_t, size_t>
__global__ void vertexPullWarpNoDiv(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, float *pagerank, float *new_pagerank);

__global__ void updateRankStructEdgeList(size_t vertex_count, size_t edge_count, unsigned *nodes, struct Edge<unsigned> *, float *pagerank, float *new_pagerank);
__global__ void updateRankEdgeList(size_t vertex_count, size_t edge_count, unsigned *nodes, unsigned *edges_in, unsigned *edges_out, float *pagerank, float *new_pagerank);
#endif
