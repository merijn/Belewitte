#ifndef __PAGERANK_H__
#define __PAGERANK_H__

#include <cuda_runtime.h>

const float dampening = 0.85f;
const float epsilon = 0.001f;
const int max_iterations = 10;

template<int chunk_sz>
struct pull_warp_mem_t {
    int vertices[chunk_sz + 1];
};

template<int chunk_sz>
struct push_warp_mem_t {
    int vertices[chunk_sz + 1];
    float ranks[chunk_sz];
};

void resetDiff();
float getDiff();

__global__ void setArrayFloat(float *array, size_t size, float val);
__global__ void consolidateRank(int, float *pagerank, float *new_pagerank);
__global__ void consolidateRankPull(int *nodes, int size, float *pagerank, float *new_pagerank);

__global__ void vertexPush(int *nodes, int *edges, int size, float *pagerank, float *new_pagerank);
__global__ void vertexPull(int *rev_nodes, int *rev_edges, int *nodes, int size, float *pagerank, float *new_pagerank);
__global__ void vertexPullNoDiv(int *rev_nodes, int *rev_edges, int *nodes, int size, float *pagerank, float *new_pagerank);


template<size_t, size_t>
__global__ void vertexPushWarp(int *nodes, int *edges, int size, float *pagerank, float *new_pagerank);

template<int, int>
__global__ void vertexPullWarp(int *rev_nodes, int *rev_edges, int *nodes, int size, float *pagerank, float *new_pagerank);
template<int, int>
__global__ void vertexPullWarpNoDiv(int *rev_nodes, int *rev_edges, int *nodes, int size, float *pagerank, float *new_pagerank);

__global__ void updateRankStructEdgeList(int *nodes, struct edge *, int, float *pagerank, float *new_pagerank);
__global__ void updateRankEdgeList(int *nodes, int *edges_in, int *edges_out, int edge_count, float *pagerank, float *new_pagerank);
#endif
