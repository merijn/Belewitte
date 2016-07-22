#ifndef __PAGERANK_H__
#define __PAGERANK_H__

#include <algorithm>
#include <cuda_runtime.h>

#include "GraphRep.hpp"

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

void resetDiff();
float getDiff();

__global__ void
setArrayFloat(float *array, size_t size, float val);

__global__ void
consolidateRank(size_t, float *pagerank, float *new_pagerank);

__global__ void
consolidateRankPull(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPush(CSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPull(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPullNoDiv(ReverseCSR<unsigned, unsigned> *graph, float *pagerank, float *new_pagerank);


template<size_t, size_t>
__global__ void
vertexPushWarp(CSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

template<size_t, size_t>
__global__ void
vertexPullWarp(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

template<size_t, size_t>
__global__ void
vertexPullWarpNoDiv(ReverseCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
updateRankStructEdgeList(StructEdgeListCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
updateRankEdgeList(EdgeListCSR<unsigned,unsigned> *, float *pagerank, float *new_pagerank);
#endif
