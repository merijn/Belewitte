#ifndef PAGERANK_HPP
#define PAGERANK_HPP

#include <algorithm>
#include <cuda_runtime.h>

#include "GraphRep.hpp"

const float dampening = 0.85f;
const float epsilon = 0.001f;
const int max_iterations = 10;

void resetDiff();
float getDiff();

__global__ void
setArrayFloat(float *array, size_t size, float val);

__global__ void
consolidateRank(size_t, float *pagerank, float *new_pagerank);

__global__ void
consolidateRankPull(ReversedCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPush(CSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPull(ReversedCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPullNoDiv(ReversedCSR<unsigned, unsigned> *graph, float *pagerank, float *new_pagerank);


__global__ void
vertexPushWarp(size_t, size_t, CSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPullWarp(size_t, size_t, ReversedCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
vertexPullWarpNoDiv(size_t, size_t, ReversedCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
updateRankStructEdgeList(StructEdgeListCSR<unsigned,unsigned> *graph, float *pagerank, float *new_pagerank);

__global__ void
updateRankEdgeList(EdgeListCSR<unsigned,unsigned> *, float *pagerank, float *new_pagerank);
#endif
