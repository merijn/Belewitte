#ifndef PAGERANK_HPP
#define PAGERANK_HPP

#include <algorithm>
#include <cuda_runtime.h>

#include "GraphRep.hpp"

const float dampening = 0.85f;
const float epsilon = 0.001f;
const int max_iterations = 50;

void resetDiff();
float getDiff();

__global__ void
zeroInitDegreesKernel(size_t vertexCount, unsigned *degrees);

__global__ void
edgeListComputeDegrees
(EdgeList<unsigned> *graph, unsigned *degrees);

__global__ void
structEdgeListComputeDegrees
(StructEdgeList<unsigned> *graph, unsigned *degrees);

__global__ void
CSRComputeDegrees
(CSR<unsigned,unsigned> *graph, unsigned *degrees);

__global__ void
reverseCSRComputeDegrees
(CSR<unsigned,unsigned> *graph, unsigned *degrees);

__global__ void
consolidateRank
(size_t, unsigned *degrees, float *pagerank, float *new_pagerank, bool);

__global__ void
consolidateRankNoDiv
(size_t, unsigned *degrees, float *pagerank, float *new_pagerank, bool);

__global__ void
vertexPushPageRank
( CSR<unsigned,unsigned> *graph
, unsigned* degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
vertexPullPageRank
( CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
vertexPullNoDivPageRank
( CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
vertexPushWarpPageRank
( size_t warp_size
, size_t chunk_size
, CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
vertexPullWarpPageRank
( size_t warp_size
, size_t chunk_size
, CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
vertexPullWarpNoDivPageRank
( size_t warp_size
, size_t chunk_size
, CSR<unsigned,unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
structEdgeListPageRank
( StructEdgeList<unsigned> *graph
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);

__global__ void
edgeListPageRank
( EdgeList<unsigned> *
, unsigned *degrees
, float *pagerank
, float *new_pagerank
);
#endif
