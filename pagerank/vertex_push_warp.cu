#include <math_functions.h>
#include "pagerank.hpp"

template<typename T>
static __device__ inline void
memcpy_SIMD(size_t warp_size, int warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

__global__ void
vertexPushWarp(size_t warp_size, size_t chunk_size,
               CSR<unsigned,unsigned> *graph, float *pagerank,
               float *new_pagerank)
{
    const size_t vertex_count = graph->vertex_count;
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t W_ID = THREAD_ID / warp_size;
    const size_t BLOCK_W_ID = threadIdx.x / warp_size;

    extern __shared__ float MEM[];
    float *myRanks = &MEM[BLOCK_W_ID * chunk_size];
    unsigned *vertices = (unsigned*) &MEM[(blockDim.x/warp_size) * chunk_size];
    unsigned *myVertices = &vertices[(1+chunk_size) * BLOCK_W_ID];

    const size_t v_ = min(W_ID * chunk_size, vertex_count);
    const size_t end = min(chunk_size, (vertex_count - v_));

    memcpy_SIMD(warp_size, W_OFF, end, myRanks, &pagerank[v_]);
    memcpy_SIMD(warp_size, W_OFF, end + 1, myVertices, &graph->vertices[v_]);

    const unsigned * const edges = graph->edges;
    for (int v = 0; v < end; v++) {
        const unsigned num_nbr = myVertices[v+1] - myVertices[v];
        const unsigned *nbrs = &edges[myVertices[v]];
        const float my_rank = myRanks[v] / num_nbr;
        for (int i = W_OFF; i < num_nbr; i += warp_size) {
            atomicAdd(&new_pagerank[nbrs[i]], my_rank);
        }
    }
}
