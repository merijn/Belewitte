#include <math_functions.h>
#include "pagerank.hpp"

template<typename T>
static __device__ inline void
memcpy_SIMD(size_t warp_size, size_t warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

__global__ void
vertexPullWarpNoDiv(size_t warp_size, size_t chunk_size,
                    InverseVertexCSR<unsigned,unsigned> *graph, float *pagerank,
                    float *new_pagerank)
{
    const size_t vertex_count = graph->vertex_count;
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t W_ID = THREAD_ID / warp_size;
    extern __shared__ unsigned MEM[];
    unsigned *myVertices = &MEM[W_ID * (1+chunk_size)];

    const size_t v_ = min(W_ID * chunk_size, vertex_count);
    const size_t end = min(chunk_size, (vertex_count - v_));

    memcpy_SIMD(warp_size, W_OFF, end + 1, myVertices, &graph->vertices[v_]);

    const unsigned * const rev_edges = graph->edges;
    for (int v = 0; v < end; v++) {
        float my_new_rank = 0;
        const unsigned num_nbr = myVertices[v+1] - myVertices[v];
        const unsigned *nbrs = &rev_edges[myVertices[v]];
        for (int i = W_OFF; i < num_nbr; i += warp_size) {
            my_new_rank += pagerank[nbrs[i]];
        }
        atomicAdd(&new_pagerank[v_ + v], my_new_rank);
    }
}
