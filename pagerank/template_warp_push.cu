#include "pagerank.h"
#include "../WarpDispatch.hpp"

template<int warp_size, typename T> static __device__ void
memcpy_SIMD(int warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

template<int warp_size, int chunk_size> static __device__ void
warp_bfs_kernel(int N, int *nodes, int *edges, float *pagerank, float *new_pagerank)
{
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const int W_ID = THREAD_ID / warp_size;
    extern __shared__ char SMEM[];
    push_warp_mem_t<chunk_size> *tmp = (push_warp_mem_t<chunk_size>*)(SMEM);
    push_warp_mem_t<chunk_size> *MY = &tmp[threadIdx.x / warp_size];

    const int v_ = min(W_ID * chunk_size, N);
    const int end = min(chunk_size, (N - v_));

    memcpy_SIMD<warp_size>(W_OFF, end, MY->ranks, &pagerank[v_]);
    memcpy_SIMD<warp_size>(W_OFF, end + 1, MY->vertices, &nodes[v_]);

    for (int v = 0; v < end; v++) {
        const int num_nbr = MY->vertices[v+1] - MY->vertices[v];
        const int *nbrs = &edges[MY->vertices[v]];
        const float my_rank = MY->ranks[v] / num_nbr;
        for (int i = W_OFF; i < num_nbr; i += warp_size) {
            atomicAdd(&new_pagerank[nbrs[i]], my_rank);
        }
    }
}

template<size_t warp_size, size_t chunk_size>
__global__ void
vertexPushWarp(int *nodes, int *edges, int size, float *pagerank, float *new_pagerank)
{
    warp_bfs_kernel<warp_size, chunk_size>(size, nodes, edges, pagerank, new_pagerank);
}

template<size_t warp, size_t chunk>
struct VertexPushWarp {
    static void work()
    { vertexPushWarp<warp, chunk> <<<1, 1, 0 >>>(NULL, NULL, 0, NULL, NULL); }
};

void dummyPush(size_t warp, size_t chunk)
{ warp_dispatch<VertexPushWarp>::work(warp, chunk); }
