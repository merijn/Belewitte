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
warp_bfs_kernel(int N, int *rev_nodes, int *rev_edges, int *nodes, float *pagerank, float *new_pagerank)
{
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const int W_ID = THREAD_ID / warp_size;
    extern __shared__ char SMEM[];
    pull_warp_mem_t<chunk_size> *tmp = (pull_warp_mem_t<chunk_size>*) SMEM;
    pull_warp_mem_t<chunk_size> *MY = &tmp[threadIdx.x / warp_size];

    const int v_ = min(W_ID * chunk_size, N);
    const int end = min(chunk_size, (N - v_));

    memcpy_SIMD<warp_size>(W_OFF, end + 1, MY->vertices, &rev_nodes[v_]);

    for (int v = 0; v < end; v++) {
        float my_new_rank = 0;
        const int num_nbr = MY->vertices[v+1] - MY->vertices[v];
        const int *nbrs = &rev_edges[MY->vertices[v]];
        for (int i = W_OFF; i < num_nbr; i += warp_size) {
            my_new_rank += pagerank[nbrs[i]];
        }
        atomicAdd(&new_pagerank[v_ + v], my_new_rank);
    }
}

template<int warp_size, int chunk_size>
__global__ void
vertexPullWarpNoDiv(int *rev_nodes, int *rev_edges, int *nodes, int size, float *pagerank, float *new_pagerank)
{
    warp_bfs_kernel<warp_size, chunk_size>(size, rev_nodes, rev_edges, nodes, pagerank, new_pagerank);
}

template<size_t warp, size_t chunk>
struct VertexPullWarpNoDiv {
    static void work()
    { vertexPullWarpNoDiv<warp, chunk> <<<1, 1, 0 >>>(NULL, NULL, NULL, 0, NULL, NULL); }
};

void dummyPullNoDiv(size_t warp, size_t chunk)
{ warp_dispatch<VertexPullWarpNoDiv>::work(warp, chunk); }
