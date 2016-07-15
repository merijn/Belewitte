#include "bfs.h"
#include "../WarpDispatch.hpp"

template<int warp_size, typename T> static __device__ void
memcpy_SIMD(int warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

template<int W_SZ> __device__ void
expand_bfs_SIMD(unsigned W_OFF, unsigned cnt, const unsigned *edges, int *levels, int curr)
{
    for (unsigned IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        unsigned v = edges[IDX];
        atomicMin(&levels[v], curr + 1);
        finished = false;
    }
    __threadfence_block();
}

template<int warp_size, int chunk_size> static __device__ void
warp_bfs(size_t N, unsigned *nodes, unsigned *edges, int *levels, int depth)
{
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t W_ID = THREAD_ID / warp_size;
    extern __shared__ char SMEM[];
    push_warp_mem_t<chunk_size> *tmp = (push_warp_mem_t<chunk_size>*)(SMEM);
    push_warp_mem_t<chunk_size> *MY = &tmp[threadIdx.x / warp_size];

    const size_t v_ = size_min_bfs(W_ID * chunk_size, N);
    const size_t end = size_min_bfs(chunk_size, (N - v_));

    memcpy_SIMD<warp_size>(W_OFF, end, MY->levels, &levels[v_]);
    memcpy_SIMD<warp_size>(W_OFF, end + 1, MY->vertices, &nodes[v_]);

    for (int v = 0; v < end; v++) {
        const unsigned num_nbr = MY->vertices[v+1] - MY->vertices[v];
        const unsigned *nbrs = &edges[MY->vertices[v]];
        if (levels[v] == depth) {
            expand_bfs_SIMD<warp_size>(W_OFF, num_nbr, nbrs, levels, depth);
        }
    }
}

template<size_t warp_size, size_t chunk_size>
__global__ void
vertexPushWarpBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{ warp_bfs<warp_size, chunk_size>(graph->vertex_count, graph->vertices, graph->edges, levels, depth); }

template<size_t warp, size_t chunk>
struct VertexPushWarpBfs {
    static void work()
    { vertexPushWarpBfs<warp, chunk> <<<1, 1, 0 >>>(NULL, NULL, 0); }
};

void dummyPushBfs(size_t warp, size_t chunk)
{ warp_dispatch<VertexPushWarpBfs>::work(warp, chunk); }
