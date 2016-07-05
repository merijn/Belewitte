#include "bfs.h"
#include "../WarpDispatch.hpp"

template<size_t warp_size, typename T> static __device__ void
memcpy_SIMD(int warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

template<size_t W_SZ> __device__ void
expand_bfs_SIMD(unsigned W_OFF, unsigned cnt, unsigned *edges, int *levels, int curr)
{
    for (unsigned IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        unsigned v = edges[IDX];
        if (levels[v] == -1) {
            levels[v] = curr + 1;
            finished = false;
        }
    }
    __threadfence_block();
}

template<size_t warp_size, size_t chunk_size> static __device__ void
warp_bfs(size_t N, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, int *levels, int depth)
{
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t W_ID = THREAD_ID / warp_size;
    extern __shared__ char SMEM[];
    pull_warp_mem_t<chunk_size> *tmp = (pull_warp_mem_t<chunk_size>*) SMEM;
    pull_warp_mem_t<chunk_size> *MY = &tmp[threadIdx.x / warp_size];

    const size_t v_ = size_min_bfs(W_ID * chunk_size, static_cast<unsigned long>(N));
    const size_t end = size_min_bfs(chunk_size, (N - v_));

    memcpy_SIMD<warp_size>(W_OFF, end + 1, MY->vertices, &rev_nodes[v_]);

    for (int v = 0; v < end; v++) {
        bool updated = false;
        const unsigned num_nbr = MY->vertices[v+1] - MY->vertices[v];
        const unsigned *nbrs = &rev_edges[MY->vertices[v]];
        for (int i = W_OFF; i < num_nbr; i += warp_size) {
            if (levels[nbrs[i]] == depth) {
                updated = true;
            }
        }

        if (updated) {
            levels[v_ + v] = depth + 1;
            finished = false;
        }
    }
}

template<size_t warp_size, size_t chunk_size>
__global__ void
vertexPullWarpBfs(size_t vertex_count, size_t edge_count, unsigned *rev_nodes, unsigned *rev_edges, unsigned *nodes, int *levels, int depth)
{ warp_bfs<warp_size, chunk_size>(vertex_count, rev_nodes, rev_edges, nodes, levels, depth); }

template<size_t warp, size_t chunk>
struct VertexPullWarpBfs {
    static void work()
    { vertexPullWarpBfs<warp, chunk> <<<1, 1, 0 >>>(0, 0, NULL, NULL, NULL, NULL, 0); }
};

void dummyPullBfs(size_t warp, size_t chunk)
{ warp_dispatch<VertexPullWarpBfs>::work(warp, chunk); }
