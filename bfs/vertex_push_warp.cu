#include <cuda_runtime_api.h>
#include "bfs.hpp"

template<typename T>
static __device__ inline void
memcpy_SIMD(size_t warp_size, int warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}

template<typename BFSVariant>
static __device__ inline void
expand_bfs(BFSVariant &bfs, int W_SZ, unsigned W_OFF, unsigned cnt,
           const unsigned *edges, int *levels, int curr)
{
    int newDepth = curr + 1;
    for (unsigned IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        unsigned v = edges[IDX];
        if (atomicMin(&levels[v], newDepth) > newDepth) {
            bfs.update();
        }
    }
    __threadfence_block();
}

template<typename BFSVariant>
__global__ void
vertexPushWarpBfs
( size_t warp_size, size_t chunk_size, CSR<unsigned,unsigned> *graph
, int *levels, int depth)
{
    BFSVariant bfs;
    const size_t vertex_count = graph->vertex_count;
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t W_ID = THREAD_ID / warp_size;
    const size_t BLOCK_W_ID = threadIdx.x / warp_size;

    extern __shared__ int MEM[];
    int *myLevels = &MEM[chunk_size * BLOCK_W_ID];
    unsigned *vertices = (unsigned*) &MEM[(blockDim.x/warp_size) * chunk_size];
    unsigned *myVertices = &vertices[(1+chunk_size) * BLOCK_W_ID];

    const size_t v_ = min(W_ID * chunk_size, vertex_count);
    const size_t end = min(chunk_size, (vertex_count - v_));

    memcpy_SIMD(warp_size, W_OFF, end, myLevels, &levels[v_]);
    memcpy_SIMD(warp_size, W_OFF, end + 1, myVertices, &graph->vertices[v_]);

    for (int v = 0; v < end; v++) {
        const unsigned num_nbr = myVertices[v+1] - myVertices[v];
        const unsigned *nbrs = &graph->edges[myVertices[v]];
        if (myLevels[v] == depth) {
            expand_bfs(bfs, warp_size, W_OFF, num_nbr, nbrs, levels, depth);
        }
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
vertexPushWarpBfs<Reduction<normal>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushWarpBfs<Reduction<bulk>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushWarpBfs<Reduction<warpreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushWarpBfs<Reduction<blockreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);
#endif
