#include "utils/cuda_utils.hpp"
#include "bfs.hpp"

static __device__ inline int
expand_bfs
(int W_SZ, unsigned W_OFF, unsigned cnt, const unsigned *edges, int *levels, int curr)
{
    int result = 0;
    for (unsigned IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        if (levels[edges[IDX]] == curr) {
            result = 1;
            break;
        }
    }
    return __any_sync(0xffffffff, result);
}

template<typename BFSVariant>
__global__ void
vertexPullWarpBfs
( size_t warp_size, size_t chunk_size, CSR<unsigned,unsigned> *graph
, int *levels, int depth)
{
    BFSVariant bfs;
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const uint64_t vertex_count = graph->vertex_count;
    const uint64_t warpsPerBlock = blockDim.x / warp_size;

    const uint64_t WARP_ID = THREAD_ID / warp_size;
    const int W_OFF = THREAD_ID % warp_size;
    const size_t BLOCK_W_ID = threadIdx.x / warp_size;
    const size_t sharedOffset = chunk_size * BLOCK_W_ID;

    extern __shared__ int MEM[];
    int *myLevels = &MEM[sharedOffset];
    unsigned *vertices = (unsigned*) &MEM[warpsPerBlock * chunk_size];
    unsigned *myVertices = &vertices[sharedOffset + BLOCK_W_ID];
    int newDepth = depth + 1;

    for ( uint64_t chunkIdx = WARP_ID
        ; chunk_size * chunkIdx < vertex_count
        ; chunkIdx += warpsPerBlock * gridDim.x
        )
    {
        const size_t v_ = min(chunkIdx * chunk_size, vertex_count);
        const size_t end = min(chunk_size, (vertex_count - v_));

        memcpy_SIMD(warp_size, W_OFF, end, myLevels, &levels[v_]);
        memcpy_SIMD(warp_size, W_OFF, end + 1, myVertices, &graph->vertices[v_]);

        for (int v = 0; v < end; v++) {
            const unsigned num_nbr = myVertices[v+1] - myVertices[v];
            const unsigned *nbrs = &graph->edges[myVertices[v]];
            if (myLevels[v] > depth) {
                if (expand_bfs(warp_size, W_OFF, num_nbr, nbrs, levels, depth)) {
                    myLevels[v] = newDepth;
                }
            }
        }

        for (unsigned IDX = W_OFF; IDX < chunk_size; IDX += warp_size) {
            if (myLevels[IDX] == newDepth) bfs.update();
        }
        memcpy_SIMD(warp_size, W_OFF, end, &levels[v_], myLevels);
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
vertexPullWarpBfs<Reduction<normal>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullWarpBfs<Reduction<bulk>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullWarpBfs<Reduction<warpreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullWarpBfs<Reduction<blockreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);
#endif
