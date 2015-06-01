#include <algorithm>

#include "bfs.h"
#include "../WarpDispatch.hpp"

#define CUDA_CHK(ans) { cudaAssert((ans), __FILE__, __LINE__); }
void cudaAssert(const cudaError_t code, const char *file, const int line);

__device__ bool finished = true;

void resetFinished()
{
    const bool val = true;
    CUDA_CHK(cudaMemcpyToSymbol(finished, &val, sizeof val));
}

bool getFinished()
{
    bool val;
    CUDA_CHK(cudaMemcpyFromSymbol(&val, finished, sizeof val));
    return val;
}

__global__ void setArray(int *array, size_t size, int val)
{
    int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
    if (idx < size) array[idx] = val;
}

template<int W_SZ> __device__ void
memcpy_SIMD(int W_OFF, int cnt, int *dest, int *src)
{
    for (int IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        dest[IDX] = src[IDX];
    }
    __threadfence_block();
}

template<int W_SZ> __device__ void
expand_bfs_SIMD(int W_OFF, int cnt, int *edges, int *levels, int curr)
{
    for (int IDX = W_OFF; IDX < cnt; IDX += W_SZ) {
        int v = edges[IDX];
        if (levels[v] == -1) {
            levels[v] = curr + 1;
            finished = false;
        }
    }
    __threadfence_block();
}

template<int warp_size, int chunk_size> static __device__ void
warp_bfs_kernel(int N, int curr, int *levels, int *nodes, int *edges)
{
    const int THREAD_ID = (blockIdx.x * blockDim.x) + threadIdx.x;
    const int W_OFF = THREAD_ID % warp_size;
    const int W_ID = THREAD_ID / warp_size;
    extern __shared__ char SMEM[];
    warp_mem_t<chunk_size> *tmp = (warp_mem_t<chunk_size>*)SMEM;
    warp_mem_t<chunk_size> *MY = &tmp[threadIdx.x / warp_size];

    const int v_ = min(W_ID * chunk_size, N);
    const int end = min(chunk_size, (N - v_));

    memcpy_SIMD<warp_size>(W_OFF, end, MY->levels, &levels[v_]);
    memcpy_SIMD<warp_size>(W_OFF, end + 1, MY->nodes, &nodes[v_]);

    for (int v = 0; v < end; v++) {
        if (MY->levels[v] == curr) {
            int num_nbr = MY->nodes[v+1] - MY->nodes[v];
            int *nbrs = &edges[MY->nodes[v]];
            expand_bfs_SIMD<warp_size>(W_OFF, num_nbr, nbrs, levels, curr);
        }
    }
}

template<size_t warp_size, size_t chunk_size> __global__ void
cudabfs(int *nodes, int *edges, int N, int *levels, int curr)
{ warp_bfs_kernel<warp_size, chunk_size>(N, curr, levels, nodes, edges); }

__global__ void set_root(int *depths, int root) { depths[root] = 0; }

template<size_t warp, size_t chunk>
struct BFS {
    static void work()
    { cudabfs<warp, chunk> <<<1, 1, 0 >>>(NULL, NULL, 0, NULL, 0); }
};

void dummyBFS(size_t warp, size_t chunk)
{ warp_dispatch<BFS>::work(warp, chunk); }
