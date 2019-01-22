#ifndef BFS_HPP
#define BFS_HPP

#include <algorithm>
#include <cuda_runtime.h>
#include "GraphRep.hpp"

void resetFrontier();
unsigned getFrontier();

enum bfs_variant {
    normal,
    bulk,
    warpreduce,
    blockreduce
};

#ifdef __CUDACC__
extern __device__ unsigned frontier;

__device__ __forceinline__
unsigned warpReduceSum(unsigned val)
{
    for (int offset = warpSize/2; offset > 0; offset /= 2) {
        val += __shfl_down(val, offset);
     }

    return val;
}
#endif

template<bfs_variant variant>
struct BFS {
    static constexpr const char * suffix = "";
    __device__ __forceinline__ void update()
    {
#ifdef __CUDACC__
        atomicAdd(&frontier, 1U);
#endif
    }

    __device__ __forceinline__ void finalise() {}
};

template<>
struct BFS<bulk> {
    static constexpr const char * suffix = "-bulk";
    unsigned count;

    __device__ BFS<bulk>() : count(0U) {}

    __device__ __forceinline__ void update()
    { count++; }

    __device__ __forceinline__ void finalise()
    {
#ifdef __CUDACC__
        atomicAdd(&frontier, count);
#endif
    }
};

template<>
struct BFS<warpreduce> {
    static constexpr const char * suffix = "-warpreduce";
    unsigned count;

    __device__ BFS<warpreduce>() : count(0U) {}

    __device__ __forceinline__ void update()
    { count++; }

    __device__ __forceinline__ void finalise()
    {
#ifdef __CUDACC__
        int lane = threadIdx.x % warpSize;

        count = warpReduceSum(count);
        if (lane == 0) atomicAdd(&frontier, count);
#endif
    }
};

template<>
struct BFS<blockreduce> {
    static constexpr const char * suffix = "-blockreduce";
    unsigned count;

    __device__ BFS<blockreduce>() : count(0U) {}

    __device__ __forceinline__ void update()
    { count++; }

    __device__ __forceinline__ void finalise()
    {
#ifdef __CUDACC__
        static __shared__ unsigned shared[32]; // Shared mem for 32 partial sums
        int lane = threadIdx.x % warpSize;
        int wid = threadIdx.x / warpSize;

        count = warpReduceSum(count);     // Each warp performs partial reduction

        if (lane==0) shared[wid]=count; // Write reduced value to shared memory

        __syncthreads();              // Wait for all partial reductions

        //read from shared memory only if that warp existed
        count = (threadIdx.x < blockDim.x / warpSize) ? shared[lane] : 0;

        if (wid==0) {
            count = warpReduceSum(count); //Final reduce within first warp
            if (lane==0) atomicAdd(&frontier, count);
        }
#endif
    }
};

template<typename BFSVariant>
__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPushBfs<BFS<normal>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<BFS<bulk>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<BFS<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<BFS<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPullBfs<BFS<normal>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<BFS<bulk>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<BFS<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<BFS<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
vertexPushWarpBfs
(size_t, size_t, CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPushWarpBfs<BFS<normal>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<BFS<bulk>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<BFS<warpreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<BFS<blockreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
edgeListBfs<BFS<normal>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<BFS<bulk>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<BFS<warpreduce>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<BFS<blockreduce>>(EdgeList<unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
revEdgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
revEdgeListBfs<BFS<normal>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<BFS<bulk>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<BFS<warpreduce>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<BFS<blockreduce>>(EdgeList<unsigned> *, int *, int);
#endif
