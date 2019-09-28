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
        val += __shfl_down_sync(0xffffffff, val, offset);
     }

    return val;
}
#endif

template<bfs_variant variant>
struct Reduction {
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
struct Reduction<bulk> {
    static constexpr const char * suffix = "-bulk";
    unsigned count;

    __device__ Reduction<bulk>() : count(0U) {}

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
struct Reduction<warpreduce> {
    static constexpr const char * suffix = "-warpreduce";
    unsigned count;

    __device__ Reduction<warpreduce>() : count(0U) {}

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
struct Reduction<blockreduce> {
    static constexpr const char * suffix = "-blockreduce";
    unsigned count;

    __device__ Reduction<blockreduce>() : count(0U) {}

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
vertexPushBfs<Reduction<normal>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<Reduction<bulk>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<Reduction<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushBfs<Reduction<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPullBfs<Reduction<normal>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<Reduction<bulk>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<Reduction<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullBfs<Reduction<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
vertexPushWarpBfs
(size_t, size_t, CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPushWarpBfs<Reduction<normal>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<Reduction<bulk>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<Reduction<warpreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPushWarpBfs<Reduction<blockreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
vertexPullWarpBfs
(size_t, size_t, CSR<unsigned,unsigned> *graph, int *levels, int depth);

extern template __global__ void
vertexPullWarpBfs<Reduction<normal>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullWarpBfs<Reduction<bulk>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullWarpBfs<Reduction<warpreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

extern template __global__ void
vertexPullWarpBfs<Reduction<blockreduce>>
(size_t, size_t, CSR<unsigned,unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
edgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
edgeListBfs<Reduction<normal>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<Reduction<bulk>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<Reduction<warpreduce>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
edgeListBfs<Reduction<blockreduce>>(EdgeList<unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
structEdgeListBfs(StructEdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
structEdgeListBfs<Reduction<normal>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
structEdgeListBfs<Reduction<bulk>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
structEdgeListBfs<Reduction<warpreduce>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
structEdgeListBfs<Reduction<blockreduce>>(StructEdgeList<unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
revStructEdgeListBfs(StructEdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
revStructEdgeListBfs<Reduction<normal>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
revStructEdgeListBfs<Reduction<bulk>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
revStructEdgeListBfs<Reduction<warpreduce>>(StructEdgeList<unsigned> *, int *, int);

extern template __global__ void
revStructEdgeListBfs<Reduction<blockreduce>>(StructEdgeList<unsigned> *, int *, int);

template<typename BFSVariant>
__global__ void
revEdgeListBfs(EdgeList<unsigned> *graph, int *levels, int depth);

extern template __global__ void
revEdgeListBfs<Reduction<normal>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<Reduction<bulk>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<Reduction<warpreduce>>(EdgeList<unsigned> *, int *, int);

extern template __global__ void
revEdgeListBfs<Reduction<blockreduce>>(EdgeList<unsigned> *, int *, int);
#endif
