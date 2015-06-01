#ifndef __BFS_H__
#define __BFS_H__

#include <cuda_runtime.h>

void resetFinished();
bool getFinished();

template<int chunk_size>
struct warp_mem_t {
    int levels[chunk_size];
    int nodes[chunk_size + 1];
};

__global__ void setArray(int *array, size_t size, int val);
__global__ void set_root(int *depths, int root);

template<size_t,size_t>
__global__ void cudabfs(int *nodes, int *edges, int N, int *levels, int curr);
#endif
