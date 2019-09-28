#include "bfs.hpp"

template<typename BFSVariant>
__global__ void
vertexPushBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t startIdx = blockIdx.x * blockDim.x + threadIdx.x;
    uint64_t size = graph->vertex_count;
    BFSVariant bfs;
    unsigned newDepth = depth + 1;
    for (uint64_t idx = startIdx; idx < size; idx += blockDim.x * gridDim.x)
    {
        if (levels[idx] == depth) {
            unsigned *vertices = graph->vertices;
            unsigned start = vertices[idx];
            unsigned end = vertices[idx + 1];

            for (unsigned i = start; i < end; i++) {
                if (atomicMin(&levels[graph->edges[i]], newDepth) > newDepth) {
                    bfs.update();
                }
            }
        }
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
vertexPushBfs<Reduction<normal>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushBfs<Reduction<bulk>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushBfs<Reduction<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPushBfs<Reduction<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);
#endif
