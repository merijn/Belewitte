#include "bfs.hpp"

template<typename BFSVariant>
__global__ void
vertexPullBfs(CSR<unsigned,unsigned> *graph, int *levels, int depth)
{
    uint64_t size = graph->vertex_count;
    int newDepth = depth + 1;
    BFSVariant bfs;

    for (uint64_t idx = blockIdx.x * blockDim.x + threadIdx.x;
        idx < size && levels[idx] > newDepth;
        idx += blockDim.x * gridDim.x)
    {
        unsigned *reverse_vertices = graph->vertices;
        unsigned start = reverse_vertices[idx];
        unsigned end = reverse_vertices[idx + 1];

        unsigned *reverse_edges = graph->edges;

        for (unsigned i = start; i < end; i++) {
            if (levels[reverse_edges[i]] == depth) {
                levels[idx] = newDepth;
                bfs.update();
                break;
            }
        }
    }
    bfs.finalise();
}

#ifndef __APPLE__
template __global__ void
vertexPullBfs<Reduction<normal>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullBfs<Reduction<bulk>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullBfs<Reduction<warpreduce>>(CSR<unsigned,unsigned> *, int *, int);

template __global__ void
vertexPullBfs<Reduction<blockreduce>>(CSR<unsigned,unsigned> *, int *, int);
#endif
