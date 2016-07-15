#ifndef __GRAPHREP_HPP__
#define __GRAPHREP_HPP__

#include <cstdint>

#ifndef __OPENCL_VERSION__
template<typename EDGE>
#endif
struct edge {
  EDGE in, out;

#ifndef __OPENCL_VERSION__
  edge(EDGE i, EDGE o) : in(i), out(o) {}
#endif
};

#ifndef __OPENCL_VERSION__
template<typename VERTEX, typename EDGE>
#endif
struct EdgeListCSR {
    uint64_t vertex_count, edge_count;

    VERTEX *vertices;
    EDGE *inEdges;
    EDGE *outEdges;
};

#ifndef __OPENCL_VERSION__
template<typename VERTEX, typename EDGE>
#endif
struct StructEdgeListCSR {
    uint64_t vertex_count, edge_count;

    VERTEX *vertices;
#ifdef __OPENCL_VERSION__
    edge *edges;
#else
    edge<EDGE> *edges;
#endif
};

#ifndef __OPENCL_VERSION__
template<typename VERTEX, typename EDGE>
#endif
struct CSR {
    uint64_t vertex_count, edge_count;

    VERTEX *vertices;
    EDGE *edges;
};

#ifndef __OPENCL_VERSION__
template<typename VERTEX, typename EDGE>
#endif
struct ReverseCSR {
    uint64_t vertex_count, edge_count;

    VERTEX *reverse_vertices;
    VERTEX *vertices;
    EDGE *reverse_edges;
};
#endif
