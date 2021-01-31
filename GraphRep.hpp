#ifndef GRAPHREP_HPP
#define GRAPHREP_HPP

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
template<typename EDGE>
#endif
struct EdgeList {
    uint64_t vertex_count, edge_count;

    EDGE *inEdges;
    EDGE *outEdges;
};

#ifndef __OPENCL_VERSION__
template<typename EDGE>
#endif
struct StructEdgeList {
    uint64_t vertex_count, edge_count;

#ifdef __OPENCL_VERSION__
    edge *edges;
#else
    edge<EDGE> *edges;
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
#endif
