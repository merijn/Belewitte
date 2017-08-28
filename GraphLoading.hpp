#ifndef GRAPHLOADING_HPP
#define GRAPHLOADING_HPP

#include "Graph.hpp"
#include "GraphRep.hpp"

namespace details {
template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<EdgeList<E>>
edgeList(Platform &p, const Accessor<V> &vertices, const Accessor<E> &edges)
{
    auto result = p.template allocConstant<EdgeList<E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = edges.size;
    result.allocLocal(&result->inEdges, edges.size);
    result.allocLocal(&result->outEdges, edges.size);

    size_t edge = 0;

    for (E i = 0; i < result->vertex_count; i++) {
        for (size_t j = vertices[i]; j < vertices[i+1]; j++) {
            result->inEdges[edge] = i;
            result->outEdges[edge] = edges[j];
            edge++;
        }
    }

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<StructEdgeList<E>>
structEdgeList(Platform &p, const Accessor<V> &vertices, const Accessor<E> &edges)
{
    auto result = p.template allocConstant<StructEdgeList<E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = edges.size;
    result.allocLocal(&result->edges, edges.size);

    size_t edge = 0;

    for (E i = 0; i < result->vertex_count; i++) {
        for (size_t j = vertices[i]; j < vertices[i+1]; j++) {
            result->edges[edge].in = i;
            result->edges[edge].out = edges[j];
            edge++;
        }
    }

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<EdgeListCSR<V,E>>
edgeListCSR(Platform &p, const Accessor<V> &vertices, const Accessor<E> &edges)
{
    auto result = p.template allocConstant<EdgeListCSR<V,E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = edges.size;
    result.allocLocal(&result->vertices, vertices.size);
    result.allocLocal(&result->inEdges, edges.size);
    result.allocLocal(&result->outEdges, edges.size);

    size_t edge = 0;

    for (E i = 0; i < result->vertex_count; i++) {
        for (size_t j = vertices[i]; j < vertices[i+1]; j++) {
            result->inEdges[edge] = i;
            result->outEdges[edge] = edges[j];
            edge++;
        }
        result->vertices[i] = vertices[i];
    }

    result->vertices[result->vertex_count] = vertices[result->vertex_count];

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<StructEdgeListCSR<V,E>>
structEdgeListCSR(Platform &p, const Accessor<V> &vertices, const Accessor<E> &edges)
{
    auto result = p.template allocConstant<StructEdgeListCSR<V,E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = edges.size;
    result.allocLocal(&result->vertices, vertices.size);
    result.allocLocal(&result->edges, edges.size);

    size_t edge = 0;

    for (E i = 0; i < result->vertex_count; i++) {
        for (size_t j = vertices[i]; j < vertices[i+1]; j++) {
            result->edges[edge].in = i;
            result->edges[edge].out = edges[j];
            edge++;
        }
        result->vertices[i] = vertices[i];
    }

    result->vertices[result->vertex_count] = vertices[result->vertex_count];

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<CSR<V,E>>
csr(Platform &p, const Accessor<V> &vertices, const Accessor<E> &edges)
{
    auto result = p.template allocConstant<CSR<V,E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = edges.size;
    result.allocLocal(&result->vertices, vertices.size);
    result.allocLocal(&result->edges, edges.size);

    for (size_t i = 0; i < vertices.size; i++) {
        result->vertices[i] = vertices[i];
    }

    for (size_t i = 0; i < edges.size; i++) {
        result->edges[i] = edges[i];
    }

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<ReversedCSR<V,E>>
reversedCSR(Platform &p, const Accessor<V> &vertices, const Accessor<V> &rev_vertices, const Accessor<E> &rev_edges)
{
    auto result = p.template allocConstant<ReversedCSR<V,E>>();
    result->vertex_count = vertices.size - 1;
    result->edge_count = rev_edges.size;
    result.allocLocal(&result->vertices, vertices.size);
    result.allocLocal(&result->reverse_vertices, rev_vertices.size);
    result.allocLocal(&result->reverse_edges, rev_edges.size);

    for (size_t i = 0; i < vertices.size; i++) {
        result->vertices[i] = vertices[i];
    }

    for (size_t i = 0; i < rev_vertices.size; i++) {
        result->reverse_vertices[i] = rev_vertices[i];
    }

    for (size_t i = 0; i < rev_edges.size; i++) {
        result->reverse_edges[i] = rev_edges[i];
    }

    return result;
}
}

template<typename Platform, typename V, typename E>
auto
loadEdgeList(Platform &p, const Graph<V,E> &graph)
{ return details::edgeList(p, graph.raw_vertices, graph.raw_edges); }

template<typename Platform, typename V, typename E>
auto
loadReverseEdgeList(Platform &p, const Graph<V,E> &graph)
{ return details::edgeList(p, graph.raw_rev_vertices, graph.raw_rev_edges); }

template<typename Platform, typename V, typename E>
auto
loadStructEdgeList(Platform &p, const Graph<V,E> &graph)
{ return details::structEdgeList(p, graph.raw_vertices, graph.raw_edges); }

template<typename Platform, typename V, typename E>
auto
loadReverseStructEdgeList(Platform &p, const Graph<V,E> &graph)
{ return details::structEdgeList(p, graph.raw_rev_vertices, graph.raw_rev_edges); }

template<typename Platform, typename V, typename E>
auto
loadEdgeListCSR(Platform &p, const Graph<V,E> &graph)
{ return details::edgeListCSR(p, graph.raw_vertices, graph.raw_edges); }

template<typename Platform, typename V, typename E>
auto
loadReverseEdgeListCSR(Platform &p, const Graph<V,E> &graph)
{ return details::edgeListCSR(p, graph.raw_rev_vertices, graph.raw_rev_edges); }

template<typename Platform, typename V, typename E>
auto
loadStructEdgeListCSR(Platform &p, const Graph<V,E> &graph)
{ return details::structEdgeListCSR(p, graph.raw_vertices, graph.raw_edges); }

template<typename Platform, typename V, typename E>
auto
loadReverseStructEdgeListCSR(Platform &p, const Graph<V,E> &graph)
{ return details::structEdgeListCSR(p, graph.raw_rev_vertices, graph.raw_rev_edges); }

template<typename Platform, typename V, typename E>
auto
loadCSR(Platform &p, const Graph<V,E> &graph)
{ return details::csr(p, graph.raw_vertices, graph.raw_edges); }

template<typename Platform, typename V, typename E>
auto
loadReverseCSR(Platform &p, const Graph<V,E> &graph)
{ return details::csr(p, graph.raw_rev_vertices, graph.raw_rev_edges); }

template<typename Platform, typename V, typename E>
auto
loadReversedCSR(Platform &p, const Graph<V,E> &graph)
{ return details::reversedCSR(p, graph.raw_vertices, graph.raw_rev_vertices, graph.raw_rev_edges); }
#endif
