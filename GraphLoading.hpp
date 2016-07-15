#ifndef __GRAPHLOADING_HPP__
#define __GRAPHLOADING_HPP__

#include "Graph.hpp"
#include "GraphRep.hpp"

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<EdgeListCSR<V,E>>
loadEdgeListCSR(Platform &p, Graph<V,E> &graph)
{
    auto result = p.template allocConstant<EdgeListCSR<V,E>>();
    result->vertex_count = graph.vertex_count;
    result->edge_count = graph.edge_count;
    result.allocLocal(&result->vertices, graph.raw_vertices.size);
    result.allocLocal(&result->inEdges, graph.raw_edges.size);
    result.allocLocal(&result->outEdges, graph.raw_edges.size);

    size_t edge = 0;

    for (E i = 0; i < graph.vertex_count; i++) {
        for (size_t j = graph.raw_vertices[i]; j < graph.raw_vertices[i+1]; j++) {
            result->inEdges[edge] = i;
            result->outEdges[edge] = graph.raw_edges[j];
            edge++;
        }
        result->vertices[i] = graph.raw_vertices[i];
    }

    result->vertices[graph.vertex_count] = graph.raw_vertices[graph.vertex_count];

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<StructEdgeListCSR<V,E>>
loadStructEdgeListCSR(Platform &p, Graph<V,E> &graph)
{
    auto result = p.template allocConstant<StructEdgeListCSR<V,E>>();
    result->vertex_count = graph.vertex_count;
    result->edge_count = graph.edge_count;
    result.allocLocal(&result->vertices, graph.raw_vertices.size);
    result.allocLocal(&result->edges, graph.raw_edges.size);

    size_t edge = 0;

    for (E i = 0; i < graph.vertex_count; i++) {
        for (size_t j = graph.raw_vertices[i]; j < graph.raw_vertices[i+1]; j++) {
            result->edges[edge].in = i;
            result->edges[edge].out = graph.raw_edges[j];
            edge++;
        }
        result->vertices[i] = graph.raw_vertices[i];
    }

    result->vertices[graph.vertex_count] = graph.raw_vertices[graph.vertex_count];

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<CSR<V,E>>
loadCSR(Platform &p, Graph<V,E> &graph)
{
    auto result = p.template allocConstant<CSR<V,E>>();
    result->vertex_count = graph.vertex_count;
    result->edge_count = graph.edge_count;
    result.allocLocal(&result->vertices, graph.raw_vertices.size);
    result.allocLocal(&result->edges, graph.raw_edges.size);

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        result->vertices[i] = graph.raw_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_edges.size; i++) {
        result->edges[i] = graph.raw_edges[i];
    }

    return result;
}

template
< typename Platform, typename V, typename E
, template<typename> class alloc_t = Platform::template alloc_t
>
alloc_t<ReverseCSR<V,E>>
loadReverseCSR(Platform &p, Graph<V,E> &graph)
{
    auto result = p.template allocConstant<ReverseCSR<V,E>>();
    result->vertex_count = graph.vertex_count;
    result->edge_count = graph.edge_count;
    result.allocLocal(&result->reverse_vertices, graph.raw_rev_vertices.size);
    result.allocLocal(&result->vertices, graph.raw_vertices.size);
    result.allocLocal(&result->reverse_edges, graph.raw_edges.size);

    for (size_t i = 0; i < graph.raw_rev_vertices.size; i++) {
        result->reverse_vertices[i] = graph.raw_rev_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_rev_edges.size; i++) {
        result->reverse_edges[i] = graph.raw_rev_edges[i];
    }

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        result->vertices[i] = graph.raw_vertices[i];
    }

    return result;
}
#endif
