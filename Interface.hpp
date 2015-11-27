#ifndef __INTERFACE_HPP__
#define __INTERFACE_HPP__

#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "Backend.hpp"
#include "Graph.hpp"

enum class prop_type { edge, vertex };

template<typename V>
class property {
    const prop_type type;

    public:
        property(prop_type pType) : type(pType) {}

        template<typename T>
        T get_size(T vertex_count, T edge_count)
        { return type == prop_type::edge ? edge_count : vertex_count; }
};

template<typename K, typename... Graph>
struct Bind {
    K kern;
    size_t dim;
    std::vector<size_t> blockSizes;
    std::vector<size_t> gridSizes;
    size_t sharedMem;
    std::tuple<Graph...> graph;

    Bind(K k, size_t d, std::vector<size_t> blocks,
         std::vector<size_t> grids, size_t shared, Graph... g)
        : kern(k)
        , dim(d)
        , blockSizes(blocks)
        , gridSizes(grids)
        , sharedMem(shared)
        , graph(g...)
    {}

    template<typename Platform, typename... Args, size_t... I>
    void call_(Backend<Platform>& backend, std::index_sequence<I...>, Args... args)
    {
        backend.setWorkSizes(dim, blockSizes, gridSizes, sharedMem);
        backend.runKernel(static_cast<typename kernel<Platform, Graph..., Args...>::type>(kern), std::get<I>(graph)..., args...);
    }

    template<typename Platform, typename... Args>
    void call(Backend<Platform>& backend, Args... args)
    {
        call_(backend, std::index_sequence_for<Graph...>{}, args...);
    }
};

template<typename K, typename... Graph>
static Bind<K, Graph...> makeBind(K k, size_t d, std::vector<size_t> blocks,
                std::vector<size_t> grids, size_t shared, Graph... g)
{
    return Bind<K, Graph...>(k, d, blocks, grids, shared, g...);
}

template<typename Platform, typename V, typename E>
std::tuple
< std::shared_ptr<alloc_t<V>>
, std::shared_ptr<alloc_t<E>>
, std::shared_ptr<alloc_t<E>>
>
loadEdgeListCSR(Backend<Platform> &p, const Graph<V,E> &graph)
{
    auto vertices = p.template allocConstant<V>(graph.vertices.size);
    auto edges_in = p.template allocConstant<E>(graph.edges.size);
    auto edges_out = p.template allocConstant<E>(graph.edges.size);

    size_t edge = 0;

    for (E i = 0; i < graph.vertex_count; i++) {
        for (size_t j = graph.raw_vertices[i]; j < graph.raw_vertices[i+1]; j++) {
            (*edges_in)[edge] = i;
            (*edges_out)[edge] = graph.raw_edges[j];
            edge++;
        }
        (*vertices)[i] = graph.raw_vertices[i];
    }

    (*vertices)[graph.vertex_count] = graph.raw_vertices[graph.vertex_count];

    return std::make_tuple(vertices, edges_in, edges_out);
}

template<typename Platform, typename V, typename E>
std::tuple
< std::shared_ptr<alloc_t<V>>
, std::shared_ptr<alloc_t<Edge<E>>>
>
loadStructEdgeListCSR(Backend<Platform> &p, const Graph<V,E> &graph)
{
    auto vertices = p.template allocConstant<V>(graph.vertices.size);
    auto edges = p.template allocConstant<Edge<E>>(graph.edges.size);

    size_t edge = 0;

    for (E i = 0; i < graph.vertex_count; i++) {
        for (size_t j = graph.raw_vertices[i]; j < graph.raw_vertices[i+1]; j++) {
            (*edges)[edge].in = i;
            (*edges)[edge].out = graph.raw_edges[j];
            edge++;
        }
        (*vertices)[i] = graph.raw_vertices[i];
    }

    (*vertices)[graph.vertex_count] = graph.raw_vertices[graph.vertex_count];

    return std::make_tuple(vertices, edges);
}

template<typename Platform, typename V, typename E>
std::tuple
< std::shared_ptr<alloc_t<V>>
, std::shared_ptr<alloc_t<E>>
>
loadCSR(Backend<Platform> &p, const Graph<V,E> &graph)
{
    auto nodes = p.template allocConstant<V>(graph.vertices.size);
    auto edges = p.template allocConstant<E>(graph.edges.size);

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        (*nodes)[i] = graph.raw_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_edges.size; i++) {
        (*edges)[i] = graph.raw_edges[i];
    }

    return std::make_tuple(nodes, edges);
}

template<typename Platform, typename V, typename E>
std::tuple
< std::shared_ptr<alloc_t<V>>
, std::shared_ptr<alloc_t<E>>
, std::shared_ptr<alloc_t<V>>
>
loadReverseCSR(Backend<Platform> &p, const Graph<V,E> &graph)
{
    auto rev_nodes = p.template allocConstant<V>(graph.raw_rev_vertices.size);
    auto rev_edges = p.template allocConstant<E>(graph.raw_rev_edges.size);
    auto nodes = p.template allocConstant<V>(graph.vertices.size);

    for (size_t i = 0; i < graph.raw_rev_vertices.size; i++) {
        (*rev_nodes)[i] = graph.raw_rev_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_rev_edges.size; i++) {
        (*rev_edges)[i] = graph.raw_rev_edges[i];
    }

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        (*nodes)[i] = graph.raw_vertices[i];
    }

    return std::make_tuple(rev_nodes, rev_edges, nodes);
}
#endif
