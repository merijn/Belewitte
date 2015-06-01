#ifndef __INTERFACE_HPP__
#define __INTERFACE_HPP__

#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "Backend.hpp"
#include "GraphFile.hpp"

struct edge {
    int in, out;
};

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

template<typename Platform>
std::tuple
< std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<int>>
>
loadEdgeListCSR(Backend<Platform> &p, const GraphFile &graph)
{
    auto vertices = p.template allocConstant<int>(graph.vertices.size);
    auto edges_in = p.template allocConstant<int>(graph.edges.size);
    auto edges_out = p.template allocConstant<int>(graph.edges.size);

    size_t edge = 0;

    for (int i = 0; i < graph.vertex_count; i++) {
        for (int j = graph.vertices[i]; j < graph.vertices[i+1]; j++) {
            (*edges_in)[edge] = i;
            (*edges_out)[edge] = graph.edges[j];
            edge++;
        }
        (*vertices)[i] = graph.vertices[i];
    }

    (*vertices)[graph.vertex_count] = graph.vertices[graph.vertex_count];

    return std::make_tuple(vertices, edges_in, edges_out);
}

template<typename Platform>
std::tuple
< std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<edge>>
>
loadStructEdgeListCSR(Backend<Platform> &p, const GraphFile &graph)
{
    auto vertices = p.template allocConstant<int>(graph.vertices.size);
    auto edges = p.template allocConstant<edge>(graph.edges.size);

    size_t edge = 0;

    for (int i = 0; i < graph.vertex_count; i++) {
        for (int j = graph.vertices[i]; j < graph.vertices[i+1]; j++) {
            (*edges)[edge].in = i;
            (*edges)[edge].out = graph.edges[j];
            edge++;
        }
        (*vertices)[i] = graph.vertices[i];
    }

    (*vertices)[graph.vertex_count] = graph.vertices[graph.vertex_count];

    return std::make_tuple(vertices, edges);
}

template<typename Platform>
std::tuple
< std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<int>>
>
loadCSR(Backend<Platform> &p, const GraphFile &graph)
{
    auto nodes = p.template allocConstant<int>(graph.vertices.size);
    auto edges = p.template allocConstant<int>(graph.edges.size);

    for (size_t i = 0; i < graph.vertices.size; i++) {
        (*nodes)[i] = graph.vertices[i];
    }

    for (size_t i = 0; i < graph.edges.size; i++) {
        (*edges)[i] = graph.edges[i];
    }

    return std::make_tuple(nodes, edges);
}

template<typename Platform>
std::tuple
< std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<int>>
, std::shared_ptr<alloc_t<int>>
>
loadReverseCSR(Backend<Platform> &p, const GraphFile &graph)
{
    auto rev_nodes = p.template allocConstant<int>(graph.rev_vertices.size);
    auto rev_edges = p.template allocConstant<int>(graph.rev_edges.size);
    auto nodes = p.template allocConstant<int>(graph.vertices.size);

    for (size_t i = 0; i < graph.rev_vertices.size; i++) {
        (*rev_nodes)[i] = graph.rev_vertices[i];
    }

    for (size_t i = 0; i < graph.rev_edges.size; i++) {
        (*rev_edges)[i] = graph.rev_edges[i];
    }

    for (size_t i = 0; i < graph.vertices.size; i++) {
        (*nodes)[i] = graph.vertices[i];
    }

    return std::make_tuple(rev_nodes, rev_edges, nodes);
}
#endif
