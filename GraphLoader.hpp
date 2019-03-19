#ifndef GRAPHLOADER_HPP
#define GRAPHLOADER_HPP

#include "utils/Graph.hpp"
#include "Backend.hpp"
#include "GraphRep.hpp"

enum class Rep : char
{ VertexCount
, EdgeCount
, EdgeList
, StructEdgeList
, EdgeListCSR
, StructEdgeListCSR
, CSR
, InverseVertexCSR
};

enum class Dir : char
{ Forward, Reverse };

struct GraphRep
{
    Rep representation;
    Dir direction;
};

inline std::ostream& operator<<(std::ostream& os, const Rep& p)
{
    switch (p) {
      case Rep::VertexCount:
        os << "VertexCount";
        break;
      case Rep::EdgeCount:
        os << "EdgeCount";
        break;
      case Rep::EdgeList:
        os << "EdgeList";
        break;
      case Rep::StructEdgeList:
        os << "StructEdgeList";
        break;
      case Rep::EdgeListCSR:
        os << "EdgeListCSR";
        break;
      case Rep::StructEdgeListCSR:
        os << "StructEdgeListCSR";
        break;
      case Rep::CSR:
        os << "CSR";
        break;
      case Rep::InverseVertexCSR:
        os << "InverseVertexCSR";
        break;
    }
    return os;
}

inline std::ostream& operator<<(std::ostream& os, const Dir& p)
{
    switch (p) {
      case Dir::Forward:
        os << "Forward";
        break;
      case Dir::Reverse:
        os << "Reverse";
        break;
    }
    return os;
}

template<Rep rep, typename Platform, typename V, typename E>
struct LoaderRep;

template
<typename Platform, typename V, typename E>
class GraphLoader
{
    template<Rep, typename, typename, typename>
    friend struct LoaderRep;

    template<typename T>
    using pair = std::tuple<T,T>;

    template<typename T>
    using alloc_t = typename Platform::template alloc_t<T>;

    template<Rep rep>
    using LoaderRep = LoaderRep<rep,Platform,V,E>;

    template<template<Rep> class F, typename... Args>
    void runWithGraphRep(GraphRep rep, Args&... args)
    {
        switch (rep.representation) {
            case Rep::VertexCount:
                F<Rep::VertexCount>::call(rep.direction, args...);
                break;
            case Rep::EdgeCount:
                F<Rep::EdgeCount>::call(rep.direction, args...);
                break;
            case Rep::EdgeList:
                F<Rep::EdgeList>::call(rep.direction, args...);
                break;
            case Rep::StructEdgeList:
                F<Rep::StructEdgeList>::call(rep.direction, args...);
                break;
            case Rep::EdgeListCSR:
                F<Rep::EdgeListCSR>::call(rep.direction, args...);
                break;
            case Rep::StructEdgeListCSR:
                F<Rep::StructEdgeListCSR>::call(rep.direction, args...);
                break;
            case Rep::CSR:
                F<Rep::CSR>::call(rep.direction, args...);
                break;
            case Rep::InverseVertexCSR:
                F<Rep::InverseVertexCSR>::call(rep.direction, args...);
                break;
        }
    }

    class RawData {
        Platform& p;

        pair<alloc_t<V>> vertices;
        pair<alloc_t<struct edge<E>>> struct_edges;
        pair<alloc_t<E>> in_edges;
        pair<alloc_t<E>> out_edges;

        template<int n>
        void
        loadData(const Accessor<V>& raw_vertices, const Accessor<E>& raw_edges)
        {
            std::get<n>(struct_edges) = p.template
                allocConstant<struct edge<E>>(edge_count);
            std::get<n>(vertices) = p.template
                allocConstant<V>(vertex_count + 1);
            std::get<n>(in_edges) = p.template allocConstant<E>(edge_count);
            std::get<n>(out_edges) = p.template allocConstant<E>(edge_count);

            for (E i = 0; i < vertex_count; i++) {
                std::get<n>(vertices)[i] = raw_vertices[i];
                for (size_t j = raw_vertices[i]; j < raw_vertices[i+1]; j++) {
                    std::get<n>(struct_edges)[j].in = i;
                    std::get<n>(in_edges)[j] = i;

                    std::get<n>(struct_edges)[j].out = raw_edges[j];
                    std::get<n>(out_edges)[j] = raw_edges[j];
                }
            }
            std::get<n>(vertices)[vertex_count] = raw_vertices[vertex_count];
        }

      public:
        size_t vertex_count, edge_count;

        RawData(const Graph<V,E>& graph) : p(Platform::get())
        {
            vertex_count = graph.vertex_count;
            edge_count = graph.edge_count;
            loadData<0>(graph.raw_vertices, graph.raw_edges);
            loadData<1>(graph.raw_rev_vertices, graph.raw_rev_edges);
        }

        void load(alloc_t<EdgeList<E>>& dest, Dir dir)
        {
            if (!dest) {
                dest = p.template allocConstant<EdgeList<E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->inEdges, get(in_edges, dir));
                dest.registerLocalAlloc(&dest->outEdges, get(out_edges, dir));
            }
        }

        void load(alloc_t<StructEdgeList<E>>& dest, Dir dir)
        {
            if (!dest) {
                dest = p.template allocConstant<StructEdgeList<E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->edges, get(struct_edges, dir));
            }
        }

        void load(alloc_t<EdgeListCSR<V,E>>& dest, Dir dir)
        {
            if (!dest) {
                dest = p.template allocConstant<EdgeListCSR<V,E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->vertices, get(vertices, dir));
                dest.registerLocalAlloc(&dest->inEdges, get(in_edges, dir));
                dest.registerLocalAlloc(&dest->outEdges, get(out_edges, dir));
            }
        }

        void load(alloc_t<StructEdgeListCSR<V,E>>& dest, Dir dir)
        {
            if (!dest) {
                dest = p.template allocConstant<StructEdgeListCSR<V,E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->vertices, get(vertices, dir));
                dest.registerLocalAlloc(&dest->edges, get(struct_edges, dir));
            }
        }

        void load(alloc_t<CSR<V,E>>& dest, Dir dir)
        {
            if (!dest) {
                dest = p.template allocConstant<CSR<V,E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->vertices, get(vertices, dir));
                dest.registerLocalAlloc(&dest->edges, get(out_edges, dir));
            }
        }

        void load(alloc_t<InverseVertexCSR<V,E>>& dest, Dir dir)
        {
            Dir opp = dir == Dir::Forward ? Dir::Reverse : Dir::Forward;
            if (!dest) {
                dest = p.template allocConstant<InverseVertexCSR<V,E>>();
                dest->vertex_count = vertex_count;
                dest->edge_count = edge_count;
                dest.registerLocalAlloc(&dest->vertices, get(vertices, dir));
                dest.registerLocalAlloc(&dest->inverse_vertices,
                                        get(vertices, opp));
                dest.registerLocalAlloc(&dest->edges, get(out_edges, dir));
            }
        }
    };

    template<Rep rep>
    struct LoadGraph
    {
        static void
        call(Dir dir, GraphLoader& loader, RawData& data)
        {
            using GraphType = typename LoaderRep<rep>::GraphType;

            if constexpr (isDeviceAlloc<GraphType>()) {
                data.load(get(loader.*LoaderRep<rep>::field, dir), dir);
            }
        }
    };

    template<Rep rep>
    struct TransferGraph
    {
        static void
        call(Dir dir, GraphLoader& loader)
        {
            using GraphType = typename LoaderRep<rep>::GraphType;

            if constexpr (isDeviceAlloc<GraphType>()) {
                get(loader.*LoaderRep<rep>::field, dir).copyHostToDev();
            }
        }
    };

    template<typename T>
    static T&
    get(pair<T>& x, Dir dir)
    { return dir == Dir::Forward ? std::get<0>(x) : std::get<1>(x); }

    template<typename T>
    struct GraphType;

    template<typename T>
    struct GraphType<T GraphLoader::*>
    { typedef decltype(get(std::declval<T>(), Dir::Forward)) type; };

  public:
    GraphLoader() {}

    template<Rep rep, Dir dir>
    typename LoaderRep<rep>::GraphType&
    getGraph()
    { return get(this->*LoaderRep<rep>::field, dir); }

    std::pair<size_t,size_t>
    loadGraph(const Graph<V,E>& graph, GraphRep rep)
    { return loadGraph(graph, std::vector<GraphRep>{rep}); }

    std::pair<size_t,size_t>
    loadGraph(const Graph<V,E>& graph, const std::vector<GraphRep>& reps)
    {
        RawData data(graph);

        vertexCount = {data.vertex_count, data.vertex_count};
        edgeCount = {data.edge_count, data.edge_count};

        for (auto rep : reps) runWithGraphRep<LoadGraph>(rep, *this, data);

        return {data.vertex_count, data.edge_count};
    }

    void transferGraph(GraphRep rep)
    { runWithGraphRep<TransferGraph>(rep, *this); }

    void freeGraph()
    {
        std::get<0>(edgeList).free();
        std::get<1>(edgeList).free();
        std::get<0>(structEdgeList).free();
        std::get<1>(structEdgeList).free();
        std::get<0>(edgeListCSR).free();
        std::get<1>(edgeListCSR).free();
        std::get<0>(structEdgeListCSR).free();
        std::get<1>(structEdgeListCSR).free();
        std::get<0>(csr).free();
        std::get<1>(csr).free();
        std::get<0>(inverseVertexCSR).free();
        std::get<1>(inverseVertexCSR).free();
    }

  private:
    pair<size_t> vertexCount;
    pair<size_t> edgeCount;
    pair<alloc_t<EdgeList<E>>> edgeList;
    pair<alloc_t<StructEdgeList<E>>> structEdgeList;
    pair<alloc_t<EdgeListCSR<V,E>>> edgeListCSR;
    pair<alloc_t<StructEdgeListCSR<V,E>>> structEdgeListCSR;
    pair<alloc_t<CSR<V,E>>> csr;
    pair<alloc_t<InverseVertexCSR<V,E>>> inverseVertexCSR;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::VertexCount, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::vertexCount;
    using FieldType = decltype(std::get<0>(std::declval<Loader>().*field));
    typedef typename std::remove_reference<FieldType>::type GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::EdgeCount, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::edgeCount;
    using FieldType = decltype(std::get<0>(std::declval<Loader>().*field));
    typedef typename std::remove_reference<FieldType>::type GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::EdgeList, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::edgeList;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::StructEdgeList, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::structEdgeList;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::EdgeListCSR, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::edgeListCSR;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::StructEdgeListCSR, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::structEdgeListCSR;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::CSR, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::csr;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};

template<typename Platform, typename V, typename E>
struct LoaderRep<Rep::InverseVertexCSR, Platform, V, E>
{
    using Loader = GraphLoader<Platform,V,E>;
    static constexpr auto Loader::* field = &Loader::inverseVertexCSR;
    typedef decltype(std::get<0>(std::declval<Loader>().*field)) GraphType;
};
#endif
