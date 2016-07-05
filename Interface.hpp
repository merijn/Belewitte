#ifndef __INTERFACE_HPP__
#define __INTERFACE_HPP__

#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "Backend.hpp"
#include "Graph.hpp"
#include "Options.hpp"
#include "Timer.hpp"
#include "WarpDispatch.hpp"

struct AlgorithmConfig {
  protected:
    AlgorithmConfig(const Options& opts, size_t count, std::string out)
     : options(opts, true), run_count(count), outputFile(out)
    {}

    AlgorithmConfig(const AlgorithmConfig& o)
     : options(o.options), run_count(o.run_count), outputFile(o.outputFile)
    {}

    virtual ~AlgorithmConfig();

    Options options;
    size_t run_count;
    std::string outputFile;

    virtual void transferGraph(const std::string) = 0;
    virtual void runImplementation() = 0;

  public:
    void operator()(const std::string filename)
    {
        transferGraph(filename);
        runImplementation();
    }

    void help(std::ostream& out, std::string prefix)
    { options.usage(out, prefix); }

    std::vector<char*> setup(std::vector<char*> args)
    { return options.parseArgsFinal(args); }
};

enum class work_division { nodes, edges };

template
< typename Platform
, typename Kernel
, typename V
, typename E
, typename... GraphData
>
struct TemplateConfig : public AlgorithmConfig {
  using Vertex = V;
  using Edge = E;
  using GraphTuple = std::tuple<GraphData...>;
  using LoadFun = std::function<GraphTuple(Platform&, Graph<V,E>&)>;
  using SharedMemFun = std::function<size_t(size_t)>;
  using Pair = std::pair<size_t,size_t>;

  protected:
    size_t vertex_count, edge_count;
    Pair nodeDivision, edgeDivision;
    SharedMemFun getSharedMemSize;
    Platform &backend;
    Kernel kernel;

    LoadFun load;
    work_division workDivision;
    GraphTuple graphData;

    TemplateConfig
        ( const Options& opts
        , size_t count
        , std::string outputFile
        , LoadFun loadFun
        , work_division division
        , Kernel kern
        )
    : AlgorithmConfig(opts, count, outputFile)
    , getSharedMemSize([](auto) { return 0; }), backend(Platform::get())
    , kernel(kern), load(loadFun), workDivision(division)
    {}

    template<typename... Args>
    void transferData(Args...) {}

    void transferData() {}

    template<typename T, typename... Args>
    void transferData(alloc_t<T> data, Args... args)
    {
        data->copyHostToDev();
        transferData(args...);
    }

    template<typename T, typename... Args>
    void transferData(T, Args... args)
    { transferData(args...); }

    template<size_t... I>
    void transferGraph(const std::string filename, std::index_sequence<I...>)
    {
        Timer graphTransfer("graphTransfer", run_count);

        auto graph = Graph<V,E>(filename);
        graphData = load(backend, graph);

        vertex_count = graph.vertex_count;
        edge_count = graph.edge_count;

        nodeDivision = backend.computeDivision(vertex_count);
        edgeDivision = backend.computeDivision(edge_count);

        graphTransfer.start();
        transferData(std::get<I>(graphData)...);
        graphTransfer.stop();
    }

    template<typename K, typename... Args, size_t... I>
    void run(std::index_sequence<I...>, K kern, Args... args)
    { backend.runKernel(kern, std::get<I>(graphData)..., args...); }

  public:
    virtual void transferGraph(const std::string filename) override
    { transferGraph(filename, std::index_sequence_for<GraphData...>()); }

    void setKernelConfig()
    {
        switch (workDivision) {
            case work_division::nodes:
                backend.setWorkSizes
                    ( 1
                    , {nodeDivision.first}
                    , {nodeDivision.second}
                    , getSharedMemSize(nodeDivision.first));
                break;
            case work_division::edges:
                backend.setWorkSizes
                    ( 1
                    , {edgeDivision.first}
                    , {edgeDivision.second}
                    , getSharedMemSize(edgeDivision.first));
                break;
        }
    }

    template<typename K, typename... Args>
    void runKernel(K kern, Args... args)
    { run(std::index_sequence_for<GraphData...>(), kern, args...); }
};

template<typename Kernel>
struct Warp_Settings {
    Kernel kernel;
    std::function<size_t(size_t)> getSharedMemSize;

    Warp_Settings(Kernel k, size_t warpSize, size_t workSize)
     : kernel(k)
    , getSharedMemSize([warpSize,workSize](size_t workPerWarp)
      { return workPerWarp / warpSize * workSize; })
    {}
};

template<typename Kernel>
Warp_Settings<Kernel>
WarpSettings(Kernel k, size_t warpSize, size_t workSize)
{ return Warp_Settings<Kernel>(k, warpSize, workSize); }

template<typename... Args>
struct ArgCapture {
template
< template<size_t,size_t> class Warp
, template<typename,typename,typename...> class BaseConfig
, typename Platform
, typename Kernel
, typename... GraphData
>
struct WarpConfig : public BaseConfig<Platform,Kernel,Args...,GraphData...> {
    using Vertex = typename WarpConfig::Vertex;
    using Edge = typename WarpConfig::Edge;
    using LoadFun = typename WarpConfig::LoadFun;
    using Config = BaseConfig<Platform,Kernel,Args...,GraphData...>;
    using Config::options;

    WarpConfig
        ( const Options& opts
        , size_t count
        , std::string outputFile
        , LoadFun loadFun
        , work_division d
        , warp_dispatch<Warp>
        , Args... args
        )
    : Config(opts, count, outputFile, loadFun, d, nullptr, args...)
    , warp_size(32), chunk_size(32)
    {
        options.add('w', "warp", "NUM", warp_size,
                    "Virtual warp size for warp variants.")
               .add('c', "chunk", "NUM", chunk_size,
                    "Work chunk size for warp variants.");
    }

    void runImplementation() override
    {
        auto warpSettings = warp_dispatch<Warp>::work(warp_size, chunk_size);
        this->kernel = warpSettings.kernel;
        this->getSharedMemSize = warpSettings.getSharedMemSize;
        Config::runImplementation();
    }

  private:
    size_t warp_size, chunk_size;
};
};

template
< template<typename, typename, typename...> class Cfg
, typename Platform
, typename Kernel
, typename... Args
, typename... GraphData
, typename Config = Cfg<Platform, Kernel, Args..., GraphData...>
, typename Vertex = typename Config::Vertex
, typename Edge = typename Config::Edge
>
Config* make_config
    ( const Options& opts
    , size_t count
    , std::string outputFile
    , std::tuple<GraphData...>(*l)(Backend<Platform>&, Graph<Vertex,Edge>&)
    , work_division w
    , Kernel kern
    , Args... args
    )
{ return new Config(opts, count, outputFile, l, w, kern, args...); }

template
< template<typename, typename, typename...> class Cfg
, template<size_t,size_t> class Warp
, typename Platform
, typename... Args
, typename... GraphData
, typename Kernel = decltype(warp_dispatch<Warp>::work(0,0).kernel)
, typename Config = typename ArgCapture<Args...>::
                    template WarpConfig<Warp,Cfg,Platform,Kernel,GraphData...>
, typename Vertex = typename Config::Vertex
, typename Edge = typename Config::Edge
>
Config* make_warp_config
    ( const Options& opts
    , size_t count
    , std::string outputFile
    , std::tuple<GraphData...>(*l)(Backend<Platform>&, Graph<Vertex,Edge>&)
    , work_division w
    , warp_dispatch<Warp> d
    , Args... args
    )
{ return new Config(opts, count, outputFile, l, w, d, args...); }

template<typename Platform, typename V, typename E>
std::tuple<size_t, size_t, alloc_t<V>, alloc_t<E>, alloc_t<E>>
loadEdgeListCSR(Backend<Platform> &p, Graph<V,E> &graph)
{
    auto vertices = p.template allocConstant<V>(graph.raw_vertices.size);
    auto inEdges = p.template allocConstant<E>(graph.raw_edges.size);
    auto outEdges = p.template allocConstant<E>(graph.raw_edges.size);

    size_t edge = 0;

    for (E i = 0; i < graph.vertex_count; i++) {
        for (size_t j = graph.raw_vertices[i]; j < graph.raw_vertices[i+1]; j++) {
            (*inEdges)[edge] = i;
            (*outEdges)[edge] = graph.raw_edges[j];
            edge++;
        }
        (*vertices)[i] = graph.raw_vertices[i];
    }

    (*vertices)[graph.vertex_count] = graph.raw_vertices[graph.vertex_count];

    return std::make_tuple(graph.vertex_count, graph.edge_count, vertices, inEdges, outEdges);
}

template<typename Platform, typename V, typename E>
std::tuple<size_t, size_t, alloc_t<V>, alloc_t<Edge<E>>>
loadStructEdgeListCSR(Backend<Platform> &p, Graph<V,E> &graph)
{
    auto vertices = p.template allocConstant<V>(graph.raw_vertices.size);
    auto edges = p.template allocConstant<Edge<E>>(graph.raw_edges.size);

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

    return std::make_tuple(graph.vertex_count, graph.edge_count, vertices, edges);
}

template<typename Platform, typename V, typename E>
std::tuple<size_t, size_t, alloc_t<V>, alloc_t<E>>
loadCSR(Backend<Platform> &p, Graph<V,E> &graph)
{
    auto nodes = p.template allocConstant<V>(graph.raw_vertices.size);
    auto edges = p.template allocConstant<E>(graph.raw_edges.size);

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        (*nodes)[i] = graph.raw_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_edges.size; i++) {
        (*edges)[i] = graph.raw_edges[i];
    }

    return std::make_tuple(graph.vertex_count, graph.edge_count, nodes, edges);
}

template<typename Platform, typename V, typename E>
std::tuple<size_t, size_t, alloc_t<V>, alloc_t<E>, alloc_t<V>>
loadReverseCSR(Backend<Platform> &p, Graph<V,E> &graph)
{
    auto rev_nodes = p.template allocConstant<V>(graph.raw_rev_vertices.size);
    auto rev_edges = p.template allocConstant<E>(graph.raw_rev_edges.size);
    auto nodes = p.template allocConstant<V>(graph.raw_vertices.size);

    for (size_t i = 0; i < graph.raw_rev_vertices.size; i++) {
        (*rev_nodes)[i] = graph.raw_rev_vertices[i];
    }

    for (size_t i = 0; i < graph.raw_rev_edges.size; i++) {
        (*rev_edges)[i] = graph.raw_rev_edges[i];
    }

    for (size_t i = 0; i < graph.raw_vertices.size; i++) {
        (*nodes)[i] = graph.raw_vertices[i];
    }

    return std::make_tuple(graph.vertex_count, graph.edge_count, rev_nodes, rev_edges, nodes);
}
#endif
