#ifndef TEMPLATECONFIG_HPP
#define TEMPLATECONFIG_HPP

#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "AlgorithmConfig.hpp"
#include "Graph.hpp"
#include "Timer.hpp"

enum class work_division { nodes, edges };

template
< typename Platform
, typename KernelType
, typename GraphData
, typename V
, typename E
>
struct TemplateConfig : public AlgorithmConfig {
  using Vertex = V;
  using Edge = E;
  using Kernel = KernelType;
  using LoadFun = std::function<GraphData(Platform&, Graph<V,E>&)>;
  using Pair = std::pair<size_t,size_t>;

  template<typename T>
  using alloc_t = typename Platform::template alloc_t<T>;

  protected:
    size_t vertex_count, edge_count;
    Pair nodeDivision, edgeDivision;
    Platform &backend;
    Kernel kernel;

    LoadFun load;
    work_division workDivision;
    GraphData graph;

    TemplateConfig
        ( const Options& opts
        , size_t count
        , LoadFun loadFun
        , work_division division
        , Kernel kern
        )
    : AlgorithmConfig(opts, count)
    , backend(Platform::get()), kernel(kern), load(loadFun)
    , workDivision(division)
    {}

    virtual size_t getSharedMemSize(size_t) const
    { return 0; }

    void transferData() {}

    template<typename T, typename... Args>
    void transferData(alloc_t<T> &data, const Args&... args)
    {
        data.copyHostToDev();
        transferData(args...);
    }

    template<typename... Args>
    void transferData(std::tuple<Args...> &tuple)
    { transferData(tuple, std::index_sequence_for<Args...>()); }

    template<size_t... I, typename... Args>
    void transferData(std::tuple<Args...> &tuple, std::index_sequence<I...>)
    { transferData(std::get<I>(tuple)...); }

    template<typename T, typename... Args>
    void transferData(const T&, const Args&... args)
    { transferData(args...); }

    template<typename K, typename... Graph, typename... Args>
    void run(K kern, std::tuple<Graph...> &tuple, const Args&... args)
    { run(kern, tuple, args..., std::index_sequence_for<Graph...>()); }

    template<typename K, typename... Graph, typename... Args, size_t... I>
    void run(K kern, std::tuple<Graph...> &tuple, const Args&... args, std::index_sequence<I...>)
    { backend.runKernel(kern, std::get<I>(tuple)..., args...); }

    template<typename K, typename... Args>
    void run(K kern, const Args&... args)
    { backend.runKernel(kern, args...); }

  public:
    void transferGraph(const std::string filename) override final
    {
        Timer graphTransfer("graphTransfer", run_count);

        auto file = Graph<V,E>(filename);
        graph = load(backend, file);

        vertex_count = file.vertex_count;
        edge_count = file.edge_count;

        nodeDivision = backend.computeDivision(vertex_count);
        edgeDivision = backend.computeDivision(edge_count);

        graphTransfer.start();
        transferData(graph);
        graphTransfer.stop();
    }

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
    void runKernel(K kern, const Args&... args)
    { run(kern, graph, args...); }

    template<typename K, typename... Args>
    void runNonWarpKernel(K kern, const Args&... args)
    { run(kern, graph, args...); }
};

template
< typename Platform
, typename Kernel
, typename GraphData
, typename V
, typename E
>
struct WarpConfig : public TemplateConfig<Platform,Kernel,GraphData,V,E> {
    using LoadFun = typename WarpConfig::LoadFun;
    using Config = TemplateConfig<Platform,Kernel,GraphData,V,E>;
    using Config::options;
    using Config::graph;

    template<typename... Args>
    WarpConfig(std::function<size_t(size_t)> memFun, Args... args)
    : Config(args...)
    , chunkMemory(memFun), warp_size(32), chunk_size(32)
    {
        options.add('w', "warp", "NUM", warp_size,
                    "Virtual warp size for warp variants.")
               .add('c', "chunk", "NUM", chunk_size,
                    "Work chunk size for warp variants.");
    }

    size_t getSharedMemSize(size_t workPerWarp) const
    { return (workPerWarp / this->warp_size) * this->chunkMemory(chunk_size); }

    template<typename K, typename... RunArgs>
    void runKernel(K kern, const RunArgs&... args)
    { this->run(kern, warp_size, chunk_size, graph, args...); }

    template<typename K, typename... RunArgs>
    void runNonWarpKernel(K kern, const RunArgs&... args)
    { this->run(kern, graph, args...); }

  private:
    std::function<size_t(size_t)> chunkMemory;
    size_t warp_size, chunk_size;
};

template
< template<typename, typename...> class Cfg
, typename Platform
, typename Kernel
, typename GraphData
, typename Vertex
, typename Edge
, typename... Args
, typename Config = Cfg<TemplateConfig<Platform,Kernel,GraphData,Vertex,Edge>, Args...>
>
Config* make_config
    ( const Options& opts
    , size_t count
    , GraphData(*l)(Platform&, Graph<Vertex,Edge>&)
    , work_division w
    , Kernel kern
    , Args... args
    )
{ return new Config(args..., opts, count, l, w, kern); }

template
< template<typename, typename...> class Cfg
, typename Platform
, typename Kernel
, typename GraphData
, typename Vertex
, typename Edge
, typename... Args
, typename Config = Cfg<WarpConfig<Platform,Kernel,GraphData,Vertex,Edge>,  Args...>
>
Config* make_warp_config
    ( const Options& opts
    , size_t count
    , GraphData(*l)(Platform&, Graph<Vertex,Edge>&)
    , work_division w
    , Kernel k
    , std::function<size_t(size_t)> memFun
    , Args... args
    )
{ return new Config(args..., memFun, opts, count, l, w, k); }
#endif
