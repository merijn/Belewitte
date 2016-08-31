#ifndef __TEMPLATECONFIG_HPP__
#define __TEMPLATECONFIG_HPP__

#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "AlgorithmConfig.hpp"
#include "Graph.hpp"
#include "Timer.hpp"
#include "WarpDispatch.hpp"

enum class work_division { nodes, edges };

template
< typename Platform
, typename Kernel
, typename V
, typename E
, typename GraphData
>
struct TemplateConfig : public AlgorithmConfig {
  using Vertex = V;
  using Edge = E;
  using LoadFun = std::function<GraphData(Platform&, Graph<V,E>&)>;
  using SharedMemFun = std::function<size_t(size_t)>;
  using Pair = std::pair<size_t,size_t>;

  template<typename T>
  using alloc_t = typename Platform::template alloc_t<T>;

  protected:
    size_t vertex_count, edge_count;
    Pair nodeDivision, edgeDivision;
    SharedMemFun getSharedMemSize;
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
    , getSharedMemSize([](auto) { return 0; }), backend(Platform::get())
    , kernel(kern), load(loadFun), workDivision(division)
    {}

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

template
< template<size_t,size_t> class Warp
, template<typename,typename,typename,typename...> class BaseConfig
, typename Platform
, typename Kernel
, typename GraphData
, typename... Args
>
struct WarpConfig : public BaseConfig<Platform,Kernel,GraphData,Args...> {
    using Vertex = typename WarpConfig::Vertex;
    using Edge = typename WarpConfig::Edge;
    using LoadFun = typename WarpConfig::LoadFun;
    using Config = BaseConfig<Platform,Kernel,GraphData,Args...>;
    using Config::options;

    WarpConfig
        ( const Options& opts
        , size_t count
        , LoadFun loadFun
        , work_division d
        , warp_dispatch<Warp>
        , Args... args
        )
    : Config(opts, count, loadFun, d, nullptr, args...)
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

template
< template<typename, typename, typename, typename...> class Cfg
, typename Platform
, typename Kernel
, typename... Args
, typename GraphData
, typename Config = Cfg<Platform, Kernel, GraphData, Args...>
, typename Vertex = typename Config::Vertex
, typename Edge = typename Config::Edge
>
Config* make_config
    ( const Options& opts
    , size_t count
    , GraphData(*l)(Platform&, Graph<Vertex,Edge>&)
    , work_division w
    , Kernel kern
    , Args... args
    )
{ return new Config(opts, count, l, w, kern, args...); }

template
< template<typename, typename, typename, typename...> class Cfg
, template<size_t,size_t> class Warp
, typename Platform
, typename... Args
, typename GraphData
, typename Kernel = decltype(warp_dispatch<Warp>::work(0,0).kernel)
, typename Config = WarpConfig<Warp,Cfg,Platform,Kernel,GraphData,Args...>
, typename Vertex = typename Config::Vertex
, typename Edge = typename Config::Edge
>
Config* make_warp_config
    ( const Options& opts
    , size_t count
    , GraphData(*l)(Platform&, Graph<Vertex,Edge>&)
    , work_division w
    , warp_dispatch<Warp> d
    , Args... args
    )
{ return new Config(opts, count, l, w, d, args...); }
#endif
