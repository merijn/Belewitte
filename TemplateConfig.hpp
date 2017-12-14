#ifndef TEMPLATECONFIG_HPP
#define TEMPLATECONFIG_HPP

#include "AlgorithmConfig.hpp"
#include "GraphLoading.hpp"
#include "Timer.hpp"

enum class work_division { vertex, edge };

template<typename Platform, typename V, typename E, typename... Args>
struct GraphKernel
{
    template<typename, typename, typename, typename...>
    friend struct TemplateConfig;

    virtual void run(Loader<Platform,V,E>&, const Args&...) = 0;

    GraphKernel(GraphRep rep, work_division w)
     : backend(Platform::get())
     , representation(rep)
     , workDivision(w)
    {}

    virtual ~GraphKernel()
    {}

    virtual size_t getSharedMemSize(size_t)
    { return 0; }

  protected:
    Platform &backend;
    const GraphRep representation;
    const work_division workDivision;
};

template<typename Platform, typename V, typename E, typename... Args>
struct WarpKernel : GraphKernel<Platform,V,E,Args...>
{
    using GraphKernel<Platform,V,E,Args...>::backend;

    WarpKernel(GraphRep r, work_division w, std::function<size_t(size_t)> mem)
     : GraphKernel<Platform,V,E,Args...>(r, w), chunkMemory(mem)
    {}

  protected:
    virtual size_t getSharedMemSize(size_t blockSize) override
    { return (blockSize / warp_size) * chunkMemory(chunk_size); }

    std::function<size_t(size_t)> chunkMemory;
    size_t warp_size, chunk_size;
};

template
< typename Platform
, typename V
, typename E
, Rep rep
, Dir dir
, typename Kernel
, typename KernelType
, typename... KernelArgs
>
struct DerivedKernel : KernelType
{
    using KernelType::backend;
    using KernelType::representation;
    using GraphKernel = GraphKernel<Platform,V,E,KernelArgs...>;
    using WarpKernel = WarpKernel<Platform,V,E,KernelArgs...>;

    Kernel kernel;

    template<typename... Args>
    DerivedKernel(Kernel kern, Args... args)
     : KernelType({rep, dir}, args...), kernel(kern)
    {}

    virtual void
    run(Loader<Platform,V,E>& loader, const KernelArgs&... args) override
    { doRun(loader, args...); }

    template<class Parent = KernelType>
    typename std::enable_if<std::is_same<WarpKernel,Parent>::value,void>::type
    doRun(Loader<Platform,V,E>& loader, const KernelArgs&... args)
    {
        const auto& graph = loader.template getGraph<rep,dir>();
        backend.runKernel(kernel, this->warp_size, this->chunk_size, graph,
                          args...);
    }

    template<class Parent = KernelType>
    typename std::enable_if<std::is_same<GraphKernel,Parent>::value,void>::type
    doRun(Loader<Platform,V,E>& loader, const KernelArgs&... args)
    {
        const auto& graph = loader.template getGraph<rep,dir>();
        backend.runKernel(kernel, graph, args...);
    }
};

template<typename Platform, typename V, typename E, typename... Args>
using KernelType = std::shared_ptr<GraphKernel<Platform,V,E,Args...>>;

template<typename Platform, typename V, typename E, typename... Args>
class KernelMap : public std::map<std::string,KernelType<Platform,V,E,Args...>>
{
    using Loader = Loader<Platform,V,E>;

    template<Rep rep, Dir dir, typename Kernel, typename Parent>
    using Base = DerivedKernel<Platform,V,E,rep,dir,Kernel,Parent,Args...>;

    template<Rep rep, Dir dir, typename Kernel>
    using GraphKernel = Base<rep,dir,Kernel,GraphKernel<Platform,V,E,Args...>>;

    template<Rep rep, Dir dir, typename Kernel>
    using WarpKernel = Base<rep,dir,Kernel,WarpKernel<Platform,V,E,Args...>>;

  public:
    template
    < Rep rep
    , Dir dir = Dir::Forward
    , typename Graph = typename LoaderRep<rep,Platform,V,E>::GraphType
    , typename Kernel = typename Platform::template kernel<Graph,Args...>::type
    >
    void insert_kernel(const std::string& key, Kernel k, work_division w)
    {
        auto ptr = std::make_shared<GraphKernel<rep,dir,Kernel>>(k,w);
        auto result = this->insert({key, ptr});
        if (!result.second) throw std::domain_error("Key already exists!");
    }

    template
    < Rep rep
    , Dir dir = Dir::Forward
    , typename Graph = typename LoaderRep<rep,Platform,V,E>::GraphType
    , typename Kernel = typename Platform::template kernel<size_t,size_t,Graph,Args...>::type
    >
    void
    insert_warp_kernel
        ( const std::string& key, Kernel k, work_division w
        , std::function<size_t(size_t)> mem)
    {
        auto ptr = std::make_shared<WarpKernel<rep,dir,Kernel>>(k,w, mem);
        auto result = this->insert({key, ptr});
        if (!result.second) throw std::domain_error("Key already exists!");
    }
};

template
< typename Platform
, typename Vertex
, typename Edge
, typename Format
, typename... Args
, typename Map = KernelMap
    < Platform
    , Vertex
    , Edge
    , typename Platform::template DevToHost<Args>::type...>
>
Map
make_kernel_map(void (*)(Format, Args...))
{ return Map(); }

template<typename Platform, typename V, typename E, typename... Args>
struct TemplateConfig : public AlgorithmConfig {
  using Vertex = V;
  using Edge = E;
  using KernelType = GraphKernel<Platform,V,E,Args...>;

  template<typename T>
  using alloc_t = typename Platform::template alloc_t<T>;

  protected:
    Platform& backend;
    size_t vertex_count, edge_count;
    std::shared_ptr<GraphKernel<Platform,V,E,Args...>> kernel;

    Loader<Platform,V,E> loader;

    TemplateConfig
        ( const Options& opts
        , size_t count
        , std::shared_ptr<KernelType> kern
        )
    : AlgorithmConfig(opts, count)
    , backend(Platform::get())
    , kernel(kern)
    {}

    void setKernelConfig(std::shared_ptr<KernelType> k)
    {
        auto div = getWorkDivision(k->workDivision);
        auto sharedMem = k->getSharedMemSize(div.first);
        backend.setWorkSizes(1, {div.first}, {div.second}, sharedMem);
    }

    void setKernelConfig(work_division w, size_t sharedMem = 0)
    {
        auto div = getWorkDivision(w);
        backend.setWorkSizes(1, {div.first}, {div.second}, sharedMem);
    }

    virtual void loadGraph(const Graph<V,E>& graph)
    { loader.loadGraph(graph, kernel->representation); }

    virtual void transferGraph()
    { loader.transferGraph(kernel->representation); }

    void loadGraph(const std::string filename) override final
    {
        Timer graphTransfer("graphTransfer", run_count);
        Graph<V,E>& graph(filename);

        loadGraph(graph);

        vertex_count = graph.vertex_count;
        edge_count = graph.edge_count;
        vertexDivision = backend.computeDivision(vertex_count);
        edgeDivision = backend.computeDivision(edge_count);

        graphTransfer.start();
        transferGraph();
        graphTransfer.stop();
    }

  private:
    std::pair<size_t,size_t> vertexDivision, edgeDivision;

    const std::pair<size_t,size_t>&
    getWorkDivision(work_division w)
    {
        switch (w) {
            case work_division::edge: return edgeDivision;
            case work_division::vertex: return vertexDivision;
        }
    }
};

template<typename Platform, typename V, typename E, typename... Args>
struct WarpConfig : public TemplateConfig<Platform,V,E,Args...> {
    using Config = TemplateConfig<Platform,V,E,Args...>;
    using Config::options;

    template<typename... ParentArgs>
    WarpConfig(ParentArgs... args)
    : Config(args...), warp_size(32), chunk_size(32)
    {
        options.add('w', "warp", "NUM", warp_size,
                    "Virtual warp size for warp variants.")
               .add('c', "chunk", "NUM", chunk_size,
                    "Work chunk size for warp variants.");
    }

  private:
    size_t warp_size, chunk_size;
};

template
< template<typename, typename...> class Cfg
, typename Platform
, typename Vertex
, typename Edge
, typename... KernelArgs
, typename... Args
, typename Base = TemplateConfig<Platform,Vertex,Edge,KernelArgs...>
, typename WarpBase = WarpConfig<Platform,Vertex,Edge,KernelArgs...>
, typename Config = Cfg<Base,Args...>
, typename WarpConfig = Cfg<WarpBase,Args...>
>
AlgorithmConfig* make_config
    ( const Options& opts
    , size_t count
    , std::shared_ptr<GraphKernel<Platform,Vertex,Edge,KernelArgs...>> k
    , Args... args
    )
{
    typedef WarpKernel<Platform,Vertex,Edge,KernelArgs...> WarpKernel;
    typedef std::shared_ptr<WarpKernel> WarpKernelPtr;

    if (auto kern = std::dynamic_pointer_cast<WarpKernelPtr>(k)) {
        return new WarpConfig(args..., opts, count, k);
    } else {
        return new Config(args..., opts, count, k);
    }
}
#endif
