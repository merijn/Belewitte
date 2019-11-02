#ifndef IMPLEMENTATIONTEMPLATE_HPP
#define IMPLEMENTATIONTEMPLATE_HPP

#include <fstream>

#include "ImplementationBase.hpp"
#include "GraphLoader.hpp"
#include "Timer.hpp"

#define tag_t(v) (tag_t<decltype(v), v>{})

template<typename T, T value>
struct tag_t {};

enum class work_division { vertex, edge };

template<typename Platform>
struct BaseKernel
{
    template<typename, typename, typename>
    friend struct ImplementationTemplate;

    using KernelType = typename Platform::kernel_type;

    template<typename T>
    using DevToHost = const typename Platform::template DevToHost<T>::type&;

    BaseKernel(KernelType k, GraphRep rep, work_division w, bool warp)
      : isWarp(warp), representation(rep), workDivision(w), kernel(k)
      , backend(Platform::get())
    {}

    virtual ~BaseKernel()
    {}

    const bool isWarp;
    const GraphRep representation;
    const work_division workDivision;

    virtual void
    setWarpConfig(size_t,size_t)
    {}

  protected:
    BaseKernel(std::nullptr_t)
      : isWarp(false), representation({Rep::VertexCount, Dir::Forward})
      , workDivision(work_division::vertex), kernel(nullptr)
      , backend(Platform::get())
    {}

    virtual size_t getSharedMemSize(size_t)
    { return 0; }

    KernelType kernel;
    Platform &backend;
};

namespace internals
{
template<typename Platform, typename V, typename E, typename... Args>
struct GraphKernel : public BaseKernel<Platform>
{
    using typename BaseKernel<Platform>::KernelType;

    template<typename T>
    using DevToHost = typename BaseKernel<Platform>::template DevToHost<T>;

    virtual explicit operator bool() const = 0;

    virtual void
    run(GraphLoader<Platform,V,E>&, DevToHost<Args>...) = 0;

    GraphKernel(std::nullptr_t p)
      : BaseKernel<Platform>(p)
    {}

    GraphKernel(KernelType k, GraphRep rep, work_division w, bool warp)
      : BaseKernel<Platform>(k, rep, w, warp)
    {}
};

template
< typename Platform
, typename V
, typename E
, Rep rep
, Dir dir
, typename... Args
>
struct GraphKernelImpl : GraphKernel<Platform,V,E,Args...>
{
    using Graph = typename LoaderRep<rep,Platform,V,E>::GraphType;
    using Kernel = typename Platform::template kernel<Graph,Args...>::type;
    using typename GraphKernel<Platform,V,E,Args...>::KernelType;
    using GraphKernel<Platform,V,E,Args...>::backend;
    using GraphKernel<Platform,V,E,Args...>::kernel;

    template<typename T>
    using DevToHost = typename BaseKernel<Platform>::template DevToHost<T>;

    GraphKernelImpl(Kernel k, work_division w)
      : GraphKernel<Platform,V,E,Args...>
            (reinterpret_cast<KernelType>(k), {rep, dir}, w, false)
    {}

    virtual explicit operator bool() const override
    { return true; }

    virtual void
    run(GraphLoader<Platform,V,E>& loader, DevToHost<Args>... args) override
    {
        const auto& graph = loader.template getGraph<rep,dir>();
        backend.runKernel(reinterpret_cast<Kernel>(kernel), graph, args...);
    }
};

template
< typename Platform
, typename V
, typename E
, Rep rep
, Dir dir
, typename... Args
>
struct WarpKernel : GraphKernel<Platform,V,E,Args...>
{
    using Graph = typename LoaderRep<rep,Platform,V,E>::GraphType;
    using Kernel = typename Platform::template kernel
            <size_t,size_t,Graph,Args...>::type;
    using typename GraphKernel<Platform,V,E,Args...>::KernelType;
    using GraphKernel<Platform,V,E,Args...>::backend;
    using GraphKernel<Platform,V,E,Args...>::kernel;

    template<typename T>
    using DevToHost = typename BaseKernel<Platform>::template DevToHost<T>;

    WarpKernel(Kernel k, work_division w, std::function<size_t(size_t)> mem)
      : GraphKernel<Platform,V,E,Args...>
            (reinterpret_cast<KernelType>(k), {rep, dir}, w, true)
      , chunkMemory(mem), warp_size(32), chunk_size(32)
    {}

    virtual explicit operator bool() const override
    { return true; }

    virtual void
    run(GraphLoader<Platform,V,E>& loader, DevToHost<Args>... args) override
    {
        const auto& graph = loader.template getGraph<rep,dir>();
        backend.runKernel
            ( reinterpret_cast<Kernel>(kernel)
            , warp_size
            , chunk_size
            , graph
            , args...);
    }

    virtual void
    setWarpConfig(size_t warp, size_t chunk) final
    {
        warp_size = warp;
        chunk_size = chunk;
    }

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
, typename... Args
>
struct NullKernel : GraphKernel<Platform,V,E,Args...>
{
    template<typename T>
    using DevToHost = typename BaseKernel<Platform>::template DevToHost<T>;

    NullKernel()
      : GraphKernel<Platform,V,E,Args...>(nullptr)
    {}

    virtual explicit operator bool() const override
    { return false; }

    virtual void
    run(GraphLoader<Platform,V,E>&, DevToHost<Args>...) override
    {}
};
}

template<typename P, typename V, typename E, typename... Args>
struct GraphKernel : std::shared_ptr<internals::GraphKernel<P,V,E,Args...>>
{
    using Null = internals::NullKernel<P,V,E,Args...>;
    using Base = internals::GraphKernel<P,V,E,Args...>;

    template<typename T>
    using DevToHost = typename Base::template DevToHost<T>;

    GraphKernel()
      : GraphKernel(std::make_shared<Null>())
    {}

    GraphKernel(const std::shared_ptr<Base>& f)
      : std::shared_ptr<Base>(f)
    {}

    explicit operator bool() const
    {
        if (std::shared_ptr<Base>::operator bool()) {
            return static_cast<bool>(*this->get());
        }
        return false;
    }

    GraphKernel&
    operator=(std::nullptr_t)
    { std::shared_ptr<Base>::operator=(std::make_shared<Null>()); return *this; }

    void
    operator()(GraphLoader<P,V,E>& l, DevToHost<Args>... args)
    { this->get()->run(l, args...); }
};

template<typename P, typename V, typename E, Rep r, Dir d, typename... Args>
GraphKernel
(const std::shared_ptr<internals::GraphKernelImpl<P,V,E,r,d,Args...>>&)
    -> GraphKernel<P,V,E,Args...>;

template<typename P, typename V, typename E, Rep r, Dir d, typename... Args>
GraphKernel
(const std::shared_ptr<internals::WarpKernel<P,V,E,r,d,Args...>>&)
    -> GraphKernel<P,V,E,Args...>;

template<typename Platform, typename V, typename E>
struct KernelBuilder
{
    KernelBuilder() {}
    ~KernelBuilder() {}

    template
    < Rep rep
    , Dir dir = Dir::Forward
    , typename... Args
    , typename Graph = typename LoaderRep<rep,Platform,V,E>::GraphType
    , typename KernelImpl = internals::GraphKernelImpl
            <Platform,V,E,rep,dir,Args...>
    >
    auto
    operator()
    ( void (*k)(Graph, Args...)
    , work_division w
    , tag_t<Rep, rep>
    , tag_t<Dir, dir> = {}
    )
    { return GraphKernel(std::make_shared<KernelImpl>(k,w)); }

    template
    < Rep rep
    , Dir dir = Dir::Forward
    , typename... Args
    , typename Graph = typename LoaderRep<rep,Platform,V,E>::GraphType
    , typename KernelImpl = internals::WarpKernel<Platform,V,E,rep,dir,Args...>
    >
    auto
    operator()
    ( void (*k)(size_t, size_t, Graph, Args...)
    , work_division w
    , std::function<size_t(size_t)> mem
    , tag_t<Rep, rep>
    , tag_t<Dir, dir> = {}
    )
    { return GraphKernel(std::make_shared<KernelImpl>(k,w, mem)); }
};

template<typename Key, typename T>
class KernelMap : public std::map<Key,T>
{
  public:
    KernelMap() {}

    KernelMap(std::initializer_list<std::pair<Key,T>> l)
      : std::map<Key,T>(l.begin(), l.end())
    {}

    KernelMap&
    operator+=(const KernelMap& other)
    {
        for (auto& pair : other) {
            auto result = this->insert(pair);
            if (!result.second) throw std::domain_error("Key already exists!");
        }

        return *this;
    }
};

template<typename T>
KernelMap(std::initializer_list<std::pair<const char*,T>> l)
    -> KernelMap<std::string,T>;

template<typename Platform, typename V, typename E>
struct ImplementationTemplate : public ImplementationBase
{
    typedef V Vertex;
    typedef E Edge;
    using ImplementationBase::options;

    template<typename T>
    using alloc_t = typename Platform::template alloc_t<T>;

    template<typename... Args>
    using GraphKernel = GraphKernel<Platform,V,E,Args...>&;

    static constexpr bool isSwitching = false;

  protected:
    Platform& backend;
    GraphLoader<Platform,V,E> loader;

    size_t vertex_count, edge_count;
    size_t warp_size, chunk_size;

    ImplementationTemplate()
      : backend(Platform::get())
      , vertex_count(0), edge_count(0)
      , warp_size(32), chunk_size(32)
    {}

    void setKernelConfig(std::shared_ptr<BaseKernel<Platform>> k)
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

    virtual void loadGraph(const Graph<V,E>&) = 0;

    virtual void transferGraph() = 0;

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

    void freeGraph() override final
    { loader.freeGraph(); }

  private:
    const std::pair<size_t,size_t>&
    getWorkDivision(work_division w)
    {
        switch (w) {
          case work_division::edge: return edgeDivision;
          case work_division::vertex: return vertexDivision;
        }
    }

    std::pair<size_t,size_t> vertexDivision, edgeDivision;
};

template<typename AlgorithmBase, typename... Kernels>
struct SimpleImplementation : public AlgorithmBase
{
    using typename AlgorithmBase::Vertex;
    using typename AlgorithmBase::Edge;
    using AlgorithmBase::options;
    using AlgorithmBase::loader;
    using AlgorithmBase::warp_size;
    using AlgorithmBase::chunk_size;

    std::tuple<Kernels...> kernels;

    SimpleImplementation(std::tuple<Kernels...> ks)
      : SimpleImplementation(ks, std::index_sequence_for<Kernels...>())
    {}

    template<size_t... I>
    SimpleImplementation(std::tuple<Kernels...> ks, std::index_sequence<I...>)
      : AlgorithmBase(std::get<I>(kernels)...)
      , kernels(ks)
    {
        if ((std::get<I>(ks)->isWarp || ...)) {
            options.add('w', "warp", "NUM", warp_size,
                        "Virtual warp size for warp variants.")
                   .add('c', "chunk", "NUM", chunk_size,
                        "Work chunk size for warp variants.");
        }
    }

  protected:
    virtual void loadGraph(const Graph<Vertex,Edge>& graph) override final
    {
        std::vector<GraphRep> reps;
        auto load = [&reps](auto&& k) {
            reps.push_back(k->representation);
        };

        mapKernels(load);

        loader.loadGraph(graph, reps);
    }

    virtual void transferGraph() override final
    {
        auto transfer = [this](auto&& k) {
            loader.transferGraph(k->representation);
        };

        mapKernels(transfer);
    }

  private:
    virtual void prepareRun() override
    {
        if (warp_size < 0) {
            reportError("Warp size should be positive!");
        } else if (warp_size > 32) {
            reportError("Warp size should be less than 32!");
        } else if (warp_size & (warp_size - 1)) {
            reportError("Warp size should be a power of 2!");
        } else if (chunk_size % warp_size != 0) {
            reportError("Chunk size should be a multiple of warp size!");
        }

        auto setWarpReferences = [this](auto&& k) {
            if (k->isWarp) k->setWarpConfig(warp_size, chunk_size);
        };

        mapKernels(setWarpReferences);
    }

    template<typename Function>
    void mapKernels(Function f)
    { mapKernels(f, std::index_sequence_for<Kernels...>()); }

    template<typename Function, size_t... I>
    void mapKernels(Function f, std::index_sequence<I...>)
    { (f(std::get<I>(kernels)), ...); }
};

template
< template<typename,typename,typename> class BaseImpl
, typename Platform
, typename Vertex
, typename Edge
, typename... KernelArgs
, typename... Kernels
, typename Impl = BaseImpl<Platform,Vertex,Edge>
>
std::unique_ptr<ImplementationBase>
make_implementation
( std::tuple
    <GraphKernel<Platform,Vertex,Edge,KernelArgs...>,Kernels...> kernels
)
{
    using Kernel = GraphKernel<Platform,Vertex,Edge,KernelArgs...>;
    using SimpleImpl = SimpleImplementation<Impl,Kernel,Kernels...>;
    return std::make_unique<SimpleImpl>(kernels);
}

template<typename AlgorithmBase, typename... Kernels>
struct SwitchImplementation;

class prop_ref : public std::reference_wrapper<double>
{
    static double dummyProp;

  public:
    prop_ref(prop_ref&&) = delete;
    prop_ref(const prop_ref&) = delete;

    prop_ref(const std::string&, ImplementationBase&)
     : std::reference_wrapper<double>(dummyProp)
    {}

    template<typename AlgorithmBase, typename... Kernels>
    prop_ref
        ( const std::string& name
        , SwitchImplementation<AlgorithmBase,Kernels...>& cfg
        , bool graphProp = false
        )
        : std::reference_wrapper<double>(dummyProp)
    {
        if (graphProp) cfg.graphProperties.emplace(name, std::ref(*this));
        else cfg.algorithmProperties.emplace(name, std::ref(*this));
    }

    prop_ref& operator=(const std::reference_wrapper<double>& val)
    { std::reference_wrapper<double>::operator=(val); return *this; }

    double& operator=(const double& val)
    { return this->get() = val; }

    void reset()
    { operator=(std::ref(dummyProp)); }
};

double prop_ref::dummyProp = 0;

template<typename AlgorithmBase, typename... Kernels>
struct SwitchImplementation : public AlgorithmBase
{
    using typename AlgorithmBase::Vertex;
    using typename AlgorithmBase::Edge;
    using AlgorithmBase::loader;
    using AlgorithmBase::options;
    using AlgorithmBase::setKernelConfig;

    using prop_set =  std::set<std::string>;
    using KernelMap = KernelMap<std::string,std::tuple<Kernels...>>;

    friend prop_ref;

    static constexpr bool isSwitching = true;
    std::tuple<Kernels...> kernels;

    class graph_prop
    {
        graph_prop(graph_prop&&) = delete;
        graph_prop(const graph_prop&) = delete;

        prop_ref absProp, inProp, outProp;

      public:
        graph_prop
          (std::string prefix, std::string suffix, SwitchImplementation& cfg)
          : absProp(prefix + "abs" + suffix, cfg, true)
          , inProp(prefix + "in" + suffix, cfg, true)
          , outProp(prefix + "out" + suffix, cfg, true)
        {}

        prop_ref& operator[](Degrees deg)
        {
            switch (deg) {
                case Degrees::abs: return absProp;
                case Degrees::in: return inProp;
                case Degrees::out: return outProp;
            }
        }
    };

    SwitchImplementation(KernelMap ks)
      : SwitchImplementation(ks, std::index_sequence_for<Kernels...>())
    {}

    template<size_t... I>
    SwitchImplementation(KernelMap ks, std::index_sequence<I...>)
      : AlgorithmBase(std::get<I>(kernels)...)
      , kernels((static_cast<void>(I), nullptr)...)
      , modelHandle(nullptr)
      , kernelMap(ks)
      , vertices("vertex count", *this, true)
      , edges("edge count", *this, true)
      , min("min ", " degree", *this)
      , lowerQuantile("lower quantile ", " degree", *this)
      , mean("mean ", " degree", *this)
      , median("median ", " degree", *this)
      , upperQuantile("upper quantile ", " degree", *this)
      , max("max ", " degree", *this)
      , stdDev("stddev ", " degree", *this)
    {
        options.add('m', "model", "FILE", model, "Prediction model to use.");
        options.add('l', "log", "FILE", logFile, "Where to log properties.");
    }

    virtual void loadGraph(const Graph<Vertex,Edge>& graph) override final
    {
        vertices = graph.vertex_count;
        edges = graph.edge_count;

        for (const auto& type : { Degrees::abs, Degrees::in, Degrees::out }) {
            auto summary = graph.degreeStatistics(type);

            min[type] = summary.min;
            lowerQuantile[type] = summary.lowerQuantile;
            mean[type] = summary.mean;
            median[type] = summary.median;
            upperQuantile[type] = summary.upperQuantile;
            max[type] = summary.max;
            stdDev[type] = summary.stdDev;
        }

        std::vector<GraphRep> reps;
        auto load = [&reps](auto&& k) {
            reps.push_back(k->representation);
        };

        for (auto& impl : implementations) {
            mapKernels(load, impl);
        }

        loader.loadGraph(graph, reps);
    }

    virtual void transferGraph() override final
    {
        auto transfer = [this](auto&& k) {
            loader.transferGraph(k->representation);
        };

        for (auto& impl : implementations) {
            mapKernels(transfer, impl);
        }
    }

  protected:
    void predictInitial()
    {
        stepNum = 0;
        logGraphProps();
        logAlgorithmProps();

        lastKernel = lookup();
        if (lastKernel == -1) lastKernel = defaultKernel;

        kernels = implementations[static_cast<size_t>(lastKernel)];
        setKernelConfig(kernels);
    }

    void predict()
    {
        ++stepNum;
        logAlgorithmProps();

        int32_t result = lookup();
        if (result != -1 && result != lastKernel) {
            kernels = implementations[static_cast<size_t>(result)];
            setKernelConfig(kernels);
            lastKernel = result;
        }
    }

    virtual void prepareRun() override final
    {
        prop_set missingGraphProps;
        prop_set missingAlgoProps;

        for (auto& pair : graphProperties) {
            auto& [name, val] = pair;
            missingGraphProps.insert(name);
        }

        for (auto& pair : algorithmProperties) {
            auto& [name, val] = pair;
            missingAlgoProps.insert(name);
        }

        if (!model.empty()) {
            setupPredictor(model.c_str(), missingGraphProps, missingAlgoProps);
        } else {
            lookup = []() { return -1; };

            try {
                kernels = kernelMap.at("edge-list");
            } catch (const std::out_of_range&) {
            }

            implementations.emplace_back(kernels);
        }

        if (!logFile.empty()) {
            setupLogging(missingGraphProps, missingAlgoProps);
        } else {
            logGraphProps = [](){};
            logAlgorithmProps = [](){};
        }

        bool anyUninitialised = false;
        auto checkInitialised = [&anyUninitialised](auto&& k) {
            if (k == nullptr) anyUninitialised = true;
        };

        mapKernels(checkInitialised, kernels);
        if (anyUninitialised) {
            reportError("No edge list implementation found!");
        }
    }

    virtual void cleanupRun() override final
    {
        implementations.clear();

        if (modelHandle) {
            int result = dlclose(modelHandle);
            if (result) reportError("dlclose() failed!\n", dlerror());
            modelHandle = nullptr;
        }

        if (!logFile.empty()) propLog = std::ofstream();

        for (auto& pair : graphProperties) {
            auto& [name, prop] = pair;
            prop.get().reset();
        }

        for (auto& pair : algorithmProperties) {
            auto& [name, prop] = pair;
            prop.get().reset();
        }
    }

  private:
    void
    setupPredictor
    (const char * const lib, prop_set& graphProps, prop_set& algoProps)
    {
        typedef std::tuple<std::string,size_t,size_t,size_t> impl_tuple;
        typedef std::reference_wrapper<double> double_ref;
        typedef const std::vector<impl_tuple> implementations_t;
        typedef const std::map<std::string,double_ref> properties;

        modelHandle = dlopen(lib, RTLD_NOW);
        if (!modelHandle) {
            reportError("dlopen() failed: ", lib, "\n", dlerror());
        }

        lookup = safe_dlsym<int32_t()>(modelHandle, "lookup");
        auto& impls = *safe_dlsym<implementations_t>(modelHandle, "implNames");
        auto& params = *safe_dlsym<properties>(modelHandle, "propNames");

        bool missing = false;
        for (auto& pair : params) {
            auto& [name, prop] = pair;
            try {
                graphProperties[name] = prop;
                graphProps.erase(name);
            } catch (const std::out_of_range&) {
                try {
                    algorithmProperties[name] = prop;
                    algoProps.erase(name);
                } catch (const std::out_of_range&) {
                    std::cerr << "Missing property: " << name << std::endl;
                    missing = true;
                }
            }
        }

        implementations.resize(impls.size());
        for (auto& data : impls) {
            auto& [ name, idx, warpRef, chunkRef ] = data;
            try {
                implementations[idx] = { kernelMap.at(name) };

                auto warp = warpRef;
                auto chunk = chunkRef;
                auto setWarpReferences = [=](auto&& k) {
                    if (k->isWarp) k->setWarpConfig(warp, chunk);
                };

                mapKernels(setWarpReferences, implementations[idx]);
                if (name == "edge-list") {
                    kernels = implementations[idx];
                    defaultKernel = static_cast<int32_t>(idx);
                }
            } catch (const std::out_of_range&) {
                std::cerr << "Missing implementation: " << name << std::endl;
                missing = true;
            }
        }

        if (missing) reportError("Missing properties/implementations!");
    }

    void setupLogging(prop_set& missingGraphProps, prop_set& missingAlgoProps)
    {
        propLog = std::ofstream(logFile);
        propLog.imbue(std::locale("C"));
        lookup = [this,oldPredictor{lookup}]() {
            int32_t result = oldPredictor();
            propLog << "prediction:" << stepNum << ":" << result
                    << std::endl;
            return result;
        };

        std::vector<std::pair<std::string,double>> graphProps;
        graphProps.reserve(missingGraphProps.size());

        for (auto& name : missingGraphProps) {
            graphProps.emplace_back(name, 0);
            graphProperties[name] = std::ref(graphProps.back().second);
        }

        logGraphProps = [this,props{std::move(graphProps)}]() {
            for (auto& pair : graphProperties) {
                auto& [name, ref] = pair;
                propLog << "graph:" << name << ":" << ref.get()
                        << std::endl;
            }
        };

        std::vector<std::pair<std::string,double>> algoProps;
        algoProps.reserve(missingAlgoProps.size());

        for (auto& name : missingAlgoProps) {
            algoProps.emplace_back(name, 0);
            algorithmProperties[name] = std::ref(algoProps.back().second);
        }

        logAlgorithmProps = [this,props{std::move(algoProps)}]() {
            for (auto& pair : algorithmProperties) {
                auto& [name, ref] = pair;
                propLog << "step:" << stepNum << ":" << name << ":"
                        << ref.get() << std::endl;
            }
        };
    }

    template<typename Fun>
    void
    mapKernels(Fun f, std::tuple<Kernels...>& tuple)
    { mapKernels(f, tuple, std::index_sequence_for<Kernels...>()); }

    template<typename Fun, size_t... I>
    void
    mapKernels(Fun f, std::tuple<Kernels...>& ks, std::index_sequence<I...>)
    { (f(std::get<I>(ks)), ...); }

    std::string logFile;
    std::string model;

    void *modelHandle;
    std::function<int32_t()> lookup;

    std::ofstream propLog;
    std::function<void()> logGraphProps;
    std::function<void()> logAlgorithmProps;

    int32_t lastKernel;
    int32_t defaultKernel;

    std::vector<std::tuple<Kernels...>> implementations;
    std::map<std::string,std::tuple<Kernels...>> kernelMap;

    refmap<std::string,prop_ref> graphProperties;
    refmap<std::string,prop_ref> algorithmProperties;

    prop_ref vertices, edges;
    graph_prop min, lowerQuantile, mean, median, upperQuantile, max, stdDev;
    size_t stepNum;
};

template
< template<typename, typename, typename> class AlgorithmBase
, typename Platform
, typename Vertex
, typename Edge
, typename... KernelArgs
, typename... Kernels
, typename Impl = AlgorithmBase<Platform,Vertex,Edge>
>
std::unique_ptr<ImplementationBase>
make_switch_implementation
( KernelMap
  < std::string
  , std::tuple<GraphKernel<Platform,Vertex,Edge,KernelArgs...>,Kernels...>
  > ks
)
{
    using KernelRef = decltype(std::get<0>(ks[""]));
    using Kernel = typename std::remove_reference<KernelRef>::type;
    using SwitchImpl = SwitchImplementation<Impl,Kernel,Kernels...>;

    return std::make_unique<SwitchImpl>(ks);
}
#endif
