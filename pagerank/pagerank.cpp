#include <fstream>
#include <iomanip>

#include "CUDA.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"

#include "pagerank.hpp"

static inline float
roundPrecision(float val, int digitPrecision)
{
    float rounder = std::pow(10.0f, static_cast<float>(digitPrecision));
    return std::round(val * rounder) / rounder;
}

template<typename Config>
struct PageRankBase : public Config
{
    using typename Config::ConfigArg;
    using Config::run_count;
    using Config::backend;
    using Config::setKernelConfig;
    using Config::vertex_count;
    using Config::kernel;
    using Config::options;

    template<typename T>
    using alloc_t = typename Config::template alloc_t<T>;

    PageRankBase(ConfigArg k) : Config(k)
    {}

    virtual void
    callConsolidate(const alloc_t<float>&, const alloc_t<float>&, bool) = 0;

    virtual void runImplementation(std::ofstream& outputFile) override
    {
        Timer initResults("initResults", run_count);
        Timer pagerankTime("computation", run_count);
        Timer resultTransfer("resultTransfer", run_count);

        auto pageranks = backend.template alloc<float>(vertex_count);
        auto new_pageranks = backend.template alloc<float>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            std::fill(pageranks.begin(), pageranks.end(), 1.0f / vertex_count);
            pageranks.copyHostToDev();

            std::fill(new_pageranks.begin(), new_pageranks.end(), 0.0f);
            new_pageranks.copyHostToDev();
            initResults.stop();

            pagerankTime.start();

            float diff;
            int j = 0;
            do {
                j++;
                resetDiff();
                setKernelConfig(kernel);
                kernel->run(this->loader, pageranks, new_pageranks);
                callConsolidate(pageranks, new_pageranks, max_iterations > j);

                diff = getDiff();
            } while (j < max_iterations);
            pagerankTime.stop();

            resultTransfer.start();
            pageranks.copyDevToHost();
            resultTransfer.stop();
        }

        outputFile << std::scientific << std::setprecision(2);
        for (size_t i = 0; i < pageranks.size; i++) {
            outputFile << i << "\t" << roundPrecision(pageranks[i], 5) << endl;
        }
    }
};

template<typename Config, typename Consolidate>
struct PageRank : public PageRankBase<Config> {
    using typename PageRankBase<Config>::ConfigArg;
    using PageRankBase<Config>::backend;

    PageRank(ConfigArg k, Consolidate c)
     : PageRankBase<Config>(k), consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename PageRankBase<Config>::template alloc_t<T>;

    void
    callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks, bool) override
    {
        this->setKernelConfig(work_division::vertex);
        backend.runKernel(consolidate, this->vertex_count, ranks, new_ranks);
    }
};

template<typename Config,typename Consolidate>
struct PageRankNoDiv : public PageRankBase<Config> {
    using typename PageRankBase<Config>::ConfigArg;
    using PageRankBase<Config>::backend;
    using PageRankBase<Config>::kernel;

    PageRankNoDiv(ConfigArg k, Consolidate c)
     : PageRankBase<Config>(k), consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename PageRankBase<Config>::template alloc_t<T>;

    void callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks, bool notLast) override
    {
        this->setKernelConfig(work_division::vertex);
        backend.runKernel(consolidate, this->loader.template getGraph<Rep::InverseVertexCSR,Dir::Reverse>(), ranks, new_ranks, notLast);
    }
};

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch(std::map<std::string, AlgorithmConfig*>& result)
{
    KernelBuilder<CUDABackend,unsigned,unsigned> make_kernel;
    WarpKernelBuilder<CUDABackend,unsigned,unsigned> make_warp_kernel;

    KernelMap prMap
    { make_kernel_pair
        ( "edge-list"
        , edgeListCSR, work_division::edge, Rep::EdgeListCSR)
    , make_kernel_pair
        ( "struct-edge-list"
        , structEdgeListCSR, work_division::edge, Rep::StructEdgeListCSR)
    , make_kernel_pair
        ( "vertex-push"
        , vertexPush, work_division::vertex, Rep::CSR)
    , make_kernel_pair
        ( "vertex-pull"
        , vertexPull, work_division::vertex, Rep::InverseVertexCSR
        , Dir::Reverse)
    , make_warp_kernel_pair
        ( "vertex-push-warp"
        , vertexPushWarp, work_division::vertex
        , [](size_t chunkSize)
          { return chunkSize * sizeof(float) + (1+chunkSize) * sizeof(unsigned); }
        , Rep::CSR)
    , make_warp_kernel_pair
        ( "vertex-pull-warp"
        , vertexPullWarp, work_division::vertex
        , [](size_t chunkSize)
          { return (1 + chunkSize) * sizeof(unsigned); }
          , Rep::InverseVertexCSR, Dir::Reverse)
    };

    for (auto& pair : prMap) {
        result[pair.first] = make_config<PageRank>(pair.second, consolidateRank);
    }

    KernelMap prNoDivMap
    { make_kernel_pair
        ( "vertex-pull-nodiv"
        , vertexPullNoDiv, work_division::vertex, Rep::InverseVertexCSR
        , Dir::Reverse)
    , make_warp_kernel_pair
        ( "vertex-pull-warp-nodiv"
        , vertexPullWarpNoDiv, work_division::vertex
        , [](size_t chunkSize) { return (1 + chunkSize) * sizeof(unsigned); }
        , Rep::InverseVertexCSR, Dir::Reverse)
    };


    for (auto& pair : prNoDivMap) {
        result[pair.first] = make_config<PageRankNoDiv>(pair.second, consolidateRankNoDiv);
    }

    result["switch"] = make_switch_config<PageRank>(prMap, consolidateRank);
}
