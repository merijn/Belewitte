#include <fstream>

#include "CUDA.hpp"
#include "GraphLoading.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"
#include "Util.hpp"

#include "pagerank.hpp"

template<typename Config>
struct PageRankBase : public Config
{
    using Config::run_count;
    using Config::backend;
    using Config::nodeDivision;
    using Config::outputFile;
    using Config::setKernelConfig;
    using Config::vertex_count;
    using Config::runKernel;
    using Config::kernel;
    using Config::options;
    using typename Config::Kernel;
    using typename Config::LoadFun;

    template<typename T>
    using alloc_t = typename Config::template alloc_t<T>;

    template<typename... Args>
    PageRankBase(Args... args)
    : Config(args...)
    {}

    virtual void
    callConsolidate(const alloc_t<float>&, const alloc_t<float>&) = 0;

    virtual void runImplementation() override
    {
        Timer initResults("initResults", run_count);
        Timer pagerankTime("computation", run_count);
        Timer resultTransfer("resultTransfer", run_count);

        auto pageranks = backend.template alloc<float>(vertex_count);
        auto new_pageranks = backend.template alloc<float>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            backend.setWorkSizes(1,{nodeDivision.first},{nodeDivision.second});
            backend.runKernel(setArrayFloat, pageranks, pageranks.size, 1.0f / vertex_count);
            backend.runKernel(setArrayFloat, new_pageranks, new_pageranks.size, 0.0f);
            initResults.stop();

            pagerankTime.start();

            float diff;
            int j = 0;
            do {
                j++;
                resetDiff();
                setKernelConfig();
                runKernel(kernel, pageranks, new_pageranks);
                callConsolidate(pageranks, new_pageranks);

                diff = getDiff();
            } while (j < max_iterations);
            pagerankTime.stop();

            resultTransfer.start();
            pageranks.copyDevToHost();
            resultTransfer.stop();
        }

        if (!outputFile.empty()) {
            std::ofstream output(outputFile);
            for (size_t i = 0; i < pageranks.size; i++) {
                output << i << "\t" << pageranks[i] << std::endl;
            }
        }
    }
};

template<typename Config, typename Consolidate>
struct PageRank : public PageRankBase<Config> {
    template<typename... Args>
    PageRank(Consolidate c, Args... args)
     : PageRankBase<Config>(args...)
     , consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename PageRankBase<Config>::template alloc_t<T>;

    void
    callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
    {
        this->backend.setWorkSizes(1,{this->nodeDivision.first},{this->nodeDivision.second});
        this->backend.runKernel(consolidate, this->vertex_count, ranks, new_ranks);
    }
};

template<typename Config,typename Consolidate>
struct PageRankNoDiv : public PageRankBase<Config> {
    template<typename... Args>
    PageRankNoDiv(Consolidate c, Args... args)
     : PageRankBase<Config>(args...)
     , consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename PageRankBase<Config>::template alloc_t<T>;

    void callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
    {
        this->backend.setWorkSizes(1, {this->nodeDivision.first}, {this->nodeDivision.second});
        this->runNonWarpKernel(consolidate, ranks, new_ranks);
    }
};

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch
    ( std::map<std::string, AlgorithmConfig*>& result
    , const Options& opts
    , size_t count
    )
{
    result = {
    { "edge-list", make_config<PageRank>
        ( opts, count
        , loadEdgeListCSR<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , updateRankEdgeList
        , consolidateRank)
    },
    { "struct-edge-list", make_config<PageRank>
        ( opts, count
        , loadStructEdgeListCSR<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , updateRankStructEdgeList
        , consolidateRank)
    },
    { "vertex-push", make_config<PageRank>
        ( opts, count
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPush
        , consolidateRank)
    },
    { "vertex-pull", make_config<PageRank>
        ( opts, count
        , loadReversedCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPull
        , consolidateRank)
    },
    { "vertex-push-warp", make_warp_config<PageRank>
        ( opts, count
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPushWarp
        , [](size_t chunkSize) {
            return chunkSize * sizeof(float) + (1+chunkSize) * sizeof(unsigned);
        }
        , consolidateRank)
    },
    { "vertex-pull-warp", make_warp_config<PageRank>
        ( opts, count
        , loadReversedCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPullWarp
        , [](size_t chunkSize) { return (1 + chunkSize) * sizeof(unsigned); }
        , consolidateRank)
    },
    { "vertex-pull-nodiv", make_config<PageRankNoDiv>
        ( opts, count
        , loadReversedCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPullNoDiv
        , consolidateRankPull)
    },
    { "vertex-pull-warp-nodiv", make_warp_config<PageRankNoDiv>
        ( opts, count
        , loadReversedCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPullWarpNoDiv
        , [](size_t chunkSize) { return (1 + chunkSize) * sizeof(unsigned); }
        , consolidateRankPull)
    }
    };
}
