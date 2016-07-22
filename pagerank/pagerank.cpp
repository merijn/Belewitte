#include <fstream>

#include "CUDA.hpp"
#include "GraphLoading.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"
#include "Util.hpp"

#include "pagerank.h"

template<size_t warp, size_t chunk>
struct pushwarp {
    static auto work()
    {
        return WarpSettings(vertexPushWarp<warp,chunk>, warp,
                            sizeof(push_warp_mem_t<chunk>));
    }
};

template<size_t warp, size_t chunk>
struct pullwarp {
    static auto work()
    {
        return WarpSettings(vertexPullWarp<warp,chunk>, warp,
                            sizeof(pull_warp_mem_t<chunk>));
    }
};

template<size_t warp, size_t chunk>
struct pullwarpnodiv {
    static auto work()
    {
        return WarpSettings(vertexPullWarpNoDiv<warp,chunk>, warp,
                            sizeof(pull_warp_mem_t<chunk>));
    }
};

template<typename Platform, typename Kernel, typename Graph>
struct PageRankBase : public TemplateConfig<Platform,Kernel,unsigned,unsigned,Graph>
{
    using Config = TemplateConfig<Platform,Kernel,unsigned,unsigned,Graph>;
    using Config::run_count;
    using Config::backend;
    using Config::nodeDivision;
    using Config::outputFile;
    using Config::setKernelConfig;
    using Config::vertex_count;
    using Config::runKernel;
    using Config::kernel;
    using Config::options;
    using typename Config::LoadFun;

    template<typename T>
    using alloc_t = typename Platform::template alloc_t<T>;

    PageRankBase
        ( const Options& opts
        , size_t count
        , std::string outputFile
        , LoadFun l
        , work_division w
        , Kernel kern
        )
    : Config(opts, count, outputFile, l, w, kern)
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

template<typename Platform,typename Kernel,typename Graph,typename Consolidate>
struct PageRank : public PageRankBase<Platform, Kernel, Graph> {
    PageRank
        ( const Options& opts
        , size_t count
        , std::string outputFile
        , typename PageRankBase<Platform, Kernel, Graph>::LoadFun l
        , work_division w
        , Kernel kern
        , Consolidate c
        )
     : PageRankBase<Platform,Kernel,Graph>(opts, count, outputFile, l, w, kern)
     , consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename Platform::template alloc_t<T>;

    void
    callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
    {
        this->backend.setWorkSizes(1,{this->nodeDivision.first},{this->nodeDivision.second});
        this->backend.runKernel(consolidate, this->vertex_count, ranks, new_ranks);
    }
};

template<typename Platform,typename Kernel,typename Graph,typename Consolidate>
struct PageRankNoDiv : public PageRankBase<Platform, Kernel, Graph> {
    PageRankNoDiv
        ( const Options& opts
        , size_t count
        , std::string outputFile
        , typename PageRankBase<Platform, Kernel, Graph>::LoadFun l
        , work_division w
        , Kernel kern
        , Consolidate c
        )
     : PageRankBase<Platform,Kernel,Graph>(opts, count, outputFile, l, w, kern)
     , consolidate(c)
    {}

    Consolidate consolidate;

    template<typename T>
    using alloc_t = typename Platform::template alloc_t<T>;

    void callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
    {
        this->backend.setWorkSizes(1, {this->nodeDivision.first}, {this->nodeDivision.second});
        this->runKernel(consolidate, ranks, new_ranks);
    }
};

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch
    ( std::map<std::string, AlgorithmConfig*>& result
    , const Options& opts
    , size_t count
    , std::string outputFile
    )
{
    result = {
    { "edge-list", make_config<PageRank>
        ( opts, count, outputFile
        , loadEdgeListCSR<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , updateRankEdgeList
        , consolidateRank)
    },
    { "struct-edge-list", make_config<PageRank>
        ( opts, count, outputFile
        , loadStructEdgeListCSR<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , updateRankStructEdgeList
        , consolidateRank)
    },
    { "vertex-push", make_config<PageRank>
        ( opts, count, outputFile
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPush
        , consolidateRank)
    },
    { "vertex-pull", make_config<PageRank>
        ( opts, count, outputFile
        , loadReverseCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPull
        , consolidateRank)
    },
    { "vertex-push-warp", make_warp_config<PageRank>
        ( opts, count, outputFile
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , warp_dispatch<pushwarp>()
        , consolidateRank)
    },
    { "vertex-pull-warp", make_warp_config<PageRank>
        ( opts, count, outputFile
        , loadReverseCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , warp_dispatch<pullwarp>()
        , consolidateRank)
    },
    { "vertex-pull-nodiv", make_config<PageRankNoDiv>
        ( opts, count, outputFile
        , loadReverseCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPullNoDiv
        , consolidateRankPull)
    },
    { "vertex-pull-warp-nodiv", make_warp_config<PageRankNoDiv>
        ( opts, count, outputFile
        , loadReverseCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , warp_dispatch<pullwarpnodiv>()
        , consolidateRankPull)
    }
    };
}
