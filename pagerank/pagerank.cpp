#include <fstream>
#include <iomanip>

#include "CUDA.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"

#include "pagerank.hpp"

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
    callConsolidate(const alloc_t<float>&, const alloc_t<float>&) = 0;

    virtual void runImplementation(std::ofstream& outputFile) override
    {
        Timer initResults("initResults", run_count);
        Timer pagerankTime("computation", run_count);
        Timer resultTransfer("resultTransfer", run_count);

        auto pageranks = backend.template alloc<float>(vertex_count);
        auto new_pageranks = backend.template alloc<float>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            setKernelConfig(work_division::vertex);
            backend.runKernel(setArrayFloat, pageranks, pageranks.size, 1.0f / vertex_count);
            backend.runKernel(setArrayFloat, new_pageranks, new_pageranks.size, 0.0f);
            initResults.stop();

            pagerankTime.start();

            float diff;
            int j = 0;
            do {
                j++;
                resetDiff();
                setKernelConfig(kernel);
                kernel->run(this->loader, pageranks, new_pageranks);
                callConsolidate(pageranks, new_pageranks);

                diff = getDiff();
            } while (j < max_iterations);
            pagerankTime.stop();

            resultTransfer.start();
            pageranks.copyDevToHost();
            resultTransfer.stop();
        }

        outputFile << std::scientific << std::setprecision(3);
        for (size_t i = 0; i < pageranks.size; i++) {
            outputFile << i << "\t" << pageranks[i] << std::endl;
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
    callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
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

    void callConsolidate(const alloc_t<float>& ranks, const alloc_t<float>& new_ranks) override
    {
        this->setKernelConfig(work_division::vertex);
        backend.runKernel(consolidate, this->loader.template getGraph<Rep::InverseVertexCSR,Dir::Reverse>(), ranks, new_ranks);
    }
};

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch(std::map<std::string, AlgorithmConfig*>& result)
{
    auto prMap = make_kernel_map<CUDABackend,unsigned,unsigned>
                        (&updateRankEdgeList);

    prMap.insert_kernel<Rep::EdgeListCSR>
        ("edge-list", updateRankEdgeList, work_division::edge);

    prMap.insert_kernel<Rep::StructEdgeListCSR>
        ("struct-edge-list", updateRankStructEdgeList, work_division::edge);

    prMap.insert_kernel<Rep::CSR>
        ("vertex-push", vertexPush, work_division::vertex);

    prMap.insert_kernel<Rep::InverseVertexCSR, Dir::Reverse>
        ("vertex-pull", vertexPull, work_division::vertex);

    prMap.insert_warp_kernel<Rep::CSR>
        ("vertex-push-warp", vertexPushWarp, work_division::vertex
        , [](size_t chunkSize) {
            return chunkSize * sizeof(float) + (1+chunkSize) * sizeof(unsigned);
        });

    prMap.insert_warp_kernel<Rep::InverseVertexCSR, Dir::Reverse>
        ("vertex-pull-warp", vertexPullWarp, work_division::vertex
        , [](size_t chunkSize) { return (1 + chunkSize) * sizeof(unsigned); });

    for (auto& pair : prMap) {
        result[pair.first] = make_config<PageRank>(pair.second, consolidateRank);
    }

    auto prNoDivMap = make_kernel_map<CUDABackend,unsigned,unsigned>
                        (vertexPullNoDiv);

    prNoDivMap.insert_kernel<Rep::InverseVertexCSR, Dir::Reverse>
        ("vertex-pull-nodiv", vertexPullNoDiv, work_division::vertex);

    prNoDivMap.insert_warp_kernel<Rep::InverseVertexCSR, Dir::Reverse>
        ("vertex-pull-warp-nodiv", vertexPullWarpNoDiv, work_division::vertex
        , [](size_t chunkSize) { return (1 + chunkSize) * sizeof(unsigned); });

    for (auto& pair : prNoDivMap) {
        result[pair.first] = make_config<PageRankNoDiv>(pair.second, consolidateRankNoDiv);
    }

    result["switch"] = make_switch_config<PageRank>(prMap, consolidateRank);
}
