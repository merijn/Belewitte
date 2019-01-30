#include <fstream>

#include "CUDA.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"

#include "bfs.hpp"

template<typename Config>
struct BFSConfig : public Config
{
    using typename Config::ConfigArg;
    using Config::run_count;
    using Config::backend;
    using Config::loader;
    using Config::setKernelConfig;
    using Config::vertex_count;
    using Config::kernel;
    using Config::options;
    using Config::isSwitching;

    unsigned root;

    prop_ref absFrontier, relFrontier, absVisited, relVisited;

    BFSConfig(ConfigArg k)
    : Config(k), root(0)
    , absFrontier("frontier abs", *this)
    , relFrontier("frontier rel", *this)
    , absVisited("visited abs", *this)
    , relVisited("visited rel", *this)
    { options.add('r', "root", "NUM", root, "Starting vertex for BFS."); }

    inline void setProps(unsigned frontier)
    {
        absVisited += absFrontier.get();
        relVisited = absVisited.get() / vertex_count;
        absFrontier = frontier;
        relFrontier = absFrontier.get() / vertex_count;
    }

    virtual void runImplementation(std::ofstream& outputFile) override
    {
        if (root >= vertex_count) return;

        Timer initResults("initResults", run_count);
        Timer bfs("computation", run_count);
        std::vector<Timer> levelTimers;
        levelTimers.reserve(1000);

        std::string timerName = ":bfsLevel";
        for (int i = 0; i < 1000; i++) {
            levelTimers.emplace_back(std::to_string(i) + timerName, run_count);
        }

        Timer resultTransfer("resultTransfer", run_count);

        auto results = backend.template alloc<int>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            std::fill(results.begin(), results.end(),
                      std::numeric_limits<int>::max());

            results[root] = 0;
            results.copyHostToDev();
            initResults.stop();

            bfs.start();

            unsigned frontier = 1;
            int curr = 0;

            if constexpr (isSwitching) {
                absFrontier = 1;
                relFrontier = 1.0 / vertex_count;
                absVisited = 0;
                relVisited = 0;
                this->predictInitial();
            } else {
                setKernelConfig(kernel);
            }

            do {
                auto& levelTimer = levelTimers[static_cast<size_t>(curr)];
                resetFrontier();
                levelTimer.start();
                kernel->run(loader, results, curr++);
                frontier = getFrontier();
                levelTimer.stop();

                if constexpr (isSwitching) {
                    setProps(frontier);
                    this->predict();
                }
            } while (frontier);
            bfs.stop();

            resultTransfer.start();
            results.copyDevToHost();
            resultTransfer.stop();
        }

        for (size_t i = 0; i < results.size; i++) {
            outputFile << i << "\t" << results[i] << std::endl;
        }
    }
};

template<bfs_variant Variant>
static inline auto
insertVariant()
{
    KernelBuilder<CUDABackend,unsigned,unsigned> make_kernel;
    WarpKernelBuilder<CUDABackend,unsigned,unsigned> make_warp_kernel;

    KernelMap kernelMap
    { make_kernel_pair
        ( std::string("edge-list") + BFS<Variant>::suffix
        , edgeListBfs<BFS<Variant>>, work_division::edge, Rep::EdgeList)
    , make_kernel_pair
        ( std::string("rev-edge-list") + BFS<Variant>::suffix
        , revEdgeListBfs<BFS<Variant>>, work_division::edge, Rep::EdgeList
        , Dir::Reverse)
    , make_kernel_pair
        ( std::string("vertex-push") + BFS<Variant>::suffix
        , vertexPushBfs<BFS<Variant>>, work_division::vertex, Rep::CSR)
    , make_kernel_pair
        ( std::string("vertex-pull") + BFS<Variant>::suffix
        , vertexPullBfs<BFS<Variant>>, work_division::vertex, Rep::CSR
        , Dir::Reverse)
    , make_warp_kernel_pair
        ( std::string("vertex-push-warp") + BFS<Variant>::suffix
        , vertexPushWarpBfs<BFS<Variant>>, work_division::vertex
        , [](size_t chunkSize)
          { return chunkSize * sizeof(int) + (chunkSize+1) * sizeof(unsigned); }
        , Rep::CSR)
    };

    return kernelMap;
}

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch(std::map<std::string, AlgorithmConfig*>& result)
{
    auto kernelMap = insertVariant<normal>();
    kernelMap += insertVariant<bulk>();
    kernelMap += insertVariant<warpreduce>();
    kernelMap += insertVariant<blockreduce>();

    for (auto& pair : kernelMap) {
        result[pair.first] = make_config<BFSConfig>(pair.second);
    }

    result["switch"] = make_switch_config<BFSConfig>(kernelMap);
}
