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
    using Config::outputFile;
    using Config::loader;
    using Config::setKernelConfig;
    using Config::vertex_count;
    using Config::kernel;
    using Config::options;
    using Config::isSwitching;

    unsigned root;

    prop_ref frontierSize;

    BFSConfig(ConfigArg k)
    : Config(k), root(0), frontierSize("frontier", *this)
    { options.add('r', "root", "NUM", root, "Starting vertex for BFS."); }

    inline void setProps(unsigned frontier)
    { frontierSize = frontier; }

    virtual void runImplementation() override
    {
        if (root >= vertex_count) return;

        Timer initResults("initResults", run_count);
        Timer bfs("computation", run_count);
        std::vector<Timer> levelTimers;
        levelTimers.reserve(1000);

        std::string timerName = std::to_string(root) + ":bfsLevel";
        for (int i = 0; i < 1000; i++) {
            levelTimers.emplace_back(timerName + std::to_string(i), run_count);
        }

        Timer resultTransfer("resultTransfer", run_count);

        auto results = backend.template alloc<int>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            setKernelConfig(work_division::vertex);
            backend.runKernel(setArray, results, results.size,
                              std::numeric_limits<int>::max());

            backend.setWorkSizes(1, {1}, {1});
            backend.runKernel(set_root, results, root);
            initResults.stop();

            bfs.start();

            unsigned frontier = 1;
            int curr = 0;

            if constexpr (isSwitching) {
                setProps(frontier);
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

        if (!outputFile.empty()) {
            std::ofstream output(outputFile);
            for (size_t i = 0; i < results.size; i++) {
                output << i << "\t" << results[i] << std::endl;
            }
        }
    }
};

template<bfs_variant Variant, typename T>
static inline void
insertVariant(T& kernelMap)
{
    kernelMap.template insert_kernel<Rep::EdgeList>
        ( std::string("edge-list") + BFS<Variant>::suffix
        , edgeListBfs<BFS<Variant>>, work_division::edge);

    kernelMap.template insert_kernel<Rep::EdgeList,Dir::Reverse>
        ( std::string("rev-edge-list") + BFS<Variant>::suffix
        , revEdgeListBfs<BFS<Variant>>, work_division::edge);

    kernelMap.template insert_kernel<Rep::CSR>
        ( std::string("vertex-push") + BFS<Variant>::suffix
        , vertexPushBfs<BFS<Variant>>, work_division::vertex);

    kernelMap.template insert_kernel<Rep::CSR,Dir::Reverse>
        ( std::string("vertex-pull") + BFS<Variant>::suffix
        , vertexPullBfs<BFS<Variant>>, work_division::vertex);

    kernelMap.template insert_warp_kernel<Rep::CSR>
        ( std::string("vertex-push-warp") + BFS<Variant>::suffix
        , vertexPushWarpBfs<BFS<Variant>>
        , work_division::vertex
        , [](size_t chunkSize) {
            return chunkSize * sizeof(int) + (chunkSize+1) * sizeof(unsigned);
        });
}

extern "C" kernel_register_t cudaDispatch;
extern "C"
void
cudaDispatch(std::map<std::string, AlgorithmConfig*>& result)
{
    auto kernelMap = make_kernel_map<CUDABackend,unsigned,unsigned>
                        (edgeListBfs<BFS<normal>>);

    insertVariant<normal>(kernelMap);
    insertVariant<bulk>(kernelMap);
    insertVariant<warpreduce>(kernelMap);
    insertVariant<blockreduce>(kernelMap);

    for (auto& pair : kernelMap) {
        result[pair.first] = make_config<BFSConfig>(pair.second);
    }

    result["switch"] = make_switch_config<BFSConfig>(kernelMap);
}
