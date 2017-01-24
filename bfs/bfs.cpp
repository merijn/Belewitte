#include <fstream>

#include "CUDA.hpp"
#include "GraphLoading.hpp"
#include "TemplateConfig.hpp"
#include "Timer.hpp"

#include "bfs.h"

template<size_t warp, size_t chunk>
struct pushwarp {
    static auto work()
    { return WarpSettings(vertexPushWarpBfs<warp,chunk>, warp,
                          sizeof(push_warp_mem_t<chunk>));
    }
};

template<typename Platform, typename Kernel, typename Graph>
struct BFSConfig : public TemplateConfig<Platform,Kernel,unsigned,unsigned,Graph>
{
    using pair = std::pair<size_t,size_t>;
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

    unsigned root;

    BFSConfig
        ( const Options& opts
        , size_t count
        , LoadFun l
        , work_division w
        , Kernel kern
        )
    : Config(opts, count, l, w, kern), root(0)
    {
        options.add('r', "root", "NUM", root,
                    "Starting vertex for BFS.");
    }

    virtual void runImplementation() override
    {
        if (root >= vertex_count) return;

        Timer initResults("initResults", run_count);
        Timer bfs("computation", run_count);
        std::vector<Timer> levelTimers;
        std::vector<unsigned> frontiers(1000, 0);

        frontiers[0] = 1;
        levelTimers.reserve(1000);

        std::string timerName = std::to_string(root) + ":bfsLevel";
        for (int i = 0; i < 1000; i++) {
            levelTimers.emplace_back(timerName + std::to_string(i), run_count);
        }

        Timer resultTransfer("resultTransfer", run_count);

        auto results = backend.template alloc<int>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            backend.setWorkSizes(1,{nodeDivision.first},{nodeDivision.second});
            backend.runKernel(setArray, results, results.size,
                              std::numeric_limits<int>::max());

            backend.setWorkSizes(1, {1}, {1});
            backend.runKernel(set_root, results, root);
            initResults.stop();

            bfs.start();

            setKernelConfig();
            unsigned frontier;
            size_t oldLevel;
            int curr = 0;
            do {
                oldLevel = static_cast<size_t>(curr);
                resetFrontier();
                levelTimers[oldLevel].start();
                runKernel(kernel, results, curr++);
                frontier = getFrontier();
                levelTimers[oldLevel].stop();
                frontiers[static_cast<size_t>(curr)] = frontier;
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

            std::ofstream frontierOutput(outputFile + ".frontier");
            for (size_t i = 0; i < 1000; i++) {
                if (frontiers[i] == 0) break;
                frontierOutput << i << "\t" << frontiers[i] << std::endl;
            }
        }
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
    { "edge-list", make_config<BFSConfig>
        ( opts, count
        , loadEdgeList<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , edgeListBfs)
    },
    { "rev-edge-list", make_config<BFSConfig>
        ( opts, count
        , loadReverseEdgeList<CUDABackend, unsigned, unsigned>
        , work_division::edges
        , revEdgeListBfs)
    },
    { "vertex-push", make_config<BFSConfig>
        ( opts, count
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPushBfs)
    },
    { "vertex-pull", make_config<BFSConfig>
        ( opts, count
        , loadReverseCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , vertexPullBfs)
    },
    { "vertex-push-warp", make_warp_config<BFSConfig>
        ( opts, count
        , loadCSR<CUDABackend, unsigned, unsigned>
        , work_division::nodes
        , warp_dispatch<pushwarp>())
    }
    };
}
