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

template<size_t warp, size_t chunk>
struct pullwarp {
    static auto work()
    { return WarpSettings(vertexPullWarpBfs<warp,chunk>, warp,
                          sizeof(pull_warp_mem_t<chunk>));
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

    int root;

    BFSConfig
        ( const Options& opts
        , size_t count
        , LoadFun l
        , work_division w
        , Kernel kern
        )
    : Config(opts, count, l, w, kern)
    {
        options.add('r', "root", "NUM", root,
                    "Starting vertex for BFS.");
    }

    virtual void runImplementation() override
    {
        Timer initResults("initResults", run_count);
        Timer bfs("computation", run_count);
        Timer resultTransfer("resultTransfer", run_count);

        auto results = backend.template alloc<int>(vertex_count);

        for (size_t i = 0; i < run_count; i++) {
            initResults.start();
            backend.setWorkSizes(1,{nodeDivision.first},{nodeDivision.second});
            backend.runKernel(setArray, results, results.size,
                              std::numeric_limits<int>::max());

            root = 0;
            backend.setWorkSizes(1, {1}, {1});
            backend.runKernel(set_root, results, 0);
            initResults.stop();

            bfs.start();

            setKernelConfig();
            bool val;
            int curr = 0;
            do {
                resetFinished();
                runKernel(kernel, results, curr++);
                val = getFinished();
            } while (!val);
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
