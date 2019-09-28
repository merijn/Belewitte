#include <fstream>

#include "Algorithm.hpp"
#include "CUDA.hpp"
#include "ImplementationTemplate.hpp"
#include "Timer.hpp"

#include "bfs.hpp"

template<typename Platform, typename Vertex, typename Edge>
struct BFS : public ImplementationTemplate<Platform,Vertex,Edge>
{
    using Impl = ImplementationTemplate<Platform,Vertex,Edge>;
    using Impl::run_count;
    using Impl::backend;
    using Impl::loader;
    using Impl::setKernelConfig;
    using Impl::vertex_count;
    using Impl::options;
    using Impl::isSwitching;

    template<typename T>
    using alloc_t = typename Impl::template alloc_t<T>;

    template<typename... Args>
    using Kernel = typename Impl::template GraphKernel<Args...>;

    unsigned root;

    prop_ref absFrontier, relFrontier, absVisited, relVisited;

    Kernel<int*,int> kernel;

    BFS(Kernel<int*,int> k)
    : root(0)
    , absFrontier("frontier abs", *this)
    , relFrontier("frontier rel", *this)
    , absVisited("visited abs", *this)
    , relVisited("visited rel", *this)
    , kernel(k)
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
                kernel(loader, results, curr++);
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

    KernelMap kernelMap
    { std::pair
        { std::string("edge-list") + Reduction<Variant>::suffix
        , std::tuple
            { make_kernel
                ( edgeListBfs<Reduction<Variant>>
                , work_division::edge
                , tag_t(Rep::EdgeList)
                )
            }
        }
    };

    kernelMap[std::string("rev-edge-list") + Reduction<Variant>::suffix] = {
        make_kernel
            ( revEdgeListBfs<Reduction<Variant>>
            , work_division::edge
            , tag_t(Rep::EdgeList)
            , tag_t(Dir::Reverse)
            )
    };

    kernelMap[std::string("struct-edge-list") + Reduction<Variant>::suffix] = {
        make_kernel
            ( structEdgeListBfs<Reduction<Variant>>
            , work_division::edge
            , tag_t(Rep::StructEdgeList)
            )
    };

    kernelMap[std::string("rev-struct-edge-list") + Reduction<Variant>::suffix] = {
        make_kernel
            ( revStructEdgeListBfs<Reduction<Variant>>
            , work_division::edge
            , tag_t(Rep::StructEdgeList)
            , tag_t(Dir::Reverse)
            )
    };

    kernelMap[std::string("vertex-push") + Reduction<Variant>::suffix] = {
        make_kernel
            ( vertexPushBfs<Reduction<Variant>>
            , work_division::vertex
            , tag_t(Rep::CSR)
            )
    };

    kernelMap[std::string("vertex-pull") + Reduction<Variant>::suffix] = {
        make_kernel
            ( vertexPullBfs<Reduction<Variant>>
            , work_division::vertex
            , tag_t(Rep::CSR)
            , tag_t(Dir::Reverse)
            )
    };

    kernelMap[std::string("vertex-push-warp") + Reduction<Variant>::suffix] = {
        make_kernel
            ( vertexPushWarpBfs<Reduction<Variant>>
            , work_division::vertex
            , [](size_t chunkSize) {
                return chunkSize * sizeof(int) + (chunkSize+1) * sizeof(unsigned);
            }
            , tag_t(Rep::CSR)
            )
    };

    kernelMap[std::string("vertex-pull-warp") + Reduction<Variant>::suffix] = {
        make_kernel
            ( vertexPullWarpBfs<Reduction<Variant>>
            , work_division::vertex
            , [](size_t chunkSize) {
                return chunkSize * sizeof(int) + (chunkSize+1) * sizeof(unsigned);
            }
            , tag_t(Rep::CSR)
            )
    };

    return kernelMap;
}

extern "C" register_algorithm_t registerCUDA;
extern "C" void registerCUDA(Algorithm& result)
{
    INITIALISE_ALGORITHM(result);

    auto kernelMap = insertVariant<normal>();
    kernelMap += insertVariant<bulk>();
    kernelMap += insertVariant<warpreduce>();
    kernelMap += insertVariant<blockreduce>();

    for (auto& [name, kernel] : kernelMap) {
        result.addImplementation(name, make_implementation<BFS>(kernel));
    }

    result.addImplementation("switch", make_switch_implementation<BFS>(kernelMap));
}
