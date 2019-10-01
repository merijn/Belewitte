#include <fstream>
#include <iomanip>

#include "Algorithm.hpp"
#include "CUDA.hpp"
#include "ImplementationTemplate.hpp"
#include "Timer.hpp"

#include "pagerank.hpp"

static inline float
roundPrecision(float val, int digitPrecision)
{
    float rounder = std::pow(10.0f, static_cast<float>(digitPrecision));
    return std::round(val * rounder) / rounder;
}

template<typename Platform, typename Vertex, typename Edge>
struct PageRank : public ImplementationTemplate<Platform,Vertex,Edge>
{
    using Impl = ImplementationTemplate<Platform,Vertex,Edge>;
    using Impl::run_count;
    using Impl::backend;
    using Impl::setKernelConfig;
    using Impl::vertex_count;
    using Impl::options;
    using Impl::isSwitching;

    template<typename T>
    using alloc_t = typename Impl::template alloc_t<T>;

    template<typename... Args>
    using Kernel = typename Impl::template GraphKernel<Args...>;

    Kernel<unsigned*> zeroInitDegrees;
    Kernel<unsigned*> computeDegrees;
    Kernel<unsigned*,float*,float*> kernel;
    Kernel<unsigned*,float*,float*,bool> consolidate;

    PageRank
    ( Kernel<unsigned*,float*,float*> k
    , Kernel<unsigned*,float*,float*,bool> c
    , Kernel<unsigned*> zeroInit
    , Kernel<unsigned*> compute
    )
      : zeroInitDegrees(zeroInit), computeDegrees(compute)
      , kernel(k), consolidate(c)
    {}

    virtual void runImplementation(std::ofstream& outputFile) override
    {
        Timer initResults("initResults", run_count);
        Timer pagerankTime("computation", run_count);
        // Step timer is redundant/pointless for PageRank, but needed for
        // consistency with other algorithms
        Timer pagerankStepTime("0:computation", run_count);
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
            pagerankStepTime.start();

            if constexpr (isSwitching) {
                this->predictInitial();
            }

            uint64_t degreeBufferSize = 0;
            if (zeroInitDegrees || computeDegrees) {
                degreeBufferSize = vertex_count;
            }
            auto degrees = backend.template alloc<unsigned>(degreeBufferSize);

            if (zeroInitDegrees) setKernelConfig(zeroInitDegrees);
            zeroInitDegrees(this->loader, degrees);

            setKernelConfig(computeDegrees);
            computeDegrees(this->loader, degrees);

            float diff;
            int j = 0;
            do {
                j++;
                resetDiff();
                setKernelConfig(kernel);
                kernel(this->loader, degrees, pageranks, new_pageranks);
                setKernelConfig(consolidate);
                consolidate(this->loader, degrees, pageranks, new_pageranks, max_iterations > j);

                diff = getDiff();
            } while (j < max_iterations);
            pagerankStepTime.stop();
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

extern "C" register_algorithm_t registerCUDA;
extern "C" void registerCUDA(Algorithm& result)
{
    INITIALISE_ALGORITHM(result);
    KernelBuilder<CUDABackend,unsigned,unsigned> make_kernel;

    auto zeroInitDegrees = make_kernel
        ( zeroInitDegreesKernel
        , work_division::vertex
        , tag_t(Rep::VertexCount)
        );

    auto reverseCSRDegrees = make_kernel
        ( reverseCSRComputeDegrees
        , work_division::vertex
        , tag_t(Rep::CSR)
        , tag_t(Dir::Reverse)
        );

    auto consolidate = make_kernel
        ( consolidateRank
        , work_division::vertex
        , tag_t(Rep::VertexCount)
        );

    auto consolidateNoDiv = make_kernel
        ( consolidateRankNoDiv
        , work_division::vertex
        , tag_t(Rep::VertexCount)
        );

    KernelMap prMap
    { std::pair
        { "edge-list"
        , std::tuple
            { make_kernel
                ( edgeListPageRank
                , work_division::edge
                , tag_t(Rep::EdgeList)
                )
            , consolidate
            , zeroInitDegrees
            , make_kernel
                ( edgeListComputeDegrees
                , work_division::edge
                , tag_t(Rep::EdgeList)
                )
            }
        }
    };

    prMap["struct-edge-list"] =
        { make_kernel
            ( structEdgeListPageRank
            , work_division::edge
            , tag_t(Rep::StructEdgeList)
            )
        , consolidate
        , zeroInitDegrees
        , make_kernel
            ( structEdgeListComputeDegrees
            , work_division::edge
            , tag_t(Rep::StructEdgeList)
            )
        };

    prMap["vertex-push"] = std::make_tuple
        ( make_kernel
            ( vertexPushPageRank
            , work_division::vertex
            , tag_t(Rep::CSR)
            )
        , consolidate
        , nullptr
        , nullptr
        );

    prMap["vertex-pull"] =
        { make_kernel
            ( vertexPullPageRank
            , work_division::vertex
            , tag_t(Rep::CSR)
            , tag_t(Dir::Reverse)
            )
        , consolidate
        , zeroInitDegrees
        , reverseCSRDegrees
        };

    prMap["vertex-push-warp"] = std::make_tuple
        ( make_kernel
            ( vertexPushWarpPageRank
            , work_division::vertex
            , [](size_t chunkSize) {
                return chunkSize * sizeof(float) + (1+chunkSize) * sizeof(unsigned);
            }
            , tag_t(Rep::CSR)
            )
        , consolidate
        , nullptr
        , nullptr
        );

    prMap["vertex-pull-warp"] =
        { make_kernel
            ( vertexPullWarpPageRank
            , work_division::vertex
            , [](size_t chunkSize) {
                return (1 + chunkSize) * sizeof(unsigned);
            }
            , tag_t(Rep::CSR)
            , tag_t(Dir::Reverse)
            )
        , consolidate
        , zeroInitDegrees
        , reverseCSRDegrees
        };

    prMap["vertex-pull-nodiv"] =
        { make_kernel
            ( vertexPullNoDivPageRank
            , work_division::vertex
            , tag_t(Rep::CSR)
            , tag_t(Dir::Reverse)
            )
        , consolidateNoDiv
        , zeroInitDegrees
        , reverseCSRDegrees
        };

    prMap["vertex-pull-warp-nodiv"] =
        { make_kernel
            ( vertexPullWarpNoDivPageRank
            , work_division::vertex
            , [](size_t chunkSize) {
                return (1 + chunkSize) * sizeof(unsigned);
            }
            , tag_t(Rep::CSR)
            , tag_t(Dir::Reverse)
            )
        , consolidateNoDiv
        , zeroInitDegrees
        , reverseCSRDegrees
        };

    for (auto& [name, kernel] : prMap) {
        result.addImplementation(name, make_implementation<PageRank>(kernel));
    }

    result.addImplementation("switch", make_switch_implementation<PageRank>(prMap));
}
