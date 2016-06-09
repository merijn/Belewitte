#include <fstream>

#include "Interface.hpp"
#include "WarpDispatch.hpp"

#include "pagerank.hpp"
#include "pagerank.h"

template<typename Platform>
struct PageRankImpl {
    private:
        Backend<Platform>& backend;
        TimerRegister& timers;
        size_t run_count;
        const char *outputFile;
        size_t vertex_count;

    public:
        PageRankImpl
        ( Backend<Platform> &b
        , TimerRegister& ts
        , size_t count
        , const char *output
        , size_t vertices
        ) : backend(b), timers(ts), run_count(count), outputFile(output)
          , vertex_count(vertices)
        {}

        template<typename K, typename G, typename... Graph, typename... Foo>
        void runKernel
            ( Bind<K, Graph...> kernel
            , Bind<G, Foo...> consolidate
            , std::function<void()> transfer
            )
        {
            Timer graphTransfer(timers, "graphTransfer", run_count);
            Timer initResults(timers, "initResults", run_count);
            Timer pagerankTime(timers, "pagerank", run_count);
            Timer resultTransfer(timers, "resultTransfer", run_count);

            auto pageranks = backend.template alloc<float>(vertex_count);
            auto new_pageranks = backend.template alloc<float>(vertex_count);

            graphTransfer.start();
            transfer();
            graphTransfer.stop();

            auto nodeSizes = backend.computeDivision(vertex_count);

            for (size_t i = 0; i < run_count; i++) {
                initResults.start();
                backend.setWorkSizes(1, {nodeSizes.first}, {nodeSizes.second});
                backend.runKernel(setArrayFloat, pageranks, pageranks->size, 1.0f / vertex_count);
                backend.runKernel(setArrayFloat, new_pageranks, new_pageranks->size, 0.0f);
                initResults.stop();

                pagerankTime.start();

                float diff;
                int j = 0;
                do {
                    j++;
                    resetDiff();
                    kernel.call(backend, pageranks, new_pageranks);

                    consolidate.call(backend, vertex_count, pageranks, new_pageranks);
                    diff = getDiff();
                } while (j < max_iterations);
                pagerankTime.stop();

                resultTransfer.start();
                pageranks->copyDevToHost();
                resultTransfer.stop();
            }

            if (outputFile) {
                std::ofstream output(outputFile);
                for (size_t i = 0; i < pageranks->size; i++) {
                    output << i << "\t" << (*pageranks)[i] << endl;
                }
            }
        }
};

template<size_t warp, size_t chunk>
struct pushwarp {
    template<typename... Args>
    static auto work(size_t dim, std::pair<size_t,size_t> nodeSizes, Args... args)
    {
        return makeBind(vertexPushWarp<warp,chunk>, dim, {nodeSizes.first}, {nodeSizes.second}, static_cast<size_t>((nodeSizes.first / warp)) * sizeof(push_warp_mem_t<chunk>), args...);
    }
};

template<size_t warp, size_t chunk>
struct pullwarp {
    template<typename... Args>
    static auto work(size_t dim, std::pair<size_t,size_t> nodeSizes, Args... args)
    {
        return makeBind(vertexPullWarp<warp,chunk>, dim, {nodeSizes.first}, {nodeSizes.second}, (nodeSizes.first / warp) * sizeof(pull_warp_mem_t<chunk>), args...);
    }
};

template<size_t warp, size_t chunk>
struct pullwarpnodiv {
    template<typename... Args>
    static auto work(size_t dim, std::pair<size_t,size_t> nodeSizes, Args... args)
    {
        return makeBind(vertexPullWarpNoDiv<warp,chunk>, dim, {nodeSizes.first}, {nodeSizes.second}, (nodeSizes.first / warp) * sizeof(pull_warp_mem_t<chunk>), args...);
    }
};

void pagerank
    ( CUDA& backend
    , TimerRegister& timers
    , size_t count
    , const std::string filename
    , const char *outputFile
    , int algorithm
    , size_t warp_size
    , size_t chunk_size
    )
{
    Graph<unsigned, unsigned> graph_file(filename);
    auto nodeSizes = backend.computeDivision(graph_file.vertex_count);
    auto edgeSizes = backend.computeDivision(graph_file.edge_count);

    PageRankImpl<CUDA> pr(backend, timers, count, outputFile, graph_file.vertex_count);
    auto consolidate = makeBind(consolidateRank, 1, {nodeSizes.first}, {nodeSizes.second}, 0);

    switch (algorithm) {
        case 0:
            {
                auto graph = loadEdgeListCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(updateRankEdgeList, 1,
                                       {edgeSizes.first}, {edgeSizes.second},
                                       0, std::get<0>(graph),
                                       std::get<1>(graph),
                                       std::get<2>(graph),
                                       static_cast<size_t>(graph_file.edge_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 1:
            {
                auto graph = loadStructEdgeListCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(updateRankStructEdgeList, 1,
                                       {edgeSizes.first}, {edgeSizes.second},
                                       0, std::get<0>(graph),
                                       std::get<1>(graph),
                                       static_cast<size_t>(graph_file.edge_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 2:
            {
                auto graph = loadCSR(backend, graph_file);
                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(vertexPush, 1, {nodeSizes.first},
                                       {nodeSizes.second}, 0,
                                       std::get<0>(graph),
                                       std::get<1>(graph),
                                       static_cast<size_t>(graph_file.vertex_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 3:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(vertexPull, 1, {nodeSizes.first},
                                       {nodeSizes.second}, 0,
                                       std::get<0>(graph),
                                       std::get<1>(graph),
                                       std::get<2>(graph),
                                       static_cast<size_t>(graph_file.vertex_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 4:
            {
                auto graph = loadCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                };

                auto kernel = warp_dispatch<pushwarp>::work(warp_size, chunk_size, 1_sz, nodeSizes, std::get<0>(graph), std::get<1>(graph), static_cast<size_t>(graph_file.vertex_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 5:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = warp_dispatch<pullwarp>::work(warp_size, chunk_size, 1_sz, nodeSizes, std::get<0>(graph), std::get<1>(graph), std::get<2>(graph), static_cast<size_t>(graph_file.vertex_count));

                pr.runKernel(kernel, consolidate, transfer);
            }
            break;
        case 6:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(vertexPullNoDiv, 1, {nodeSizes.first},
                                       {nodeSizes.second}, 0,
                                       std::get<0>(graph),
                                       std::get<1>(graph),
                                       std::get<2>(graph),
                                       static_cast<size_t>(graph_file.vertex_count));

                auto consolidatePull = makeBind(consolidateRankPull, 1,
                                                {nodeSizes.first},
                                                {nodeSizes.second}, 0,
                                                std::get<2>(graph)
                                                );

                pr.runKernel(kernel, consolidatePull, transfer);
            }
            break;
        case 7:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = warp_dispatch<pullwarpnodiv>::work(warp_size, chunk_size, 1_sz, nodeSizes, std::get<0>(graph), std::get<1>(graph), std::get<2>(graph), static_cast<size_t>(graph_file.vertex_count));

                auto consolidatePull = makeBind(consolidateRankPull, 1,
                                                {nodeSizes.first},
                                                {nodeSizes.second}, 0,
                                                std::get<2>(graph)
                                                );

                pr.runKernel(kernel, consolidatePull, transfer);
            }
            break;
    }
}
