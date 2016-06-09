#include <fstream>

#include "../Interface.hpp"
#include "../WarpDispatch.hpp"

#include "bfs.hpp"
#include "bfs.h"

template<typename Platform>
struct BFSImpl {
    private:
        Backend<Platform>& backend;
        TimerRegister &timers;
        size_t run_count;
        const char *outputFile;
        size_t vertex_count;

    public:
        BFSImpl
        ( Backend<Platform> &b
        , TimerRegister& ts
        , size_t count
        , const char *output
        , size_t vertices
        ) : backend(b), timers(ts), run_count(count), outputFile(output)
          , vertex_count(vertices)
        {}

        template<typename K, typename... Graph>
        void runKernel(Bind<K, Graph...> kernel, std::function<void()> transfer)
        {
            Timer graphTransfer(timers, "graphTransfer", run_count);
            Timer initResults(timers, "initResults", run_count);
            Timer bfsTime(timers, "bfsTime", run_count);
            Timer resultTransfer(timers, "resultTransfer", run_count);

            auto results = backend.template alloc<int>(vertex_count);

            graphTransfer.start();
            transfer();
            graphTransfer.stop();

            auto nodeSizes = backend.computeDivision(vertex_count);

            for (size_t i = 0; i < run_count; i++) {
                initResults.start();
                backend.setWorkSizes(1, {nodeSizes.first}, {nodeSizes.second});
                backend.runKernel(setArray, results, results->size, std::numeric_limits<int>::max());

                backend.setWorkSizes(1, {1}, {1});
                backend.runKernel(set_root, results, 0);
                initResults.stop();

                bfsTime.start();

                bool val;
                int curr = 0;
                do {
                    resetFinished();
                    kernel.call(backend, results, curr++);
                    val = getFinished();
                } while (!val);
                bfsTime.stop();

                resultTransfer.start();
                results->copyDevToHost();
                resultTransfer.stop();
            }

            if (outputFile) {
                std::ofstream output(outputFile);
                for (size_t i = 0; i < results->size; i++) {
                    output << i << "\t" << (*results)[i] << endl;
                }
            }
        }
};

template<size_t warp, size_t chunk>
struct pushwarp {
    template<typename... Args>
    static auto work(size_t dim, std::pair<size_t,size_t> nodeSizes, Args... args)
    {
        return makeBind(vertexPushWarpBfs<warp,chunk>, dim, {nodeSizes.first}, {nodeSizes.second}, static_cast<size_t>((nodeSizes.first / warp)) * sizeof(push_warp_mem_t<chunk>), args...);
    }
};

template<size_t warp, size_t chunk>
struct pullwarp {
    template<typename... Args>
    static auto work(size_t dim, std::pair<size_t,size_t> nodeSizes, Args... args)
    {
        return makeBind(vertexPullWarpBfs<warp,chunk>, dim, {nodeSizes.first}, {nodeSizes.second}, (nodeSizes.first / warp) * sizeof(pull_warp_mem_t<chunk>), args...);
    }
};

void bfs
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

    BFSImpl<CUDA> bfs(backend, timers, count, outputFile, graph_file.vertex_count);

    switch (algorithm) {
        case 0:
            {
                auto graph = loadEdgeListCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(edgeListBfs, 1,
                                       {edgeSizes.first}, {edgeSizes.second},
                                       0, std::get<0>(graph),
                                       std::get<1>(graph),
                                       std::get<2>(graph),
                                       static_cast<size_t>(graph_file.edge_count));

                bfs.runKernel(kernel, transfer);
            }
            break;
        case 1:
            {
                auto graph = loadCSR(backend, graph_file);
                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(vertexPushBfs, 1, {nodeSizes.first},
                                       {nodeSizes.second}, 0,
                                       std::get<0>(graph),
                                       std::get<1>(graph),
                                       static_cast<size_t>(graph_file.vertex_count));

                bfs.runKernel(kernel, transfer);
            }
            break;
        case 2:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = makeBind(vertexPullBfs, 1, {nodeSizes.first},
                                       {nodeSizes.second}, 0,
                                       std::get<0>(graph),
                                       std::get<1>(graph),
                                       std::get<2>(graph),
                                       static_cast<size_t>(graph_file.vertex_count));

                bfs.runKernel(kernel, transfer);
            }
            break;
        case 3:
            {
                auto graph = loadCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                };

                auto kernel = warp_dispatch<pushwarp>::work(warp_size, chunk_size, 1_sz, nodeSizes, std::get<0>(graph), std::get<1>(graph), static_cast<size_t>(graph_file.vertex_count));

                bfs.runKernel(kernel, transfer);
            }
            break;
        case 4:
            {
                auto graph = loadReverseCSR(backend, graph_file);

                auto transfer = [=] () {
                    std::get<0>(graph)->copyHostToDev();
                    std::get<1>(graph)->copyHostToDev();
                    std::get<2>(graph)->copyHostToDev();
                };

                auto kernel = warp_dispatch<pullwarp>::work(warp_size, chunk_size, 1_sz, nodeSizes, std::get<0>(graph), std::get<1>(graph), std::get<2>(graph), static_cast<size_t>(graph_file.vertex_count));

                bfs.runKernel(kernel, transfer);
            }
            break;
    }
}
