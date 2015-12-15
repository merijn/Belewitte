#include <atomic>
#include <memory>

#include "Connectivity.hpp"
#include "Graph.hpp"

typedef Graph<uint64_t,uint64_t> Graph_t;
typedef Graph_t::Vertices Vertices;
typedef Accessor<uint64_t> Edges;
typedef std::unique_ptr<std::atomic<uint64_t>[]> DepthArray;

#ifdef __INTEL_COMPILER
#include "tbb/parallel_reduce.h"
#include "tbb/blocked_range.h"

using namespace tbb;

template<typename C>
using Range = blocked_range<typename C::const_iterator>;

template<typename C>
static bool
reduce(const C& c, std::function<bool(const Range<C>&, bool)> fun)
{
    bool init = false;
    return parallel_reduce(Range<C>(c.begin(), c.end()), init, fun,
            std::logical_or<bool>());
}

static bool
bfs_step(Graph_t& graph, DepthArray& depths, uint64_t current_level)
{
    bool directed = !graph.undirected;
    uint64_t new_level = current_level + 1;

    auto setEdges = [=,&depths](const Range<Edges>& edges, bool init)->bool
    {
        for (auto&& edge : edges) {
            init |= atomic_min(depths[edge], new_level);
        }
        return init;
    };

    auto sum = [=,&depths](const Range<Vertices>& vertices, bool init)->bool
    {
        for (auto&& vertex : vertices) {
            if (depths[vertex.id] == current_level) {
                init |= reduce(vertex.edges, setEdges);
                if (directed) {
                    init |= reduce(vertex.rev_edges, setEdges);
                }
            }
        }
        return init;
    };

    return reduce(graph.vertices, sum);
}
#endif

double connectivity(Graph_t& graph)
{
#ifdef __INTEL_COMPILER
    uint64_t count = 0;
    uint64_t level = 0;
    auto depths = std::make_unique<std::atomic<uint64_t>[]>(graph.vertex_count);

    for (uint64_t i = 0; i < graph.vertex_count; i++) {
        depths[i] = std::numeric_limits<uint64_t>::max();
    }

    depths[0] = 0;
    while (bfs_step(graph, depths, level++));

    for (uint64_t i = 0; i < graph.vertex_count; i++) {
        if (depths[i] != std::numeric_limits<uint64_t>::max()) count++;
    }

    return static_cast<double>(count) / graph.vertex_count;
#else
    (void) graph;
    checkError(true, "Connectivity not implemented!");
#endif
}
