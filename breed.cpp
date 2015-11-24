#include <functional>
#include <random>
#include <set>

#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

typedef Edge<uint64_t> edge_t;
typedef set<edge_t> edge_set;

void
graph_iteration
( const Graph<uint64_t,uint64_t>& graph1
, const Graph<uint64_t,uint64_t>& graph2
, mt19937_64& generator
, edge_set& mutations
, std::function<void(const edge_t)> callback
);

void
graph_iteration
( const Graph<uint64_t,uint64_t>& graph1
, const Graph<uint64_t,uint64_t>& graph2
, mt19937_64& generator
, edge_set& mutations
, std::function<void(const edge_t)> callback
)
{
    uniform_int_distribution<int> crossover(0, 1);
    auto mut = simple_iterator<edge_set>(mutations);
    auto edges1 = simple_iterator<Graph<uint64_t,uint64_t>::Edges>(graph1.edges);
    auto edges2 = simple_iterator<Graph<uint64_t,uint64_t>::Edges>(graph2.edges);

    while (edges1 || edges2 || mut) {
        if (edges1 && edges2 && *edges1 == *edges2) {
            if (mut && *mut == *edges1) mut++;
            else callback(*edges1);

            edges1++;
            edges2++;
        } else if (edges1 && (!edges2 || *edges1 < *edges2) && (!mut || *edges1 <= *mut)) {
            if (crossover(generator)) {
                if (mut && *mut == *edges1) mut++;
                else callback(*edges1);
            } else if (mut && *mut == *edges1) {
                mut++;
                callback(*edges1);
            }
            edges1++;
        } else if (edges2 && (!edges1 || *edges2 < *edges1) && (!mut || *edges2 <= *mut)) {
            if (crossover(generator)) {
                if (mut && *mut == *edges2) mut++;
                else callback(*edges2);
            } else if (mut && *mut == *edges2) {
                mut++;
                callback(*edges2);
            }
            edges2++;
        } else if (mut && (!edges1 || *mut < *edges1) && (!edges2 || *mut < *edges2)) {
            callback(*mut);
            mut++;
        }
    }
}

int main(int argc, char **argv)
{
    checkError(argc == 4, "Wrong number of arguments.");
    const Graph<uint64_t,uint64_t> graph1(argv[1]);
    const Graph<uint64_t,uint64_t> graph2(argv[2]);
    double mutation_rate = stod(argv[3]);
    checkError(graph1.vertex_count == graph2.vertex_count,
            "Attempting crossover of graphs with incompatible vertex counts.");

    uint64_t vertex_count = graph1.vertex_count;
    uint64_t edge_count = vertex_count * vertex_count;

    mt19937_64 generator;
    binomial_distribution<uint64_t> num_mutations(edge_count, mutation_rate);
    uniform_int_distribution<uint64_t> mutated_edge(0, edge_count);
    uint64_t mutation_count = num_mutations(generator);

    edge_set mutations;

    while (mutations.size() < mutation_count) {
        uint64_t edgeIdx = mutated_edge(generator);
        mutations.insert({edgeIdx / vertex_count, edgeIdx % vertex_count});
    }

    uint64_t seed = uniform_int_distribution<uint64_t>()(generator);

    uint64_t edges = 0;
    auto edge_increment = [&](const edge_t) { edges++; };

    generator.seed(seed);
    graph_iteration(graph1, graph2, generator, mutations, edge_increment);

    Graph<uint64_t,uint64_t> newGraph("foo.graph", true, graph1.vertex_count, edges);

    uint64_t vertexId = 0, edgeId = 0;
    auto build_graph = [&](const edge_t newEdge) {
        while (vertexId < newEdge.in) {
            newGraph.raw_vertices[++vertexId] = edgeId;
        }

        newGraph.raw_edges[edgeId++] = newEdge.out;
    };

    generator.seed(seed);
    graph_iteration(graph1, graph2, generator, mutations, build_graph);

    while (vertexId < newGraph.vertex_count) {
        newGraph.raw_vertices[++vertexId] = edgeId;
    }

    return 0;
}

