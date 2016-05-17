#include <fcntl.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/stat.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include "Util.hpp"
#include "Graph.hpp"

#define CEIL_DIV(x, y)          (((x) + ((y) - 1)) / (y))

using namespace std;

static const char *execName = "reorder-graph";

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static void
messupSort(std::vector<Edge<uint64_t>>& vertices)
{
    ssize_t numWarps = CEIL_DIV(vertices.size(), 32);
    auto start = vertices.begin();
    auto end = vertices.end();
    std::vector<Edge<uint64_t>> tmpVector;

    advance(start, numWarps);
    advance(end, -numWarps);

    tmpVector.reserve(vertices.size());
    for (int64_t i = 0; i < numWarps; i++) {
        tmpVector.push_back(vertices[static_cast<size_t>(i)]);
        tmpVector.push_back(end[i]);
        for (int64_t j = 0; j < 30 && (30*i) + j < distance(start, end); j++) {
            tmpVector.push_back(start[(30*i) + j]);
        }
    }

    vertices = tmpVector;
}

static void
sortGraph(Graph<uint64_t,uint64_t>& graph, string fileName, sort_order order, bool worst)
{
    Graph<uint64_t,uint64_t> newGraph(fileName, graph.undirected, graph.vertex_count, graph.edge_count);

    vector<Edge<uint64_t>> newOrder;
    newOrder.reserve(graph.vertex_count);

    for (uint64_t v = 0; v < graph.vertex_count; v++) {
        uint64_t degree;

        switch (order) {
            case in_degree:
                degree = graph.raw_rev_vertices[v + 1] - graph.raw_rev_vertices[v];
                break;
            case out_degree:
                degree = graph.raw_vertices[v + 1] - graph.raw_vertices[v];
                break;
            case abs_degree:
                degree = graph.raw_vertices[v + 1] - graph.raw_vertices[v];
                degree += graph.raw_rev_vertices[v + 1] - graph.raw_rev_vertices[v];
                break;
        }

        newOrder.emplace_back(degree, v);
    }

    stable_sort(newOrder.begin(), newOrder.end(), greater<Edge<uint64_t>>());
    if (worst) messupSort(newOrder);

    uint64_t edgeOffset = 0, revEdgeOffset = 0;

    for (uint64_t n = 0; n < graph.vertex_count; n++) {
        newGraph.raw_vertices[n] = edgeOffset;

        uint64_t v = newOrder[n].out;
        uint64_t degree = graph.raw_vertices[v+1] - graph.raw_vertices[v];
        for (uint64_t i = 0; i < degree; i++) {
            newGraph.raw_edges[edgeOffset++] = graph.raw_edges[graph.raw_vertices[v] + i];
        }

        if (!graph.undirected) {
            degree = graph.raw_rev_vertices[v+1] - graph.raw_rev_vertices[v];
            newGraph.raw_rev_vertices[n] = revEdgeOffset;
            for (uint64_t i = 0; i < degree; i++) {
                newGraph.raw_rev_edges[revEdgeOffset++] = graph.raw_rev_edges[graph.raw_rev_vertices[v] + i];
            }
        }
    }

    newGraph.raw_vertices[graph.vertex_count] = edgeOffset;
    if (!graph.undirected) {
        newGraph.raw_rev_vertices[graph.vertex_count] = revEdgeOffset;
    }
}

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " <graph 1> [<graph 2>...]" << endl;
    exit(exitCode);
}

int main(int argc, char **argv)
{
    string name;

    if (argc < 2) usage();

    for (int i = 1; i < argc; i++) {
        name = string(argv[i]);
        Graph<uint64_t, uint64_t> graph(name);
        sortGraph(graph, name + ".in", in_degree, false);
        sortGraph(graph, name + ".out", out_degree, false);
        sortGraph(graph, name + ".abs", abs_degree, false);
        sortGraph(graph, name + ".in-worst", in_degree, true);
        sortGraph(graph, name + ".out-worst", out_degree, true);
        sortGraph(graph, name + ".abs-worst", abs_degree, true);
    }

    return 0;
}
