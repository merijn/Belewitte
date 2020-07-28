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

#include "utils/Util.hpp"
#include "utils/Graph.hpp"

#define CEIL_DIV(x, y)          (((x) + ((y) - 1)) / (y))

using namespace std;

static const char *execName = "reorder-graph";

template<typename V>
struct VertexDegree
{
    V vertexId;
    uint64_t degree;

    VertexDegree(V v, uint64_t d) : vertexId(v), degree(d)
    {}
};

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static void
messupSort(std::vector<VertexDegree<uint64_t>>& vertices)
{
    ssize_t numWarps = CEIL_DIV(vertices.size(), 32);
    auto start = vertices.begin();
    auto end = vertices.end();
    std::vector<VertexDegree<uint64_t>> tmpVector;

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

    assert(tmpVector.size() == vertices.size());
    vertices = tmpVector;
}

static void
sortGraph
(Graph<uint64_t,uint64_t>& graph, string fileName, sort_order order, bool worst)
{
    vector<VertexDegree<uint64_t>> newOrder;
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

        newOrder.emplace_back(v, degree);
    }

    using Degree = VertexDegree<uint64_t>;

    auto cmp = [](const Degree &a, const Degree &b) {
        if (a.degree > b.degree) return true;
        if (a.degree == b.degree && a.vertexId < b.vertexId) return true;
        return false;
    };

    stable_sort(newOrder.begin(), newOrder.end(), cmp);
    if (worst) messupSort(newOrder);

    uint64_t updatedCount = 0;
    vector<uint64_t> revLookup(newOrder.size());

    for (uint64_t i = 0; i < newOrder.size(); i++) {
        revLookup.at(newOrder.at(i).vertexId) = i;
        updatedCount++;
    }

    assert(updatedCount == newOrder.size());
    newOrder.clear();

    vector<Edge<uint64_t>> edges, rev_edges;
    edges.reserve(graph.edge_count);

    for (auto &&edge : graph.edges) {
        edges.emplace_back(revLookup.at(edge.in), revLookup.at(edge.out));
    }

    if (!graph.undirected) {
        rev_edges.reserve(graph.edge_count);

        for (auto &&edge : graph.rev_edges) {
            rev_edges.emplace_back(revLookup.at(edge.in), revLookup.at(edge.out));
        }
    }

    Graph<uint64_t,uint64_t>::output(fileName, edges, rev_edges);
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
        string newName { name.substr(0, name.find_last_of(".")) };
        sortGraph(graph, newName + ".in.graph", in_degree, false);
        sortGraph(graph, newName + ".out.graph", out_degree, false);
        sortGraph(graph, newName + ".abs.graph", abs_degree, false);
        sortGraph(graph, newName + ".in-worst.graph", in_degree, true);
        sortGraph(graph, newName + ".out-worst.graph", out_degree, true);
        sortGraph(graph, newName + ".abs-worst.graph", abs_degree, true);
    }

    return 0;
}
