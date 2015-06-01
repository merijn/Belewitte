#include <fcntl.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/stat.h>

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include "GraphFile.hpp"

#define CEIL_DIV(x, y)          (((x) + ((y) - 1)) / (y))

using namespace std;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static void
messupSort(std::vector<pair<int,int>>& vertices)
{
    size_t numWarps = CEIL_DIV(vertices.size(), 32);
    auto start = vertices.begin() + numWarps;
    auto end = vertices.end() - numWarps;
    std::vector<pair<int, int>> tmpVector;

    tmpVector.reserve(vertices.size());
    for (int i = 0; i < numWarps; i++) {
        tmpVector.push_back(vertices[i]);
        tmpVector.push_back(end[i]);
        for (int j = 0; j < 30 && (30*i) + j < distance(start, end); j++) {
            tmpVector.push_back(start[(30*i) + j]);
        }
    }

    vertices = tmpVector;
}

static void
sortGraph(GraphFile& graph, string fileName, sort_order order, bool worst)
{
    GraphFile newGraph(fileName, graph.undirected, graph.vertex_count, graph.edge_count);

    vector<pair<int,int>> newOrder;
    newOrder.reserve(static_cast<size_t>(graph.vertex_count));

    for (int v = 0; v < graph.vertex_count; v++) {
        int degree;

        switch (order) {
            case in_degree:
                degree = graph.rev_vertices[v + 1] - graph.rev_vertices[v];
                break;
            case out_degree:
                degree = graph.vertices[v + 1] - graph.vertices[v];
                break;
            case abs_degree:
                degree = graph.vertices[v + 1] - graph.vertices[v];
                degree += graph.rev_vertices[v + 1] - graph.rev_vertices[v];
                break;
        }

        newOrder.emplace_back(degree, v);
    }

    stable_sort(newOrder.begin(), newOrder.end(), greater<pair<int,int>>());
    if (worst) messupSort(newOrder);

    int edgeOffset = 0, revEdgeOffset = 0;

    for (int n = 0; n < graph.vertex_count; n++) {
        newGraph.vertices[n] = edgeOffset;

        int v = newOrder[static_cast<size_t>(n)].second;
        int degree = graph.vertices[v+1] - graph.vertices[v];
        for (int i = 0; i < degree; i++) {
            newGraph.edges[edgeOffset++] = graph.edges[graph.vertices[v] + i];
        }

        if (!graph.undirected) {
            degree = graph.rev_vertices[v+1] - graph.rev_vertices[v];
            newGraph.rev_vertices[n] = revEdgeOffset;
            for (int i = 0; i < degree; i++) {
                newGraph.rev_edges[revEdgeOffset++] = graph.rev_edges[graph.rev_vertices[v] + i];
            }
        }
    }

    newGraph.vertices[graph.vertex_count] = edgeOffset;
    if (!graph.undirected) {
        newGraph.rev_vertices[graph.vertex_count] = revEdgeOffset;
    }
}

int main(int argc, char **argv)
{
    string name;

    for (int i = 1; i < argc; i++) {
        name = string(argv[i]);
        GraphFile graph(name);
        sortGraph(graph, name + ".in", in_degree, false);
        sortGraph(graph, name + ".out", out_degree, false);
        sortGraph(graph, name + ".abs", abs_degree, false);
        sortGraph(graph, name + ".in-worst", in_degree, true);
        sortGraph(graph, name + ".out-worst", out_degree, true);
        sortGraph(graph, name + ".abs-worst", abs_degree, true);
    }

    return 0;
}
