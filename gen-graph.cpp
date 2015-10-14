#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include "Graph.hpp"
#include "GraphFile.hpp"

using namespace std;

string gen_chain(vector<edge<uint64_t>>&, size_t);
string gen_star(vector<edge<uint64_t>>&, size_t);
string gen_mesh(vector<edge<uint64_t>>&, size_t, size_t);
string gen_degree4(vector<edge<uint64_t>>&, size_t);
string gen_degree6(vector<edge<uint64_t>>&, size_t);
string gen_anydegree(vector<edge<uint64_t>>&, size_t, size_t);

string gen_name(ostringstream&);

string gen_name(ostringstream& stream)
{
    stream << ".graph";
    return stream.str();
}

template<typename T, typename... Args>
string gen_name(ostringstream& stream, T first, Args... args)
{
    stream << "_" << first;
    return gen_name(stream, args...);
}

template<typename T, typename... Args>
string file_name(T first, Args... args)
{
    ostringstream fileName;
    fileName << first;
    return gen_name(fileName, args...);
}

string gen_chain(vector<edge<uint64_t>>& edges, size_t N)
{
    edges.reserve(N);

    for (uint64_t i = 0; i < N; i++) {
        edges.emplace_back(i, i+1);
    }

    return file_name("chain", N);
}

string gen_star(vector<edge<uint64_t>>& edges, size_t N)
{
    edges.reserve(N);

    for (size_t i = 0; i < N; i++) {
        edges.emplace_back(0, i+1);
    }

    return file_name("star", N);
}

string gen_mesh(vector<edge<uint64_t>>& edges, size_t H, size_t W)
{
    size_t edge_count = 0;

    for (size_t n = 0; n < 2; n++) {
        edges.reserve(edge_count);
        for (size_t j = 0; j < H; j++) {
            for (size_t i = 0; i < W; i++) {
                if (i < W-1) {
                    if (n) edges.emplace_back(j*W+i, j*W+i+1);
                    else edge_count++;
                }

                if (j < H-1) {
                    if (n) edges.emplace_back(j*W+i, (j+1)*W+i);
                    else edge_count++;
                }
            }
        }
    }

    return file_name("mesh", H, W);
}

string gen_degree4(vector<edge<uint64_t>>& edges, size_t N)
{
    edges.reserve(2 * N * N);

    for (size_t j = 0; j < N; j++) {
        for (size_t i = 0; i < N; i++) {
            if (i < N-1) {
                edges.emplace_back(j*N+i, j*N+i+1);
            } else {
                edges.emplace_back(j*N, j*N+i);
            }

            if (j < N-1) {
                edges.emplace_back(j*N+i, (j+1)*N+i);
            } else {
                edges.emplace_back(i, j*N+i);
            }
        }
    }

    string tmp = file_name("degree4", N);
    return tmp;
}

string gen_degree6(vector<edge<uint64_t>>& edges, size_t N)
{
    edges.reserve( 3 * N * N * N);

    for (size_t k = 0; k < N; k++) {
        for (size_t j = 0; j < N; j++) {
            for (size_t i = 0; i < N; i++) {
                if (i < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, k*N*N+j*N+i+1);
                } else {
                    edges.emplace_back(k*N*N+j*N, k*N*N+j*N+i);
                }

                if (j < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, k*N*N+(j+1)*N+i);
                } else {
                    edges.emplace_back(k*N*N+i, k*N*N+j*N+i);
                }

                if (k < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, (k+1)*N*N+j*N+i);
                } else {
                    edges.emplace_back(j*N+i, k*N*N+j*N+i);
                }
            }
        }
    }

    return file_name("degree6", N);
}

string gen_anydegree(vector<edge<uint64_t>>& edges, size_t N, size_t D)
{
    std::vector<uint64_t> powA(D + 1);
    std::vector<uint64_t> coef(D);

    for (size_t p = 0; p <= D; p++) {
        powA[p] = static_cast<uint64_t>(pow(N,p));
    }

    edges.reserve(D * static_cast<uint64_t>(pow(N, D)));

    for (uint64_t i = 0; i < static_cast<uint64_t>(pow(N, D)); i++) {
        uint64_t tmp = i;

        for (uint64_t p = 0; p < D; p++) {
            coef[p] = tmp % N;
            tmp /= N;
        }

        for (uint64_t p = 0; p < D; p++) {
            if (coef[p] < N-1) {
                edges.emplace_back(i, i + powA[p]);
            } else {
                edges.emplace_back((i + powA[p]) - powA[p+1], i);
            }
        }
    }

    return file_name("degree", D, N);
}

static void
outputGraph(string fileName, vector<edge<uint64_t>>& tmp_vector)
{
    vector<edge<uint64_t>> edge_vector;
    edge_vector.reserve(2 * tmp_vector.size());

    for (auto &edge : tmp_vector) {
        edge_vector.push_back(edge);
        edge_vector.emplace_back(edge.out, edge.in);
    }

    sort(edge_vector.begin(), edge_vector.end());

    uint64_t vertex_count = 0;
    for (auto &edge : edge_vector) {
        vertex_count = max(vertex_count, max(edge.in, edge.out));
    }

    size_t edge_count = edge_vector.size();

    GraphFile<uint64_t, uint64_t> graph(fileName, true, vertex_count, edge_count);

    uint64_t vertex = 0, edgeOffset = 0;

    graph.vertices[0] = edgeOffset;
    for (auto &edge : edge_vector) {
        while (vertex < edge.in) {
           graph.vertices[vertex++] = edgeOffset;
        }
        graph.edges[edgeOffset++] = edge.out;
    }

    while (vertex < vertex_count) {
        graph.vertices[vertex++] = edgeOffset;
    }
}

int main(int argc, char **argv)
{
    string name;
    vector<edge<uint64_t>> graph;

    if (argc == 3 && !strcmp(argv[1], "chain")) {
        name = gen_chain(graph, stoull(argv[2]));
    } else if (argc == 3 && !strcmp(argv[1], "star")) {
        name = gen_star(graph, stoull(argv[2]));
    } else if (argc == 4 && !strcmp(argv[1], "mesh")) {
        name = gen_mesh(graph, stoull(argv[2]), stoull(argv[3]));
    } else if (argc == 3 && !strcmp(argv[1], "degree4")) {
        name = gen_degree4(graph, stoull(argv[2]));
    } else if (argc == 3 && !strcmp(argv[1], "degree6")) {
        name = gen_degree6(graph, stoull(argv[2]));
    } else if (argc == 4 && !strcmp(argv[1], "any")) {
        name = gen_anydegree(graph, stoull(argv[2]), stoull(argv[3]));
    }

    outputGraph(name, graph);

    return 0;
}
