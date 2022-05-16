#include <getopt.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include "utils/Graph.hpp"
#include "options/Options.hpp"

using namespace std;

typedef Graph<uint64_t,uint64_t> Graph_t;

string gen_chain(vector<Edge<uint64_t>>&, bool undirected, size_t);
string gen_star(vector<Edge<uint64_t>>&, bool undirected, size_t);
string gen_mesh(vector<Edge<uint64_t>>&, bool undirected, size_t, size_t);
string gen_degree4(vector<Edge<uint64_t>>&, bool undirected, size_t);
string gen_degree6(vector<Edge<uint64_t>>&, bool undirected, size_t);
string gen_anydegree(vector<Edge<uint64_t>>&, bool undirected, size_t, size_t);

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

static const char *exeName = "gen-graph";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << exeName << " -S" << endl;
    out << exeName << " chain <vertex count>" << endl;
    out << exeName << " star <vertex count>" << endl;
    out << exeName << " mesh <mesh width> <mesh height>" << endl;
    out << exeName << " degree4 <vertex count>" << endl;
    out << exeName << " degree6 <vertex count>" << endl;
    out << exeName << " any <?> <?>" << endl;
    out << exeName << " random <graph> <vertex count> <edge count>" << endl;
    out << exeName << " random-mutations <graph> <vertex count> <mutation rate>"
        << endl << endl;
    out << endl << "Options:" << endl;
});

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    options.usage(cout, "    ");
    exit(exitCode);
}

string gen_chain(vector<Edge<uint64_t>>& edges, bool undirected, size_t N)
{
    edges.reserve(undirected ? 2*N : N);

    for (uint64_t i = 0; i < N; i++) {
        edges.emplace_back(i, i+1);
        if (undirected) edges.emplace_back(i+1, i);
    }

    return file_name("chain", N);
}

string gen_star(vector<Edge<uint64_t>>& edges, bool undirected, size_t N)
{
    edges.reserve(undirected ? 2*N : N);

    for (size_t i = 0; i < N; i++) {
        edges.emplace_back(0, i+1);
        if (undirected) edges.emplace_back(i+1, 0);
    }

    return file_name("star", N);
}

string gen_mesh(vector<Edge<uint64_t>>& edges, bool undirected, size_t H, size_t W)
{
    size_t increment = undirected ? 2 : 1;
    size_t edge_count = 0;

    for (size_t n = 0; n < 2; n++) {
        edges.reserve(edge_count);
        for (size_t j = 0; j < H; j++) {
            for (size_t i = 0; i < W; i++) {
                if (i < W-1) {
                    if (n) {
                        edges.emplace_back(j*W+i, j*W+i+1);
                        if (undirected) edges.emplace_back(j*W+i+1, j*W+i);
                    } else edge_count += increment;
                }

                if (j < H-1) {
                    if (n) {
                        edges.emplace_back(j*W+i, (j+1)*W+i);
                        if (undirected) edges.emplace_back((j+1)*W+i, j*W+i);
                    } else edge_count += increment;
                }
            }
        }
    }

    return file_name("mesh", H, W);
}

string gen_degree4(vector<Edge<uint64_t>>& edges, bool undirected, size_t N)
{
    edges.reserve((undirected ? 4 : 2) * N * N);

    for (size_t j = 0; j < N; j++) {
        for (size_t i = 0; i < N; i++) {
            if (i < N-1) {
                edges.emplace_back(j*N+i, j*N+i+1);
                if (undirected) edges.emplace_back(j*N+i+1, j*N+i);
            } else {
                edges.emplace_back(j*N, j*N+i);
                if (undirected) edges.emplace_back(j*N+i, j*N);
            }

            if (j < N-1) {
                edges.emplace_back(j*N+i, (j+1)*N+i);
                if (undirected) edges.emplace_back((j+1)*N+i, j*N+i);
            } else {
                edges.emplace_back(i, j*N+i);
                if (undirected) edges.emplace_back(j*N+i, i);
            }
        }
    }

    string tmp = file_name("degree4", N);
    return tmp;
}

string gen_degree6(vector<Edge<uint64_t>>& edges, bool undirected, size_t N)
{
    edges.reserve((undirected ? 6 : 3) * N * N * N);

    for (size_t k = 0; k < N; k++) {
        for (size_t j = 0; j < N; j++) {
            for (size_t i = 0; i < N; i++) {
                if (i < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, k*N*N+j*N+i+1);
                    if (undirected) edges.emplace_back(k*N*N+j*N+i+1, k*N*N+j*N+i);
                } else {
                    edges.emplace_back(k*N*N+j*N, k*N*N+j*N+i);
                    if (undirected) edges.emplace_back(k*N*N+j*N+i, k*N*N+j*N);
                }

                if (j < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, k*N*N+(j+1)*N+i);
                    if (undirected) edges.emplace_back(k*N*N+(j+1)*N+i, k*N*N+j*N+i);
                } else {
                    edges.emplace_back(k*N*N+i, k*N*N+j*N+i);
                    if (undirected) edges.emplace_back(k*N*N+j*N+i, k*N*N+i);
                }

                if (k < N-1) {
                    edges.emplace_back(k*N*N+j*N+i, (k+1)*N*N+j*N+i);
                    if (undirected) edges.emplace_back((k+1)*N*N+j*N+i, k*N*N+j*N+i);
                } else {
                    edges.emplace_back(j*N+i, k*N*N+j*N+i);
                    if (undirected) edges.emplace_back(k*N*N+j*N+i, j*N+i);
                }
            }
        }
    }

    return file_name("degree6", N);
}

string gen_anydegree(vector<Edge<uint64_t>>& edges, bool undirected, size_t N, size_t D)
{
    std::vector<uint64_t> powA(D + 1);
    std::vector<uint64_t> coef(D);

    for (size_t p = 0; p <= D; p++) {
        powA[p] = static_cast<uint64_t>(pow(N,p));
    }

    edges.reserve((undirected ? 2 : 1) *D * static_cast<uint64_t>(pow(N, D)));

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

int main(int argc, char **argv)
{
    string name;
    int undirected = false;
    bool sorted = false;
    uint64_t vertex_count = 0;
    vector<Edge<uint64_t>> edges;

    const char *optString = ":duh?";
    static const struct option longopts[] = {
        { "directed", no_argument, &undirected, false},
        { "undirected", no_argument, &undirected, true},
        { "help", no_argument, nullptr, 'h' },
        { nullptr, 0, nullptr, 0 },
    };

    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    for (;;) {
        int longIndex;
        int opt = getopt_long(argc, argv, optString, longopts, &longIndex);
        if (opt == -1) break;

        switch (opt) {
            case 'd':
                undirected = false;
                break;

            case 'u':
                undirected = true;
                break;

            case 'h':
            case '?':
                usage(EXIT_SUCCESS);

            case 0: break;

            case ':':
                cerr << "Missing option for flag '" << optopt << "'." << endl;
                FALLTHROUGH;
            default:
                usage();
        }
    }

    argc -= optind;
    argv = &argv[optind];

    if (argc == 2 && !strcmp(argv[0], "chain")) {
        vertex_count = stoull(argv[1]);
        name = gen_chain(edges, undirected, vertex_count);
    } else if (argc == 2 && !strcmp(argv[0], "star")) {
        vertex_count = stoull(argv[1]);
        name = gen_star(edges, undirected, vertex_count);
    } else if (argc == 3 && !strcmp(argv[0], "mesh")) {
        name = gen_mesh(edges, undirected, stoull(argv[1]), stoull(argv[2]));
    } else if (argc == 2 && !strcmp(argv[0], "degree4")) {
        vertex_count = stoull(argv[1]);
        name = gen_degree4(edges, undirected, vertex_count);
    } else if (argc == 2 && !strcmp(argv[0], "degree6")) {
        vertex_count = stoull(argv[1]);
        name = gen_degree6(edges, undirected, vertex_count);
    } else if (argc == 3 && !strcmp(argv[0], "any")) {
        name = gen_anydegree(edges, undirected, stoull(argv[1]), stoull(argv[2]));
    } else if (argc == 4 && !strcmp(argv[0], "random")) {
        sorted = true;
        name = argv[1];
        vertex_count = stoul(argv[2]);
        uint64_t edge_count = stoul(argv[3]);
        edges = random_edges<uint64_t>(undirected, vertex_count, edge_count);
    } else if (argc == 4 && !strcmp(argv[0], "random-mutations")) {
        sorted = true;
        name = argv[1];
        vertex_count = stoul(argv[2]);
        double mutation_rate = stod(argv[3]);
        edges = random_edges<uint64_t>(undirected, vertex_count, mutation_rate);
    } else {
        usage();
    }

    if (undirected) {
        if (sorted) Graph_t::outputSortedUniq(name, vertex_count, edges);
        else Graph_t::outputUniq(name, vertex_count, edges);
    } else {
        auto rev_edges = reverse_and_sort(edges);
        if (sorted) {
            Graph_t::outputSortedUniq(name, vertex_count, edges, rev_edges);
        } else {
            Graph_t::outputUniq(name, vertex_count, edges, rev_edges);
        }
    }

    cout << name << endl;

    return 0;
}
