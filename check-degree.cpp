#include <getopt.h>

#include <cstring>
#include <iostream>
#include <map>

#include "Graph.hpp"

using namespace std;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static const char *execName = "check-degree";

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " [--help | -h]" << endl;
    out << execName << " [-v | --verbose] abs <graph1> [<graph2>...]" << endl;
    out << execName << " [-v | --verbose] in <graph1> [<graph2>...]" << endl;
    out << execName << " [-v | --verbose] out <graph1> [<graph2>...]" << endl;
    exit(exitCode);
}

int main(int argc, char **argv)
{
    string name;
    sort_order ordering;
    int verbose = false;
    const char *optString = ":vh?";
    static const struct option longopts[] = {
        { "verbose", no_argument, &verbose, 1},
        { "help", no_argument, nullptr, 'h' },
        { nullptr, 0, nullptr, 0 },
    };

    execName = argv[0];
    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    for (;;) {
        int longIndex;
        int opt = getopt_long(argc, argv, optString, longopts, &longIndex);
        if (opt == -1) break;

        switch (opt) {
            case 'v':
                verbose = true;
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

    if (argc <= 1) usage();

    if (!strcmp(argv[0], "abs")) ordering = abs_degree;
    else if (!strcmp(argv[0], "in")) ordering = in_degree;
    else if (!strcmp(argv[0], "out")) ordering = out_degree;
    else usage();

    for (int i = 1; i < argc; i++) {
        name = string(argv[i]);

        const Graph<uint64_t, uint64_t> graph(name);
        map<size_t, size_t> degrees;

        for (auto v : graph.vertices) {
            size_t degree = 0;

            if (ordering == out_degree || ordering == abs_degree) {
                degree += v.edges.size;
            }

            if (ordering == in_degree || (ordering == abs_degree && !graph.undirected)) {
                degree += v.rev_edges.size;
            }

            degrees[degree]++;
        }

        cout << name << ": " << endl;
        cout << "Vertex count: " << graph.vertex_count << endl;
        cout << "Edge count: " << graph.edge_count << endl;
        if (verbose) {
            cout << "Degrees: " << endl;
            for (auto &pair : degrees) {
                cout << "\t" << pair.first << " : " << pair.second << endl;
            }
        }
        cout << endl;
    }

    return 0;
}
