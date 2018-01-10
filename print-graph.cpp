#include <getopt.h>

#include "utils/Graph.hpp"
#include "utils/Util.hpp"

using namespace std;

static const char *execName = "print-graph";

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " [--help | -h]" << endl;
    out << execName << " [-v | --verbose] <graph 1> [<graph 2>...]" << endl;
    exit(exitCode);
}

int main(int argc, char **argv)
{
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

    if (argc == 0) usage();

    for (int i = 0; i < argc; i++) {
        Graph<uint64_t,uint64_t> graph(argv[i]);
        cout << "Undirected: " << (graph.undirected ? "true" : "false") << endl;
        cout << "Vertex count: " << graph.vertex_count << endl;
        cout << "Edge count: " << graph.edge_count << endl;
        if (verbose) {
            for (auto v : graph.vertices) {
                cout << v.id << endl;
                for (auto &e : v.edges) {
                    cout << "  -> " << e << endl;
                }

                if (!graph.undirected) {
                    for (auto &e : v.rev_edges) {
                        cout << "  " << e << "  ->" << endl;
                    }
                }
            }
            cout << endl;
        }
    }
    return 0;
}
