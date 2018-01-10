#include <cstring>
#include <iostream>
#include <iterator>
#include <set>

#include "utils/Graph.hpp"
#include "options/Options.hpp"

using namespace std;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static const char *exeName = "graph-edges";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << "    " << exeName << " graph" << endl << endl;
    out << "Options:" << endl;
});

int main(int argc, char **argv)
{
    bool readable = false;
    vector<char*> graphs;

    options.add('h', "human", readable, true, "Human readable output.");

    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));

    graphs = options.parseArgs(argc, argv);
    if (graphs.size() != 1) {
        cerr << "Wrong number of arguments!" << endl;
        options.usage(cerr);
    }

    if (readable) cout.imbue(std::locale());

    Graph<uint64_t, uint64_t> graph(graphs[0]);

    for (auto v : graph.vertices) {
        for (auto &e : v.edges) {
            cout << v.id << " " << e << endl;
        }
    }

    return 0;
}
