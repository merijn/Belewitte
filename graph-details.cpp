#include <cstring>
#include <iostream>
#include <iterator>
#include <set>

#include <boost/filesystem.hpp>

#include "utils/Graph.hpp"
#include "options/Options.hpp"

using namespace std;
using namespace boost::filesystem;

static const char *exeName = "graph-details";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << "    " << exeName << " graph [graphs...]" << endl << endl;
    out << "Options:" << endl;
});

int main(int argc, char * const *argv)
{
    map<string, Degrees> orderings;
    bool verbose = false;
    vector<string> graphs;

    options.add('v', "verbose", verbose, true, "Verbose output.");

    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    graphs = options.parseArgs(argc, argv);

    orderings = {
        {"abs", Degrees::abs},
        {"in", Degrees::in},
        {"out", Degrees::out}
    };

    for (string filename : graphs) {
        Graph<uint64_t, uint64_t> graph(filename);
        string name = path(filename).stem().string();

        cout << name << ":vertex-count:" << graph.vertex_count << endl;
        cout << name << ":edge-count:" << graph.edge_count << endl;

        for (auto p : orderings) {
            string prefix = name + ":" + p.first;
            auto summary = graph.degreeStatistics(p.second);

            cout << prefix << ":min:" << summary.min << endl;
            cout << prefix << ":lower:" << summary.lowerQuantile << endl;
            cout << prefix << ":median:" << summary.median << endl;
            cout << prefix << ":mean:" << summary.mean << endl;
            cout << prefix << ":upper:" << summary.upperQuantile << endl;
            cout << prefix << ":max:" << summary.max << endl;
            cout << prefix << ":stddev:" << summary.stdDev << endl;
        }
    }

    return 0;
}
