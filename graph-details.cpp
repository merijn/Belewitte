#include <cstring>
#include <iostream>
#include <iterator>
#include <set>

#include <boost/filesystem.hpp>

#include "Graph.hpp"
#include "Options.hpp"

using namespace std;
using namespace boost::filesystem;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static const char *exeName = "graph-details";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << "    " << exeName << " graph [graphs...]" << endl << endl;
    out << "Options:" << endl;
});

static pair<size_t,size_t>
medianIndices(size_t count)
{
    if (count % 2 == 0) return { (count/2) - 1, count/2 };
    return { count/2, count/2 };
}

static void
fiveDigitSummary(ostream& out, string prefix, vector<size_t> &degrees)
{
    size_t half = degrees.size()/2;
    pair<size_t,size_t> median = medianIndices(degrees.size());
    pair<size_t,size_t> lower = medianIndices(half);
    pair<size_t,size_t> upper = lower;

    upper.first += half + (degrees.size() % 2);
    upper.second += half + (degrees.size() % 2);

    sort(degrees.begin(), degrees.end());

    double total = 0;
    double M = 0.0;
    double S = 0.0;
    int k = 1;
    for (auto d : degrees) {
        total += d;
        double tmpM = M;
        M += (d - tmpM) / k;
        S += (d - tmpM) * (d - M);
        k++;
    }

    out << prefix << ":min:" << degrees.front() << endl;
    out << prefix << ":lower:" << (degrees[lower.first] + degrees[lower.second])/2.0 << endl;
    out << prefix << ":median:" << (degrees[median.first] + degrees[median.second])/2.0 << endl;
    out << prefix << ":mean:" << (total / degrees.size()) << endl;
    out << prefix << ":upper:" << (degrees[upper.first] + degrees[upper.second])/2.0 << endl;
    out << prefix << ":max:" << degrees.back() << endl;
    out << prefix << ":stddev:" << sqrt(S / (k-1)) << endl;
}

int main(int argc, char **argv)
{
    map<string, sort_order> orderings;
    bool verbose = false;
    vector<char*> graphs;

    options.add('v', "verbose", verbose, true, "Verbose output.");

    std::set_new_handler(out_of_memory);
    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    graphs = options.parseArgs(argc, argv);

    orderings = {{"abs", abs_degree}, {"in", in_degree}, {"out", out_degree}};

    for (string filename : graphs) {
        Graph<uint64_t, uint64_t> graph(filename);
        string name = path(filename).stem().string();
        for (auto p : orderings) {
            vector<size_t> degrees;
            degrees.reserve(graph.vertex_count);

            for (auto v : graph.vertices) {
                size_t degree = 0;

                if (p.second == out_degree || p.second == abs_degree) {
                    degree += v.edges.size;
                }

                if (p.second == in_degree || (p.second == abs_degree && !graph.undirected)) {
                    degree += v.rev_edges.size;
                }

                degrees.emplace_back(degree);
            }

            cout << name << ":" << p.first << ":vertex-count:"
                 << graph.vertex_count << endl;
            cout << name << ":" << p.first << ":edge-count:"
                 << graph.edge_count << endl;
            fiveDigitSummary(cout, name + ":" + p.first, degrees);
        }
    }

    return 0;
}
