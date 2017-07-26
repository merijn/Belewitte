#include <cstring>
#include <iostream>
#include <iterator>
#include <set>

#include "Graph.hpp"
#include "Options.hpp"

using namespace std;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

static const char *exeName = "graph-details";
static Options options('h', "help", cout, [](ostream& out)
{
    out << "Usage:" << endl;
    out << "    " << exeName << " graph" << endl << endl;
    out << "Options:" << endl;
});

static pair<size_t,size_t>
medianIndices(size_t count)
{
    if (count % 2 == 0) return { (count/2) - 1, count/2 };
    return { count/2, count/2 };
}

static void
fiveDigitSummary(ostream& out, string type, vector<size_t> &degrees)
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

    out << "min " << type << " degree:" << degrees.front() << endl;
    out << "lower quantile " << type << " degree:"
        << (degrees[lower.first] + degrees[lower.second])/2.0 << endl;
    out << "median " << type << " degree:"
        << (degrees[median.first] + degrees[median.second])/2.0 << endl;
    out << "mean " << type << " degree:"
        << (total / degrees.size()) << endl;
    out << "upper quantile " << type << " degree:"
        << (degrees[upper.first] + degrees[upper.second])/2.0 << endl;
    out << "max " << type << " degree:" << degrees.back() << endl;
    out << "stddev " << type << " degree:" << sqrt(S / (k-1)) << endl;
}

int main(int argc, char **argv)
{
    map<string, sort_order> orderings;
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

    orderings = {{"abs", abs_degree}, {"in", in_degree}, {"out", out_degree}};

    Graph<uint64_t, uint64_t> graph(graphs[0]);
    cout << "vertex count:" << graph.vertex_count << endl;
    cout << "edge count:" << graph.edge_count << endl;

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

        fiveDigitSummary(cout, p.first, degrees);
        }

    return 0;
}
