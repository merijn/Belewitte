#include <getopt.h>
#include <cstring>

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>

#include "utils/Graph.hpp"
#include "utils/Util.hpp"

using namespace std;
using namespace boost::filesystem;

static const char *execName = "normalise-graph";

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " [--help | -h]" << endl;
    out << execName << " normalise [--directed | -d] [--undirected | -u] "
        << "<graph> [<graphs>...]" << endl;
    out << execName << " mtx <graph> [<graphs>...]" << endl;
    out << execName << " edge-list <graph> [<graphs>...]" << endl;
    out << execName << " lookup <map> <id> [<id>...]" << endl;
    out << execName << " revlookup <map> <id> [<id>...]" << endl;
    exit(exitCode);
}

static size_t
translate(unordered_map<string,uint64_t> &map, uint64_t &unique_id, string key)
{
    uint64_t result;
    try {
        result = map.at(key);
    } catch (const out_of_range &) {
        map.emplace(key, unique_id);
        result = unique_id++;
    }

    return result;
}

static void
normalise(const string graphName, bool undirected)
{
    bool bipartite = false;
    uint64_t inId, outId;
    string in, out;
    uint64_t unique_id = 0;
    vector<Edge<uint64_t>> edges;
    vector<Edge<uint64_t>> rev_edges;
    unordered_map<string,uint64_t> lookup_map;
    unordered_map<string,uint64_t> bipartite_lookup_map;

    std::ifstream graph (graphName);
    std::ofstream lookup_table (graphName + ".map");

    string line;
    string format;
    getline(graph, line);

    if (line[0] == '%') {
        if (line.find("asym") != string::npos) {
            undirected = false;
        } else {
            undirected = true;
            if (line.find("bip") != string::npos) {
                bipartite = true;
            }
        }
    }

    auto &out_lookup_map = bipartite ? bipartite_lookup_map : lookup_map;

    do {
        istringstream ss(line);

        if (line[0] != '#' && line[0] != '%') {
            ss >> in >> out;
            inId = translate(lookup_map, unique_id, in);
            outId = translate(out_lookup_map, unique_id, out);

            edges.emplace_back(inId, outId);
            if (undirected) edges.emplace_back(outId, inId);
            else rev_edges.emplace_back(outId, inId);
        }
    } while (getline(graph, line));

    string name(graphName + ".graph");
    Graph<uint64_t,uint64_t>::output(name, edges, rev_edges);

    lookup_table << unique_id << endl;
    for (auto &pair : lookup_map) {
        lookup_table << pair.first << "\t" << pair.second << endl;
    }
}

static void
lookup(const string fileName, const bool reverse, int argc, char **argv)
{
    std::ifstream lookup_table(fileName);
    int count, originalID, newID;
    unordered_map<uint64_t,uint64_t> lookup_map;

    lookup_table >> count;
    for (int i = 0; i < count; i++) {
        lookup_table >> originalID >> newID;
        if (reverse) lookup_map.emplace(originalID, newID);
        else lookup_map.emplace(newID, originalID);
    }

    for (int i = 1; i < argc; i++) {
        uint64_t id;
        try {
            id = lookup_map.at(stoul(argv[i]));
            cout << argv[i] << " -> " << id << endl;
        } catch (...) {
            cout << "Not a valid id: " << argv[i] << endl;
        }
    }
}

static void
convertMatrixMarket(const string graphFile, bool nonVoid = false)
{
    auto filename = path(graphFile).filename().replace_extension(".mtx");
    Graph<uint64_t,uint64_t> graph(graphFile);
    std::ofstream mtxFile(filename.string());
    mtxFile.imbue(std::locale("C"));
    mtxFile << "%%MatrixMarket matrix coordinate pattern general" << endl;
    mtxFile << graph.vertex_count << " " << graph.vertex_count << " "
            << graph.edge_count << endl;

    for (auto v : graph.vertices) {
        for (auto &e : v.edges) {
            mtxFile << v.id + 1 << "\t" << e + 1;
            if (nonVoid) mtxFile << "\t1.0";
            mtxFile << endl;
        }
    }
}

static void
convertEdgeList(const string graphFile)
{
    auto filename = path(graphFile).filename().replace_extension(".edges");
    Graph<uint64_t,uint64_t> graph(graphFile);
    std::ofstream edgeList(filename.string());
    edgeList.imbue(std::locale("C"));
    edgeList << graph.vertex_count << " " << graph.edge_count << endl;
    for (auto v : graph.vertices) {
        for (auto &e : v.edges) {
            edgeList << v.id << "\t" << e << endl;
        }
    }
}

int main(int argc, char **argv)
{
    int undirected = false;
    const char *optString = ":dm:uh?";
    static const struct option longopts[] = {
        { "directed", no_argument, &undirected, false},
        { "undirected", no_argument, &undirected, true},
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

    if (argc >= 2 && !strcmp(argv[0], "normalise")) {
        for (int i = 1; i < argc; i++) {
            cout << "Normalising: " << argv[i] << endl;
            normalise(argv[i], undirected);
        }
    } else if (argc >= 2 && !strcmp(argv[0], "mtx")) {
        for (int i = 1; i < argc; i++) {
            cout << "Converting to MatrixMarket: " << argv[i] << endl;
            convertMatrixMarket(argv[i]);
        }
    } else if (argc >= 2 && !strcmp(argv[0], "mtx-float")) {
        for (int i = 1; i < argc; i++) {
            cout << "Converting to MatrixMarket: " << argv[i] << endl;
            convertMatrixMarket(argv[i], true);
        }
    } else if (argc >= 2 && !strcmp(argv[0], "edge-list")) {
        for (int i = 1; i < argc; i++) {
            cout << "Converting to edge list: " << argv[i] << endl;
            convertEdgeList(argv[i]);
        }
    } else if (argc >= 3 && !strcmp(argv[0], "lookup")) {
        lookup(argv[1], false, argc - 2, &argv[2]);
    } else if (argc >= 3 && !strcmp(argv[0], "revlookup")) {
        lookup(argv[1], true, argc - 2, &argv[2]);
    } else {
        usage();
    }

    return EXIT_SUCCESS;
}
