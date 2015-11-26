#include <fcntl.h>
#include <unistd.h>

#include <sys/mman.h>

#include <algorithm>
#include <array>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <limits>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

static void __attribute__((noreturn))
usage(const char *name)
{
    cerr << "Usage:" << endl;
    cerr << name << " normalise <graph> [<graphs>...]" << endl;
    cerr << name << " lookup <map> <id> ..." << endl;
    cerr << name << " revlookup <map> <id> ..." << endl;
    exit(EXIT_FAILURE);
}

static size_t
translate(unordered_map<string,uint64_t> &map, uint64_t &unique_id, string key)
{
    uint64_t result;
    try {
        result = map.at(key);
    } catch (const out_of_range &oor) {
        (void) oor;
        map.insert({key, unique_id});
        result = unique_id++;
    }

    return result;
}

static istream&
ignoreLine(std::istream& is)
{
    // The characters in the stream are read one-by-one using a std::streambuf.
    // That is faster than reading them one-by-one using the std::istream.
    // Code that uses streambuf this way must be guarded by a sentry object.
    // The sentry object performs various tasks,
    // such as thread synchronization and updating the stream state.

    istream::sentry se(is, true);
    streambuf* sb = is.rdbuf();

    for (;;) {
        switch (sb->sbumpc()) {
            case EOF:
                is.setstate(ios::eofbit);
                [[clang::fallthrough]];
            case '\n':
            case '\r':
            case '\f':
                return is;
            default:
                break;
        }
    }
}

static void
normalise(const string path, const string graphName, bool undirected)
{
    bool cond;
    int next;
    uint64_t inId, outId;
    string in, out;
    uint64_t unique_id = 0;
    vector<Edge<uint64_t>> edges;
    unordered_map<string,uint64_t> lookup_map;

    ifstream graph (path + "/raw/" + graphName);
    ofstream lookup_table (path + "/map/" + graphName);

    while (!graph.eof()) {
        next = graph.peek();
        cond = next != '#' && next != EOF && next != '\n'
            && next != '\f' && next != '\r';

        if (cond) {
            graph >> in >> out;
            inId = translate(lookup_map, unique_id, in);
            outId = translate(lookup_map, unique_id, out);

            edges.emplace_back(inId, outId);
            if (undirected) edges.emplace_back(outId, inId);
        }

        ignoreLine(graph);
    }

    string name(path + graphName + ".graph");
    Graph<uint64_t,uint64_t>::output(name, edges);

    lookup_table << unique_id << endl;
    for (auto &pair : lookup_map) {
        lookup_table << pair.first << "\t" << pair.second << endl;
    }
}

static void
lookup(ifstream &lookup_table, const bool reverse, int argc, char **argv)
{
    int count, originalID, newID;
    unordered_map<int,int> lookup_map;

    lookup_table >> count;
    for (int i = 0; i < count; i++) {
        lookup_table >> originalID >> newID;
        if (reverse) lookup_map.insert({originalID, newID});
        else lookup_map.insert({newID, originalID});
    }

    for (int i = 1; i < argc; i++) {
        int id;
        try {
            id = lookup_map.at(stoi(argv[i]));
            cout << argv[i] << " -> " << id << endl;
        } catch (...) {
            cout << "Not a valid id: " << argv[i] << endl;
        }
    }
}

static bool
isUndirected(string &graph)
{
    bool result = false;
    const string end = string("-undirected");

    if (graph.length() >= end.length() &&
        0 == graph.compare(graph.length() - end.length(), end.length(), end))
    {
        //graph = graph.substr(0, graph.length() - end.length());
        result = true;
    }

    return result;
}

static void
split(const string file, bool lookup, string &path, string &graphName)
{
    size_t pathEnd, nameStart = file.rfind("/");
    if (nameStart == file.npos) {
        cerr << "Error parsing graph path." << endl;
        exit(EXIT_FAILURE);
    }

    if (lookup) {
        path = file.substr(0, nameStart);
        graphName = file.substr(nameStart + 1);
    } else {
        pathEnd = file.rfind("/", nameStart - 1);
        if (pathEnd == file.npos) {
            path = string("./");
            graphName = file.substr(nameStart + 1);
        } else {
            path = file.substr(0, pathEnd + 1);
            graphName = file.substr(nameStart + 1);
        }
    }
}

int main(int argc, char **argv)
{
    bool undirected;
    string path;
    string graphName;

    std::set_new_handler(out_of_memory);

    if (argc < 2) usage(argv[0]);

    if (!strcmp(argv[1], "normalise") && argc >= 3) {
        for (int i = 2; i < argc; i++) {
            cout << "Normalising " << argv[i] << endl;
            split(string(argv[i]), false, path, graphName);
            undirected = isUndirected(graphName);
            normalise(path, graphName, undirected);
        }
    } else if (!strcmp(argv[1], "lookup") && argc >= 4) {
        split(string(argv[2]), true, path, graphName);
        ifstream inputFile(path + "/map/" + graphName);
        lookup(inputFile, false, argc - 2, &argv[2]);
    } else if (!strcmp(argv[1], "revlookup") && argc >= 4) {
        split(string(argv[2]), true, path, graphName);
        ifstream inputFile(path + "/map/" + graphName);
        lookup(inputFile, true, argc - 2, &argv[2]);
    } else {
        usage(argv[0]);
    }

    return 0;
}
