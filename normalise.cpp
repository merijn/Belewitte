#include <getopt.h>
#include <cstring>

#include <fstream>

#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

static const char *execName = "normalise";

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " [--help | -h]" << endl;
    out << execName << " normalise [--directed | -d] [--undirected | -u] "
        << "<graph> [<graphs>...]" << endl;
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
normalise(const string graphName, bool undirected)
{
    bool cond;
    int next;
    uint64_t inId, outId;
    string in, out;
    uint64_t unique_id = 0;
    vector<Edge<uint64_t>> edges;
    unordered_map<string,uint64_t> lookup_map;

    ifstream graph (graphName);
    ofstream lookup_table (graphName + ".map");

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

    string name(graphName + ".graph");
    Graph<uint64_t,uint64_t>::output(name, edges);

    lookup_table << unique_id << endl;
    for (auto &pair : lookup_map) {
        lookup_table << pair.first << "\t" << pair.second << endl;
    }
}

static void
lookup(const string fileName, const bool reverse, int argc, char **argv)
{
    ifstream lookup_table(fileName);
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

            case ':':
                cerr << "Missing option for flag '" << optopt << "'." << endl;
                [[clang::fallthrough]];
            default:
                usage();
        }
    }

    argc -= optind;
    argv = &argv[optind];

    if (argc >= 2 && !strcmp(argv[0], "normalise")) {
        for (int i = 1; i < argc; i++) {
            normalise(argv[i], undirected);
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
