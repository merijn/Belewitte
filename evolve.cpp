#include <getopt.h>

#include <algorithm>
#include <cstring>
#include <functional>
#include <random>
#include <unordered_map>

#include <boost/math/distributions/uniform.hpp>

#include "Connectivity.hpp"
#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

#define BIG_CONSTANT(x) (x##LLU)

typedef Edge<uint64_t> edge;
typedef Graph<uint64_t,uint64_t> Graph_t;

inline uint64_t rotl64(uint64_t x, int8_t r)
{ return (x << r) | (x >> (64 - r)); }

inline uint64_t fmix64(uint64_t k)
{
  k ^= k >> 33;
  k *= BIG_CONSTANT(0xff51afd7ed558ccd);
  k ^= k >> 33;
  k *= BIG_CONSTANT(0xc4ceb9fe1a85ec53);
  k ^= k >> 33;

  return k;
}

static inline uint64_t
murmurhash3(uint64_t seed, uint64_t k1, uint64_t k2)
{
    const uint64_t c1 = BIG_CONSTANT(0x87c37b91114253d5);
    const uint64_t c2 = BIG_CONSTANT(0x4cf5ad432745937f);

    uint64_t h1 = seed;
    uint64_t h2 = seed;

    k1 *= c1; k1  = rotl64(k1,31); k1 *= c2; h1 ^= k1;

    h1 = rotl64(h1,27); h1 += h2; h1 = h1*5+0x52dce729;

    k2 *= c2; k2  = rotl64(k2,33); k2 *= c1; h2 ^= k2;

    h2 = rotl64(h2,31); h2 += h1; h2 = h2*5+0x38495ab5;

    h1 ^= 2; h2 ^= 2;

    h1 += h2;
    h2 += h1;

    h1 = fmix64(h1);
    h2 = fmix64(h2);

    h1 += h2;
    h2 += h1;

    return h2;
}

template<bool undirected>
class Crossover {
    const uint64_t seed;
    const Graph_t graph1;
    const Graph_t graph2;
    const vector<edge> mutations;
    const vector<edge> rev_mutations;

    class Iterator;

    struct Edges {
        typedef Iterator const_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        const Graph_t::Edges& edges1;
        const Graph_t::Edges& edges2;
        const vector<edge>& mutations;
        const uint64_t& seed;
        const bool reversed;

        Edges( const Graph_t::Edges& e1
             , const Graph_t::Edges& e2
             , const vector<edge>& muts
             , const uint64_t& s, bool rev = false)
             : edges1(e1), edges2(e2), mutations(muts), seed(s), reversed(rev)
        {}

        bool empty() const
        { return edges1.size == 0 && edges2.size == 0 && mutations.empty(); }

        const_iterator begin() const { return const_iterator(*this, false); }
        const_iterator cbegin() const { return const_iterator(*this, false); }

        const_iterator end() const { return const_iterator(*this, true); }
        const_iterator cend() const { return const_iterator(*this, true); }

        const_reverse_iterator rbegin() const
        { return const_reverse_iterator(end()); }
        const_reverse_iterator crbegin() const
        { return const_reverse_iterator(end()); }

        const_reverse_iterator rend() const
        { return const_reverse_iterator(begin()); }
        const_reverse_iterator crend() const
        { return const_reverse_iterator(begin()); }
    };

    class Iterator : public std::iterator<std::forward_iterator_tag, edge> {
      public:
        typedef std::bidirectional_iterator_tag iterator_category;
        typedef const edge value_type;
        typedef value_type& reference;
        typedef value_type* pointer;

      private:
        bool ended;
        bool reversed;
        uint64_t seed;
        simple_iterator<Graph<uint64_t,uint64_t>::Edges> edges1;
        simple_iterator<Graph<uint64_t,uint64_t>::Edges> edges2;
        simple_iterator<vector<edge>> mut;
        edge val;

        bool crossover(uint64_t v1, uint64_t v2)
        {
            if (reversed) return murmurhash3(seed, v1, v2) & 1;
            else return murmurhash3(seed, v2, v1) & 1;
        }

      public:
        Iterator(const Edges &f, bool isEnd = false)
            : ended(isEnd), reversed(f.reversed), seed(f.seed)
            , edges1(f.edges1), edges2(f.edges2), mut(f.mutations), val(0,0)
        { this->operator++(); }

        bool operator==(const Iterator& it) const
        { return ended == it.ended; }

        bool operator!=(const Iterator& it) const
        { return !operator==(it); }

        Iterator& operator++()
        {
            bool found = false;
            while (!found && (edges1 || edges2 || mut)) {
                if (edges1 && edges2 && *edges1 == *edges2 && (!mut || *edges1 <= *mut)) {
                    if (mut && *mut == *edges1) mut++;
                    else {
                        val = *edges1;
                        found = true;
                    }

                    edges1++;
                    edges2++;
                } else if (edges1 && (!edges2 || *edges1 < *edges2) && (!mut || *edges1 <= *mut)) {
                    if (crossover(edges1->in, edges1->out)) {
                        if (mut && *mut == *edges1) mut++;
                        else {
                            val = *edges1;
                            found = true;
                        }
                    } else if (mut && *mut == *edges1) {
                        mut++;
                        val = *edges1;
                        found = true;
                    }
                    edges1++;
                } else if (edges2 && (!edges1 || *edges2 < *edges1) && (!mut || *edges2 <= *mut)) {
                    if (crossover(edges2->in, edges2->out)) {
                        if (mut && *mut == *edges2) mut++;
                        else {
                            val = *edges2;
                            found = true;
                        }
                    } else if (mut && *mut == *edges2) {
                        mut++;
                        val = *edges2;
                        found = true;
                    }
                    edges2++;
                } else if (mut && (!edges1 || *mut < *edges1) && (!edges2 || *mut < *edges2)) {
                    val = *mut;
                    found = true;
                    mut++;
                }
            }
            if (!found) ended = true;
            return *this;
        }

        Iterator operator++(int)
        {
            Iterator tmp(*this);
            ++*this;
            return tmp;
        }

        value_type& operator*() const
        { return val; }
    };

    public:
        Crossover
            ( std::string g1
            , std::string g2
            , double mutation_rate
            )
            : seed(random_device()()), graph1(g1), graph2(g2)
            , mutations(random_edges<uint64_t>(undirected, graph1.vertex_count, mutation_rate))
            , rev_mutations(reverse_and_sort(mutations))
            , vertex_count(graph1.vertex_count)
            , edges(graph1.edges, graph2.edges, mutations, seed)
            , rev_edges(graph1.rev_edges, graph2.rev_edges, rev_mutations, seed, true)
        {
            checkError(graph1.vertex_count == graph2.vertex_count,
                    "Incompatible graphs for crossover!");

            checkError(graph1.undirected == graph1.undirected || !undirected,
                    "Can't produce undirected crossover of directed graph!");
        }

        const uint64_t vertex_count;
        Edges edges;
        Edges rev_edges;
};

typedef Crossover<false> DirectedCrossover;
typedef Crossover<true> UndirectedCrossover;

static const char *execName = "evolve";

static void
computeFitness(const string file)
{
    Graph_t graph(file);
    vector<uint64_t> counts;

    double conn_percent = connectivity(graph);

    counts.reserve(graph.vertex_count);
    for (auto&& v : graph.vertices) {
        if (graph.undirected) counts.emplace_back(v.edges.size);
        else counts.emplace_back(v.edges.size + v.rev_edges.size);
    }

    boost::math::uniform_distribution<double> dist(0, graph.vertex_count);
    double Kmax = 0;
    sort(counts.begin(), counts.end());
    auto it = counts.begin();
    while (it != counts.end()) {
        uint64_t val = *it;
        it = upper_bound(it, counts.end(), val);

        double density = distance(counts.begin(), it) /
                         static_cast<double>(counts.size());

        Kmax = max(Kmax, abs(density - boost::math::cdf(dist, val)));
    }

    cout << (1.0 - Kmax) << " " << conn_percent << endl;
}

static void __attribute__((noreturn))
usage(int exitCode = EXIT_FAILURE)
{
    ostream& out(exitCode == EXIT_SUCCESS ? cout : cerr);
    out << "Usage:" << endl;
    out << execName << " [--help | -h]" << endl;
    out << execName << " fitness <graph>" << endl;
    out << execName << " [--directed | -d] [--undirected | -u] "
         << "[--mutation-rate <rate> | -m <rate>] crossover <graph 1> "
         << "<graph 2> <output graph>" << endl;
    exit(exitCode);
}

int main(int argc, char **argv)
{
    std::string name = argv[0];
    int undirected = false;
    double mutation_rate = 0.01;
    const char *optString = ":dm:uh?";
    static const struct option longopts[] = {
        { "directed", no_argument, &undirected, false},
        { "undirected", no_argument, &undirected, true},
        { "mutation-rate", required_argument, nullptr, 'm' },
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

            case 'm':
                mutation_rate = stod(optarg);
                break;

            case 'u':
                undirected = true;
                break;

            case 'h':
            case '?':
                usage(EXIT_SUCCESS);

            case ':':
                cerr << "Missing option for flag '" << optopt << "'." << endl;
                FALLTHROUGH;
            default:
                usage();
        }
    }

    argc -= optind;
    argv = &argv[optind];

    if (argc == 4 && !strcmp(argv[0], "crossover")) {
        if (undirected) {
            UndirectedCrossover crossover(argv[1], argv[2], mutation_rate);
            Graph_t::outputSortedUniq(argv[3], crossover.vertex_count,
                                      crossover.edges);
        } else {
            DirectedCrossover crossover(argv[1], argv[2], mutation_rate);
            Graph_t::outputSortedUniq(argv[3], crossover.vertex_count,
                    crossover.edges, crossover.rev_edges);
        }
        computeFitness(argv[3]);
    } else if (argc == 2 && !strcmp(argv[0], "fitness")) {
        computeFitness(argv[1]);
    } else {
        usage();
    }

    return 0;
}
