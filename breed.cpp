#include <functional>
#include <random>
#include <unordered_map>

#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

typedef Edge<uint64_t> edge;

static std::vector<edge>
gen_mutations(uint64_t vertex_count, double mutation_rate)
{
    mt19937_64 generator{random_device()()};

    std::vector<edge> mutations;
    uint64_t edge_count = vertex_count * vertex_count;

    binomial_distribution<uint64_t> num_mutations(edge_count, mutation_rate);
    uint64_t mutation_count = num_mutations(generator);

    mutations.reserve(mutation_count);

    for (uint64_t i = 0; i < mutation_count; i++) {
        mutations.emplace_back(i / vertex_count, i % vertex_count);
    }

    {
        unordered_map<uint64_t, edge> sparse_array;
        sparse_array.reserve(mutation_count);

        for (uint64_t i = 0; i < mutation_count; i++) {
            uniform_int_distribution<uint64_t> mutated_edge(i, edge_count);
            uint64_t idx = mutated_edge(generator);

            if (idx < mutation_count) {
                std::swap(mutations[i], mutations[idx]);
            } else {
                auto e = edge(idx / vertex_count, idx % vertex_count);
                std::swap(mutations[i], sparse_array.emplace(idx, e).first->second);
            }
        }
    }

    std::sort(mutations.begin(), mutations.end());

    return mutations;
}

static std::vector<edge>
flip_edges(const std::vector<edge>& edges)
{
    std::vector<edge> result;
    result.reserve(edges.size());

    for (auto&& e : edges) {
        result.emplace_back(e.out, e.in);
    }

    return result;
}

template<bool directed>
class Crossover {
    const uint64_t seed;
    const Graph<uint64_t,uint64_t> graph1;
    const Graph<uint64_t,uint64_t> graph2;
    const vector<edge> mutations;
    const vector<edge> rev_mutations;

    class Iterator;

    struct Edges {
        typedef Iterator const_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        const Graph<uint64_t,uint64_t>::Edges& edges1;
        const Graph<uint64_t,uint64_t>::Edges& edges2;
        const vector<edge>& mutations;
        const uint64_t& seed;

        Edges( const Graph<uint64_t,uint64_t>::Edges& e1
             , const Graph<uint64_t,uint64_t>::Edges& e2
             , const vector<edge>& muts
             , const uint64_t& s)
             : edges1(e1), edges2(e2), mutations(muts), seed(s)
        {}

        bool empty() const
        { return edges1.size == 0 || edges2.size == 0 || mutations.empty(); }

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
        mt19937_64 generator;
        uniform_int_distribution<int> crossover;
        simple_iterator<Graph<uint64_t,uint64_t>::Edges> edges1;
        simple_iterator<Graph<uint64_t,uint64_t>::Edges> edges2;
        simple_iterator<vector<edge>> mut;
        edge val;

      public:
        Iterator(const Edges &f, bool isEnd = false)
            : ended(isEnd), crossover(0,1), edges1(f.edges1), edges2(f.edges2)
            , mut(f.mutations) , val(0,0)
        {
            generator.seed(f.seed);
            this->operator++();
        }

        bool operator==(const Iterator& it) const
        { return ended == it.ended; }

        bool operator!=(const Iterator& it) const
        { return !operator==(it); }

        Iterator& operator++()
        {
            bool found = false;
            while (!found && (edges1 || edges2 || mut)) {
                if (edges1 && edges2 && *edges1 == *edges2) {
                    if (mut && *mut == *edges1) mut++;
                    else {
                        val = *edges1;
                        found = true;
                    }

                    edges1++;
                    edges2++;
                } else if (edges1 && (!edges2 || *edges1 < *edges2) && (!mut || *edges1 <= *mut)) {
                    if (crossover(generator)) {
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
                    if (crossover(generator)) {
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
            , mutations(gen_mutations(graph1.vertex_count, mutation_rate))
            , rev_mutations(flip_edges(mutations))
            , edges(graph1.edges, graph2.edges, mutations, seed)
            , rev_edges(graph1.rev_edges, graph2.rev_edges, rev_mutations, seed)
        {
            checkError(graph1.vertex_count == graph2.vertex_count,
                    "Incompatible graphs for crossover!");
        }

        Edges edges;
        Edges rev_edges;
};

typedef Crossover<true> DirectedCrossover;
typedef Crossover<false> UndirectedCrossover;

int main(int argc, char **argv)
{
    random_device device;
    checkError(argc == 4, "Wrong number of arguments.");

    DirectedCrossover crossover(argv[1], argv[2], stod(argv[3]));
    Graph<uint64_t,uint64_t>::outputSortedUniq("foo.graph", crossover.edges, crossover.rev_edges);

    return 0;
}
