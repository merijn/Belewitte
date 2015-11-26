#include "Graph.hpp"
#include "Util.hpp"

using namespace std;

int main(int argc, char **argv)
{
    checkError(argc >= 2, "Not enough arguments!");

    for (int i = 1; i < argc; i++) {
        Graph<uint64_t,uint64_t> graph(argv[i]);
        cout << "Undirected: " << (graph.undirected ? "true" : "false") << endl;
        cout << "Vertex count: " << graph.vertex_count << endl;
        cout << "Edge count: " << graph.edge_count << endl;
        for (auto v : graph.vertices) {
            cout << v.id << endl;
            for (auto e : v.edges) {
              cout << "  -> " << e << endl;
            }
        }

        /*
        cout << endl;

        for (auto v : graph.rev_vertices) {
            cout << v.id << endl;
            for (auto e : v.edges) {
              cout << "  -> " << e << endl;
            }
        }

        cout << endl;

        for (auto e : graph.edges) {
              cout << e.in << " -> " << e.out << endl;
        }

        cout << endl;

        for (auto e : graph.rev_edges) {
              cout << e.in << " -> " << e.out << endl;
        }
        */
    }
    return 0;
}
