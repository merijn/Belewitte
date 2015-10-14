#include <cstring>

#include <iostream>
#include <map>

#include "GraphFile.hpp"

using namespace std;

enum sort_order {
    in_degree,
    out_degree,
    abs_degree
};

int main(int argc, char **argv)
{
    string name;
    sort_order ordering;

    std::locale::global(std::locale(""));
    cout.imbue(std::locale());

    if (argc <= 2) return 1;

    if (!strcmp(argv[1], "abs")) ordering = abs_degree;
    else if (!strcmp(argv[1], "in")) ordering = in_degree;
    else if (!strcmp(argv[1], "out")) ordering = out_degree;
    else return EXIT_FAILURE;

    for (int i = 2; i < argc; i++) {
        name = string(argv[i]);

        const GraphFile<unsigned, unsigned> graph(name);
        map<int, int> degrees;

        for (unsigned j = 0; j < graph.vertex_count; j++) {
            int degree = 0;

            if (ordering == out_degree || ordering == abs_degree) {
                degree += graph.vertices[j+1] - graph.vertices[j];
            }

            if (ordering == in_degree || ordering == abs_degree) {
                degree += graph.rev_vertices[j+1] - graph.rev_vertices[j];
            }

            degrees[degree]++;
        }

        cout << name << ": " << endl;
        cout << "Vertex count: " << graph.vertex_count << endl;
        cout << "Edge count: " << graph.edge_count << endl;
        cout << "Degrees: " << endl;
        for (auto &pair : degrees) {
            cout << "\t" << pair.first << " : " << pair.second << endl;
        }
        cout << endl;
    }

    return 0;
}
