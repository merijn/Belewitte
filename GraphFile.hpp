#ifndef __GRAPHFILE_HPP__
#define __GRAPHFILE_HPP__

#include <string>

class GraphFile {
    private:
        class accessor {
            friend GraphFile;

            private:
                int *data;

            public:
                accessor(int *ptr, size_t n);
                accessor(int *ptr, int n);

                const size_t size;

                int& operator[](size_t i);
                int& operator[](int i);
                const int& operator[](size_t i) const;
                const int& operator[](int i) const;
        };

        const size_t size;
        int *data;

        static size_t initSize(bool undirected, size_t vertex_count, size_t edge_count);
        static int* initFile(std::string fileName, size_t size);

        static size_t getFileSize(std::string fileName);
        static int *getMmap(std::string fileName, size_t size);

    public:
        GraphFile(std::string fileName);
        GraphFile
            ( std::string fileName
            , bool undirected
            , int vertex_count
            , int edge_count
            );

        ~GraphFile();

        const bool undirected;
        const int vertex_count, edge_count;
        accessor vertices, edges, rev_vertices, rev_edges;
};
#endif
