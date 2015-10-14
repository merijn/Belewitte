#ifndef __GRAPHFILE_HPP__
#define __GRAPHFILE_HPP__

#include <string>
#include <limits>

template<typename T>
class accessor {
    template<typename V, typename E>
    friend class GraphFile;

    class converter {
        public:
            converter(const accessor<T>& a, size_t idx);
            converter(converter&& v);
            converter& operator=(T val);
            converter& operator=(const converter& val);
            operator T() const;

        private:
            const accessor<T> &a;
            size_t idx;
    };

    void *data;

    public:
        accessor(void *ptr, uint64_t n, uint32_t valueSize,
                 uint64_t maxVal, uint32_t version);

        const uint64_t size;
        const uint32_t valueSize;
        const uint64_t maxVal;
        const uint32_t version;

        converter operator[](size_t n);
        const converter operator[](size_t n) const;
        void *end_ptr() const;
};


template<typename V, typename E>
class GraphFile {
    private:
        const size_t size;
        uint32_t *data;
        const uint32_t version;

        static uint32_t detectVersion(uint32_t *data);

        static size_t initSize(bool undirected, size_t vertex_count,
                               size_t edge_count);

        static uint32_t *initFile(std::string fileName, size_t size);

        static size_t getFileSize(std::string fileName);
        static uint32_t *getMmap(std::string fileName, size_t size);

    public:
        GraphFile(std::string fileName);
        GraphFile
            ( std::string fileName
            , bool undirected
            , size_t vertex_count
            , size_t edge_count
            );

        ~GraphFile();

        const bool undirected;
        const uint32_t vertex_size, edge_size;
        const uint64_t vertex_count, edge_count;
        accessor<V> vertices;
        accessor<E> edges;
        accessor<V> rev_vertices;
        accessor<E> rev_edges;
};
#endif
