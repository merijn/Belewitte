#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "Util.hpp"
#include "GraphFile.hpp"

template<typename T>
accessor<T>::converter::converter
    (const accessor<T>& a_, size_t i)
    : a(a_), idx(i)
{}

template<typename T>
accessor<T>::converter::converter(converter&& c)
    : a(c.a), idx(c.idx)
{}

template<typename T>
typename accessor<T>::converter&
accessor<T>::converter::operator=(T val)
{
    if (std::is_signed<T>::value) {
        checkError(val >= 0, "Attempting to store negative value: ", val,
                   " at index ", idx);
    }

    if (a.version == 0) {
        checkError(val <= a.maxVal, "Assigned value to big at index ",
                   idx, " value is: ", val, " max value is: ", a.maxVal);
        static_cast<int32_t*>(a.data)[idx] = static_cast<int32_t>(val);
    } else if (a.valueSize == 4) {
        checkError(val <= a.maxVal, "Assigned value to big at index ",
                   idx, " value is: ", val, " max value is: ", a.maxVal);
        static_cast<uint32_t*>(a.data)[idx] = static_cast<uint32_t>(val);
    } else if (a.valueSize == 8) {
        checkError(val <= a.maxVal, "Assigned value to big at index ",
                   idx, " value is: ", val, " max value is: ", a.maxVal);
        static_cast<uint64_t*>(a.data)[idx] = static_cast<uint64_t>(val);
    } else {
        reportError("Invalid graph file version or vertex/edge size!");
    }
    return *this;
}

template<typename T>
typename accessor<T>::converter&
accessor<T>::converter::operator=(const converter& val)
{ return *this = static_cast<T>(val); }

template<typename T>
accessor<T>::converter::operator T() const
{
    if (a.version == 0) {
        return static_cast<T>(static_cast<int32_t*>(a.data)[idx]);
    } else if (a.valueSize == 4) {
        return static_cast<T>(static_cast<uint32_t*>(a.data)[idx]);
    } else if (a.valueSize == 8) {
        return static_cast<T>(static_cast<uint64_t*>(a.data)[idx]);
    }
    reportError("Invalid graph file version or vertex/edge size!");
}

template<typename T>
accessor<T>::accessor
    ( void *ptr
    , uint64_t n
    , uint32_t vSize
    , uint64_t max
    , uint32_t versionNo)
    : data(ptr), size(n), valueSize(vSize), maxVal(max), version(versionNo)
{}

template<typename T>
typename accessor<T>::converter
accessor<T>::operator[](size_t n)
{
    checkError(n < size, "Index too large! Index: ", n, " Max: ", size);
    return converter(*this, n);
}

template<typename T>
const typename accessor<T>::converter
accessor<T>::operator[](size_t n) const
{
    checkError(n < size, "Index too large! Index: ", n, " Max: ", size);
    return converter(*this, n);
}

template<typename T>
void *
accessor<T>::end_ptr() const
{ return static_cast<char*>(data) + size * valueSize; }

static uint32_t
smallest_size(size_t val)
{ return val > std::numeric_limits<uint32_t>::max() ? 8 : 4; }

template<typename V, typename E>
GraphFile<V,E>::GraphFile(std::string fileName)
    : size(getFileSize(fileName))
    , data(getMmap(fileName, size))
    , version(detectVersion(data))
    , undirected(data[version ? 3 : 0])
    , vertex_size(version ? data[4] : 4)
    , edge_size(version ? data[5] : 4)
    , vertex_count(version ? reinterpret_cast<uint64_t*>(data)[3] : data[1])
    , edge_count(version ? reinterpret_cast<uint64_t*>(data)[4] : data[2])
    , vertices(&data[version ? 10 : 3], vertex_count + 1, vertex_size,
                edge_count, version)
    , edges(vertices.end_ptr(), edge_count, edge_size, vertex_count,
            version)
    , rev_vertices( undirected ? vertices.data : edges.end_ptr(),
                    vertex_count + 1, vertex_size, edge_count, version)
    , rev_edges(rev_vertices.end_ptr(), edge_count, edge_size,
                vertex_count, version)
{
    size_t checkSize;

    if (version == 0) {
        checkSize = 3 + vertex_count + 1 + edge_count;
        if (!undirected) checkSize += vertex_count + 1 + edge_count;
        checkSize *= sizeof(int32_t);
    } else {
        checkSize = initSize(undirected, vertex_count, edge_count);
    }

    checkError(size == checkSize,
               "Invalid file size! Wrong format? Expected: ", checkSize,
               " Found: ", size);
    if (version == 0) {
        checkError(edge_count <= std::numeric_limits<int32_t>::max(),
                   "Number of edges larger than storable in version 0 ",
                   "file format! Edge count: ", edge_count, " Max: ",
                   std::numeric_limits<int32_t>::max());
    } else if (vertex_size == 4) {
        checkError(edge_count <= std::numeric_limits<uint32_t>::max(),
                   "Number of edges larger than storable in vertex value!",
                   " Edge count: ", edge_count, " Max: ",
                   std::numeric_limits<uint32_t>::max());
    } else if (vertex_size == 8) {
        checkError(edge_count <= std::numeric_limits<uint64_t>::max(),
                   "Number of edges larger than storable in vertex value!",
                   " Edge count: ", edge_count, " Max: ",
                   std::numeric_limits<uint64_t>::max());
    } else {
        reportError("Invalid graph file version or vertex/edge size!");
    }

    if (version == 0) {
        checkError(vertex_count <= std::numeric_limits<int32_t>::max(),
                   "Number of vertices larger than storable in version 0 ",
                   "file format! Vertex count: ", vertex_count, " Max: ",
                   std::numeric_limits<int32_t>::max());
    } else if (edge_size == 4) {
        checkError(vertex_count <= std::numeric_limits<uint32_t>::max(),
                   "Number of vertices larger than storable in edge value!",
                   " Vertex count: ", vertex_count, " Max: ",
                   std::numeric_limits<uint32_t>::max());
    } else if (edge_size == 8) {
        checkError(vertex_count <= std::numeric_limits<uint64_t>::max(),
                   "Number of vertices larger than storable in edge value!",
                   " Vertex count: ", vertex_count, " Max: ",
                   std::numeric_limits<uint64_t>::max());
    } else {
        reportError("Invalid graph file version or vertex/edge size!");
    }
}

template<typename V, typename E>
GraphFile<V,E>::GraphFile
    ( std::string fileName
    , bool undir
    , size_t num_vertex
    , size_t num_edge
    )
    : size(initSize(undir, num_vertex, num_edge))
    , data(initFile(fileName, size))
    , version(1)
    , undirected(undir)
    , vertex_size(smallest_size(num_edge))
    , edge_size(smallest_size(num_vertex))
    , vertex_count(num_vertex)
    , edge_count(num_edge)
    , vertices(&data[10], vertex_count + 1, vertex_size, edge_count,
               version)
    , edges(vertices.end_ptr(), edge_count, edge_size, vertex_count,
            version)
    , rev_vertices(undirected ? vertices.data : edges.end_ptr(),
                   vertex_count + 1, vertex_size, edge_count, version)
    , rev_edges(rev_vertices.end_ptr(), edge_count, edge_size,
                vertex_count, version)
{
    data[0] = version;
    data[1] = 0;
    data[2] = 0;
    data[3] = undirected;
    data[4] = vertex_size;
    data[5] = edge_size;
    reinterpret_cast<uint64_t*>(data)[3] = vertex_count;
    reinterpret_cast<uint64_t*>(data)[4] = edge_count;
}

template<typename V, typename E>
GraphFile<V,E>::~GraphFile()
{ munmap(const_cast<uint32_t*>(data), size); }

template<typename V, typename E>
uint32_t
GraphFile<V,E>::detectVersion(uint32_t *data)
{
    if (data[1] == 0 && data[2] == 0) return data[0];
    return 0;
}

template<typename V, typename E>
size_t
GraphFile<V,E>::initSize(bool undir, size_t num_vertices, size_t num_edges)
{
    size_t size
        = 10 * sizeof(uint32_t)
        + (num_vertices + 1) * smallest_size(num_edges)
        + num_edges * smallest_size(num_vertices);

    if (!undir) {
        size += (num_vertices + 1) * smallest_size(num_edges)
             + num_edges * smallest_size(num_vertices);
    }

    return size;
}

template<typename V, typename E>
uint32_t *
GraphFile<V,E>::initFile(std::string fileName, size_t size)
{
    int fd = open(fileName.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        perror("open");
        dump_stack_trace(EXIT_FAILURE);
    }

    if (ftruncate(fd, static_cast<off_t>(size)) != 0) {
        perror("open");
        dump_stack_trace(EXIT_FAILURE);
    }

    uint32_t *data = static_cast<uint32_t*>(mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0));
    if (reinterpret_cast<intptr_t>(data) == -1) {
        perror("mmap");
        dump_stack_trace(EXIT_FAILURE);
    }
    close(fd);

    return data;
}

template<typename V, typename E>
size_t
GraphFile<V,E>::getFileSize(std::string fileName)
{
    struct stat statbuf;

    if (stat(fileName.c_str(), &statbuf) != 0) {
        perror("stat");
        dump_stack_trace(EXIT_FAILURE);
    }

    checkError(statbuf.st_size >= 0, "Found negative file size!");

    return static_cast<size_t>(statbuf.st_size);
}

template<typename V, typename E>
uint32_t *
GraphFile<V,E>::getMmap(std::string fileName, size_t size)
{
    int fd;
    void *result;

    fd = open(fileName.c_str(), O_RDONLY);
    if (fd == -1) {
        perror("open");
        dump_stack_trace(EXIT_FAILURE);
    }

    result = mmap(nullptr, size, PROT_READ, MAP_SHARED, fd, 0);
    close(fd);

    return static_cast<uint32_t*>(result);
}

template class accessor<unsigned>;
template class GraphFile<unsigned, unsigned>;

template class accessor<uint64_t>;
template class GraphFile<uint64_t, uint64_t>;
