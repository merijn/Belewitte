#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cassert>

#include "GraphFile.hpp"

GraphFile::accessor::accessor(int *ptr, size_t n)
    : data(ptr), size(n)
{}

GraphFile::accessor::accessor(int *ptr, int n)
    : data(ptr), size(static_cast<size_t>(n))
{ assert(n >= 0); }

const int&
GraphFile::accessor::operator[](size_t i) const
{
    assert(i < size);
    return data[i];
}

int&
GraphFile::accessor::operator[](size_t i)
{ return const_cast<int&>(const_cast<const GraphFile::accessor&>(*this).operator[](i)); }

const int&
GraphFile::accessor::operator[](int i) const
{
    assert(i >= 0 && static_cast<size_t>(i) < size);
    return data[i];
}

int&
GraphFile::accessor::operator[](int i)
{ return const_cast<int&>(const_cast<const GraphFile::accessor&>(*this).operator[](i)); }


GraphFile::GraphFile(std::string fileName)
    : size(getFileSize(fileName))
    , data(getMmap(fileName, size))
    , undirected(data[0])
    , vertex_count(data[1])
    , edge_count(data[2])
    , vertices(&data[3], vertex_count + 1)
    , edges(vertices.data + vertices.size, edge_count)
    , rev_vertices( undirected ? vertices.data : edges.data + edges.size
                  , vertex_count + 1)
    , rev_edges(rev_vertices.data + rev_vertices.size, edge_count)
{}

GraphFile::GraphFile(std::string fileName, bool undir, int num_vertex, int num_edge)
    : size(initSize(undir, static_cast<size_t>(num_vertex), static_cast<size_t>(num_edge)))
    , data(initFile(fileName, size))
    , undirected(data[0] = undir)
    , vertex_count(data[1] = num_vertex)
    , edge_count(data[2] = num_edge)
    , vertices(&data[3], vertex_count + 1)
    , edges(vertices.data + vertices.size, edge_count)
    , rev_vertices( undirected ? vertices.data : edges.data + edges.size
                  , vertex_count + 1)
    , rev_edges(rev_vertices.data + rev_vertices.size, edge_count)
{}

GraphFile::~GraphFile()
{ munmap(const_cast<int*>(data), size); }

size_t
GraphFile::initSize(bool undir, size_t num_vertices, size_t num_edges)
{
    size_t size = 3 + num_vertices + 1 + num_edges;
    if (!undir) size += num_vertices + 1 + num_edges;
    return size * sizeof(int);
}

int *
GraphFile::initFile(std::string fileName, size_t size)
{
    int fd = open(fileName.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    if (ftruncate(fd, static_cast<off_t>(size)) != 0) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    int *data = static_cast<int*>(mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0));
    if (data == reinterpret_cast<int*>(-1)) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }
    close(fd);

    return data;
}


size_t
GraphFile::getFileSize(std::string fileName)
{
    struct stat statbuf;

    if (stat(fileName.c_str(), &statbuf) != 0) {
        perror("stat");
        exit(EXIT_FAILURE);
    }

    return static_cast<size_t>(statbuf.st_size);
}

int *
GraphFile::getMmap(std::string fileName, size_t size)
{
    int fd;
    void *result;

    fd = open(fileName.c_str(), O_RDONLY);
    if (fd == -1) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    result = mmap(nullptr, size, PROT_READ, MAP_SHARED, fd, 0);
    close(fd);

    return static_cast<int*>(result);
}
