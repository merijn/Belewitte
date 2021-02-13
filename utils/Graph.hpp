#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cassert>
#include <algorithm>
#include <limits>
#include <random>
#include <string>
#include <unordered_map>
#include <vector>

#include "Util.hpp"
#include "StatisticalSummary.hpp"

enum class Degrees { in, out, abs };

template<typename E>
struct Edge {
  E in, out;

  Edge(E i, E o) : in(i), out(o) {}

  bool operator==(const Edge& e) const
  { return in == e.in && out == e.out; }

  bool operator!=(const Edge& e) const
  { return !operator==(e); }

  bool operator<(const Edge& e) const
  { return in < e.in || (in == e.in && out < e.out); }

  bool operator<=(const Edge& e) const
  { return operator<(e) || operator==(e); }

  bool operator>(const Edge& e) const
  { return !operator<=(e); }

  bool operator>=(const Edge& e) const
  { return operator>(e) || operator==(e); }
};

template<typename E>
Edge<E> triangular_edge(uint64_t idx);

template<typename E>
Edge<E> triangular_edge(uint64_t idx)
{
    double root = sqrt(8*static_cast<double>(idx) + 1);
    uint64_t row = static_cast<uint64_t>(floor((root - 1)/2));
    uint64_t triangular = (row * (row+1))/2;
    uint64_t col = idx - triangular;
    return Edge<E>(row, col);
}

template<typename E>
std::vector<Edge<E>>
random_edges(bool undirected, uint64_t vertex_count, uint64_t edge_count);

template<typename E>
std::vector<Edge<E>>
random_edges(bool undirected, uint64_t vertex_count, uint64_t edge_count)
{
    std::mt19937_64 generator{std::random_device()()};
    std::vector<Edge<E>> edges;
    uint64_t max_edges;

    if (undirected) max_edges = (vertex_count * (vertex_count + 1))/2;
    else max_edges = vertex_count * vertex_count;

    edges.reserve(undirected ? 2*edge_count : edge_count);

    for (uint64_t i = 0; i < edge_count; i++) {
        if (undirected) edges.emplace_back(triangular_edge<E>(i));
        else edges.emplace_back(i / vertex_count, i % vertex_count);
    }

    {
        std::unordered_map<uint64_t, Edge<E>> sparse_array;
        sparse_array.reserve(edge_count);

        for (uint64_t i = 0; i < edge_count; i++) {
            std::uniform_int_distribution<uint64_t> new_edge(i, max_edges - 1);
            uint64_t idx = new_edge(generator);

            if (idx < edge_count) {
                std::swap(edges[i], edges[idx]);
            } else {
                Edge<E> edge(0,0);
                if (undirected) edge = triangular_edge<E>(idx);
                else edge = Edge<E>(idx / vertex_count, idx % vertex_count);

                std::swap(edges[i], sparse_array.emplace(idx, edge).first->second);
            }
        }
    }

    if (undirected) {
        uint64_t size = edges.size();
        for (uint64_t i = 0; i < size; i++) {
            Edge<E> e = edges[i];
            if (e.in != e.out) edges.emplace_back(e.out, e.in);
        }
    }

    std::sort(edges.begin(), edges.end());

    return edges;
}

template<typename E>
std::vector<Edge<E>>
random_edges(bool undirected, uint64_t vertex_count, double mutation_rate);

template<typename E>
std::vector<Edge<E>>
random_edges(bool undirected, uint64_t vertex_count, double mutation_rate)
{
    std::mt19937_64 generator{std::random_device()()};
    uint64_t max_edges;

    if (undirected) max_edges = (vertex_count * (vertex_count + 1))/2;
    else max_edges = vertex_count * vertex_count;

    std::binomial_distribution<uint64_t> num_muts(max_edges, mutation_rate);
    uint64_t mutation_count = num_muts(generator);

    return random_edges<E>(undirected, vertex_count, mutation_count);
}

template<typename E>
std::vector<Edge<E>>
reverse_and_sort(const std::vector<Edge<E>>& edges);

template<typename E>
std::vector<Edge<E>>
reverse_and_sort(const std::vector<Edge<E>>& edges)
{
    std::vector<Edge<E>> result;
    result.reserve(edges.size());

    for (auto&& e : edges) {
        result.emplace_back(e.out, e.in);
    }

    std::sort(result.begin(), result.end());

    return result;
}

template<typename T>
class Accessor {
  template<typename V, typename E>
  friend class MutableGraph;

  void *end_ptr() const
  { return static_cast<char*>(data) + size * valueSize; }

  const shared_array<char> data;

  class Converter {
    friend class Accessor;

    const Accessor<T> &a;
    size_t idx;

      Converter(const Accessor<T>& a_, size_t i)
        : a(a_), idx(i)
      {}

    public:
      Converter(const Converter&& c)
        : a(c.a), idx(c.idx)
      {}

      Converter& operator=(T val)
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

      Converter& operator=(const Converter& val)
      { return *this = static_cast<T>(val); }

      operator T() const
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
  };

  template<bool is_const>
  class Iterator : public std::iterator<std::random_access_iterator_tag, Converter> {
    public:
      typedef typename std::conditional
          <is_const, const Accessor<T>, Accessor<T>>::type accessor;
      typedef std::random_access_iterator_tag iterator_category;
      typedef uint64_t size_type;
      typedef int64_t difference_type;
      typedef typename std::conditional
          <is_const, const Converter, Converter>::type value_type;
      typedef value_type& reference;
      typedef value_type* pointer;

    private:
      accessor& acc;
      size_type offset;
      size_type endOffset;

    public:
      Iterator(accessor& a, size_type n, bool isEnd)
        : acc(a), offset(isEnd ? acc.size : n), endOffset(acc.size)
      {}

      Iterator(accessor& a, bool isEnd)
        : Iterator(a, 0, isEnd)
      {}

      Iterator(const Iterator& it)
        : acc(it.acc), offset(it.offset), endOffset(it.endOffset)
      {}

      Iterator(Iterator&& it)
        : acc(it.acc), offset(it.offset), endOffset(it.endOffset)
      {}

      Iterator& operator=(const Iterator& it)
      {
        checkError(acc == it.acc,
                "Can only assign iterators for same data!");
        offset = it.offset;
        endOffset = it.endOffset;
        return *this;
      }

      Iterator& operator++()
      { ++offset; return *this; }

      Iterator operator++(int)
      {
        Iterator tmp(*this);
        ++*this;
        return tmp;
      }

      Iterator& operator--()
      { --offset; return *this; }

      Iterator operator--(int)
      {
        Iterator tmp(*this);
        --*this;
        return tmp;
      }

      reference operator*() const
      { return acc[offset]; }

      bool operator==(const Iterator& it) const
      { return acc == it.acc && offset == it.offset && endOffset == it.endOffset; }

      bool operator!=(const Iterator& it) const
      { return !operator==(it); }

      bool operator<(const Iterator& it) const
      {
        checkError(acc == it.acc, "Iterators from different accessors!");
        return offset < it.offset;
      }

      bool operator<=(const Iterator& it) const
      { return operator<(it) || operator==(it); }

      bool operator>(const Iterator& it) const
      { return !operator<=(it); }

      bool operator>=(const Iterator& it) const
      { return operator==(it) || operator>(it); }

      Iterator& operator+=(size_type n)
      { offset += n; return *this; }

      Iterator operator+(size_type n) const
      { return Iterator(acc, offset + n); }

      Iterator& operator-=(size_type n)
      { offset -= n; return *this; }

      Iterator operator-(size_type n) const
      { return Iterator(acc, offset - n); }

      difference_type operator-(const Iterator& it) const
      {
          return static_cast<difference_type>(offset)
                 - static_cast<difference_type>(it.offset);
      }

      reference operator[](size_type n) const
      { return *(this + n); }
  };

    mutable Converter value;

  public:
    typedef Iterator<false> iterator;
    typedef Iterator<true> const_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

    const uint64_t size;
    const uint32_t valueSize;
    const uint64_t maxVal;
    const uint32_t version;

    Accessor
      ( const shared_array<uint32_t> &graph_data
      , void *ptr
      , uint64_t n
      , uint32_t vSize
      , uint64_t max
      , uint32_t versionNo)
      : data(graph_data, ptr), value(*this, 0), size(n), valueSize(vSize)
      , maxVal(max), version(versionNo)
    {}

    Accessor(Accessor& acc, uint64_t start, uint64_t end_)
      : data(acc.data, static_cast<char*>(acc.data) + start * acc.valueSize)
      , value(*this, 0), size(end_ - start), valueSize(acc.valueSize)
      , maxVal(acc.maxVal), version(acc.version)
    {}

    Accessor(const Accessor& acc)
      : data(acc.data), value(*this, 0), size(acc.size)
      , valueSize(acc.valueSize), maxVal(acc.maxVal), version(acc.version)
    {}

    bool operator==(const Accessor& acc) const
    {
      return data == acc.data && size == acc.size && valueSize == acc.valueSize
          && maxVal == acc.maxVal && version == acc.version;
    }

    bool operator!=(const Accessor& acc) const
    { return !operator==(acc); }

    Converter& operator[](size_t n)
    {
      checkError(n < size, "Index too large! Index: ", n, " Max: ", size);
      value.idx = n;
      return value;
    }

    const Converter& operator[](size_t n) const
    {
      checkError(n < size, "Index too large! Index: ", n, " Max: ", size);
      value.idx = n;
      return value;
    }

    iterator begin() { return iterator(*this, false); }
    const_iterator begin() const { return const_iterator(*this, false); }
    const_iterator cbegin() const { return const_iterator(*this, false); }

    iterator end() { return iterator(*this, true); }
    const_iterator end() const { return const_iterator(*this, true); }
    const_iterator cend() const { return const_iterator(*this, true); }

    reverse_iterator rbegin() { return reverse_iterator(end()); }
    const_reverse_iterator rbegin() const
    { return const_reverse_iterator(end()); }
    const_reverse_iterator crbegin() const
    { return const_reverse_iterator(end()); }

    reverse_iterator rend() { return reverse_iterator(begin()); }
    const_reverse_iterator rend() const
    { return const_reverse_iterator(begin()); }
    const_reverse_iterator crend() const
    { return const_reverse_iterator(begin()); }
};

template<typename V, typename E>
class MutableGraph {
  public:
    const std::string fileName;

    struct Vertex {
      const V id;
      const Accessor<E> edges;
      const Accessor<E> rev_edges;

      Vertex(V i, const Accessor<E>&& e, const Accessor<E>&& r)
        : id(i), edges(e), rev_edges(r)
      {}
    };

    class Vertices;
    class Edges;

    template<bool is_const>
    class VertexIterator : public std::iterator<std::random_access_iterator_tag, Vertex> {
      public:
        typedef typename std::conditional
            < is_const
            , const MutableGraph::Vertices
            , MutableGraph::Vertices>::type Vertices;

        typedef std::random_access_iterator_tag iterator_category;
        typedef uint64_t size_type;
        typedef int64_t difference_type;
        typedef typename std::conditional
            <is_const, const Vertex, Vertex>::type value_type;
        typedef value_type& reference;
        typedef value_type* pointer;

      private:
        Vertices& vertices;
        size_type offset;

      public:
        VertexIterator(Vertices& v, size_type n, bool isEnd = false)
          : vertices(v), offset(isEnd ? v.size : n)
        {}

        VertexIterator(Vertices& v, bool isEnd = false)
          : VertexIterator(v, 0, isEnd)
        {}

        VertexIterator(const VertexIterator& it)
          : vertices(it.vertices), offset(it.offset)
        {}

        VertexIterator(VertexIterator&& it)
          : vertices(it.vertices), offset(it.offset)
        {}

        VertexIterator& operator=(const VertexIterator& it)
        {
          checkError(vertices == it.vertices,
                "Can only assign iterators for same graph!");
          offset = it.offset;
          return *this;
        }

        VertexIterator& operator++()
        { ++offset; return *this; }

        VertexIterator operator++(int)
        {
          VertexIterator tmp(*this);
          ++*this;
          return tmp;
        }

        VertexIterator& operator--()
        { --offset; return *this; }

        VertexIterator operator--(int)
        {
          VertexIterator tmp(*this);
          --*this;
          return tmp;
        }

        value_type operator*() const
        { return vertices[offset]; }

        bool operator==(const VertexIterator& it) const
        { return vertices == it.vertices && offset == it.offset; }

        bool operator!=(const VertexIterator& it) const
        { return !operator==(it); }

        bool operator<(const VertexIterator& it) const
        {
          checkError(vertices == it.vertices,
              "Iterators from different graphs!");
          return offset < it.offset;
        }

        bool operator<=(const VertexIterator& it) const
        { return operator<(it) || operator==(it); }

        bool operator>(const VertexIterator& it) const
        { return !operator<=(it); }

        bool operator>=(const VertexIterator& it) const
        { return operator==(it) || operator>(it); }

        VertexIterator& operator+=(size_type n)
        { offset += n; return *this; }

        VertexIterator operator+(size_type n) const
        { return VertexIterator(vertices, offset + n); }

        VertexIterator& operator-=(size_type n)
        { offset -= n; return *this; }

        VertexIterator operator-(size_type n) const
        { return VertexIterator(vertices, offset - n); }

        difference_type operator-(const VertexIterator& it) const
        {
            return static_cast<difference_type>(offset)
                   - static_cast<difference_type>(it.offset);
        }

        reference operator[](size_type n) const
        { return *(this + n); }
    };

    template<bool is_const>
    class EdgeIterator : public std::iterator<std::bidirectional_iterator_tag, Edge<E>> {
      public:
        typedef typename std::conditional
            < is_const
            , const MutableGraph::Edges
            , MutableGraph::Edges>::type Edges;

        typedef std::bidirectional_iterator_tag iterator_category;
        typedef uint64_t size_type;
        typedef int64_t difference_type;
        typedef typename std::conditional
            <is_const, const Edge<E>, Edge<E>>::type value_type;
        typedef value_type& reference;
        typedef value_type* pointer;

      private:
        Edges& edges;
        size_type endVertex;
        size_type vertexId;
        size_type edgeId;
        mutable Edge<E> value;

      public:
        EdgeIterator(Edges& e, bool isEnd)
          : edges(e), endVertex(edges.vertices.size - 1)
          , vertexId(isEnd ? endVertex : 0)
          , edgeId(isEnd ? e.size : 0)
          , value(0,0)
        {
          while ( vertexId < endVertex
              && edges.vertices[vertexId + 1] - edges.vertices[vertexId] == 0)
          { vertexId++; }

          edgeId = edges.vertices[vertexId];
        }

        EdgeIterator(const EdgeIterator& it)
          : edges(it.edges), endVertex(it.endVertex), vertexId(it.vertexId)
          , edgeId(it.edgeId), value(0,0)
        {}

        EdgeIterator(EdgeIterator&& it)
          : edges(it.edges), endVertex(it.endVertex), vertexId(it.vertexId)
          , edgeId(it.edgeId), value(0,0)
        {}

        EdgeIterator& operator=(const EdgeIterator& it)
        {
          edges = it.edges;
          endVertex = it.endVertex;
          vertexId = it.vertexId;
          edgeId = it.edgeId;
          return this;
        }

        EdgeIterator& operator++()
        {
          ++edgeId;
          while ( vertexId < endVertex
              && edgeId >= edges.vertices[vertexId + 1])
          { vertexId++; }
          return *this;
        }

        EdgeIterator operator++(int)
        {
          EdgeIterator tmp(*this);
          ++*this;
          return tmp;
        }

        EdgeIterator& operator--()
        {
          --edgeId;
          while (vertexId > 0 && edgeId < edges.vertices[vertexId]) vertexId--;
          return *this;
        }

        EdgeIterator operator--(int)
        {
          EdgeIterator tmp(*this);
          --*this;
          return tmp;
        }

        reference operator*() const
        {
            value.in = vertexId;
            value.out = edges.edges[edgeId];
            return value;
        }

        pointer operator->() const
        {
            value.in = vertexId;
            value.out = edges.edges[edgeId];
            return &value;
        }

        bool operator==(const EdgeIterator& it) const
        {
          return edges == it.edges && endVertex == it.endVertex
              && vertexId == it.vertexId && edgeId == it.edgeId;
        }

        bool operator!=(const EdgeIterator& it) const
        { return !operator==(it); }

        bool operator<(const EdgeIterator& it) const
        {
          checkError(edges == it.edges,
              "Iterators from different graphs!");
          return vertexId < it.vertexId || (vertexId == it.vertexId && edgeId < it.edgeId);
        }

        bool operator<=(const EdgeIterator& it) const
        { return operator<(it) || operator==(it); }

        bool operator>(const EdgeIterator& it) const
        { return !operator<=(it); }

        bool operator>=(const EdgeIterator& it) const
        { return operator==(it) || operator>(it); }
    };

    class Vertices {
      Accessor<V>& vertices;
      Accessor<V>& rev_vertices;
      Accessor<E>& edges;
      Accessor<E>& rev_edges;

      Vertices(Vertices&) = delete;
      Vertices(Vertices&&) = delete;

      public:
        typedef VertexIterator<false> iterator;
        typedef VertexIterator<true> const_iterator;
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        const decltype(vertices.size) size;

        Vertices(Accessor<V>& v, Accessor<V>& rv, Accessor<E>& e, Accessor<E>& re)
          : vertices(v), rev_vertices(rv), edges(e), rev_edges(re), size(v.size - 1)
        {}

        Vertex operator[](uint64_t n)
        {
          checkError(n < size , "Index too large! Index: ", n, " Max: ", size);
          return Vertex(n, Accessor<E>(edges, vertices[n], vertices[n+1]),
                  Accessor<E>(rev_edges, rev_vertices[n], rev_vertices[n+1]));
        }

        const Vertex operator[](uint64_t n) const
        {
          checkError(n < size, "Index too large! Index: ", n, " Max: ", size);
          return Vertex(static_cast<V>(n),
                  Accessor<E>(edges, vertices[n], vertices[n+1]),
                  Accessor<E>(rev_edges, rev_vertices[n], rev_vertices[n+1]));
        }

        bool operator==(const Vertices& v) const
        { return vertices == v.vertices && edges == v.edges && size == v.size; }

        bool operator!=(const Vertices& v) const
        { return !operator==(v); }

        iterator begin() { return iterator(*this); }
        const_iterator begin() const { return const_iterator(*this); }
        const_iterator cbegin() const { return const_iterator(*this); }

        iterator end() { return iterator(*this, true); }
        const_iterator end() const { return const_iterator(*this, true); }
        const_iterator cend() const { return const_iterator(*this, true); }

        reverse_iterator rbegin() { return reverse_iterator(end()); }
        const_reverse_iterator rbegin() const
        { return const_reverse_iterator(end()); }
        const_reverse_iterator crbegin() const
        { return const_reverse_iterator(end()); }

        reverse_iterator rend() { return reverse_iterator(begin()); }
        const_reverse_iterator rend() const
        { return const_reverse_iterator(begin()); }
        const_reverse_iterator crend() const
        { return const_reverse_iterator(begin()); }
    };

    class Edges {
      template<bool>
      friend class EdgeIterator;

      Accessor<V>& vertices;
      Accessor<E>& edges;

      Edges(Edges&) = delete;
      Edges(Edges&&) = delete;

      public:
        typedef EdgeIterator<false> iterator;
        typedef EdgeIterator<true> const_iterator;
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        const decltype(edges.size) size;

        Edges(Accessor<V>& v, Accessor<E>& e)
          : vertices(v), edges(e), size(e.size)
        {}

        bool operator==(const Edges& e) const
        { return vertices == e.vertices && edges == e.edges && size == e.size; }

        bool operator!=(const Edges& e) const
        { return !operator==(e); }

        iterator begin() { return iterator(*this, false); }
        const_iterator begin() const { return const_iterator(*this, false); }
        const_iterator cbegin() const { return const_iterator(*this, false); }

        iterator end() { return iterator(*this, true); }
        const_iterator end() const { return const_iterator(*this, true); }
        const_iterator cend() const { return const_iterator(*this, true); }

        reverse_iterator rbegin() { return reverse_iterator(end()); }
        const_reverse_iterator rbegin() const
        { return const_reverse_iterator(end()); }
        const_reverse_iterator crbegin() const
        { return const_reverse_iterator(end()); }

        reverse_iterator rend() { return reverse_iterator(begin()); }
        const_reverse_iterator rend() const
        { return const_reverse_iterator(begin()); }
        const_reverse_iterator crend() const
        { return const_reverse_iterator(begin()); }
    };

  private:
    const size_t size;
    shared_array<uint32_t> data;
    const uint32_t version;

    static uint32_t
    smallest_size(size_t val)
    { return val > std::numeric_limits<uint32_t>::max() ? 8 : 4; }

    static uint32_t detectVersion(shared_array<uint32_t> data)
    {
      if (data[1] == 0 && data[2] == 0) return data[0];
      return 0;
    }

    static size_t initSize(bool undir, size_t num_vertices, size_t num_edges)
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

    static shared_array<uint32_t> initFile(std::string fileName, size_t size)
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

      void *ptr = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
      close(fd);

      if (reinterpret_cast<intptr_t>(ptr) == -1) {
        perror("mmap");
        dump_stack_trace(EXIT_FAILURE);
      }

      auto deleter = [=](void *data_ptr) { munmap(data_ptr, size); };
      return shared_array<uint32_t>(ptr, deleter);
    }

    static size_t getFileSize(std::string fileName)
    {
      struct stat statbuf;

      if (stat(fileName.c_str(), &statbuf) != 0) {
        perror("stat");
        std::cerr << "Failed to open graph: " << fileName << std::endl;
        dump_stack_trace(EXIT_FAILURE);
      }

      checkError(statbuf.st_size >= 0, "Found negative file size!");

      return static_cast<size_t>(statbuf.st_size);
    }

    static shared_array<uint32_t> getMmap(std::string fileName, size_t size)
    {
      int fd = open(fileName.c_str(), O_RDONLY);
      if (fd == -1) {
        perror("open");
        dump_stack_trace(EXIT_FAILURE);
      }

      void *ptr = mmap(nullptr, size, PROT_READ, MAP_SHARED, fd, 0);
      close(fd);

      if (reinterpret_cast<intptr_t>(ptr) == -1) {
        perror("mmap");
        dump_stack_trace(EXIT_FAILURE);
      }

      auto deleter = [=](void *data_ptr) { munmap(data_ptr, size); };
      return shared_array<uint32_t>(ptr, deleter);
    }

    static std::vector<Edge<E>>& empty_vector()
    {
        static auto& result = *new std::vector<Edge<E>>();
        return result;
    }

    template<bool sorted, bool uniq, typename C, typename D>
    struct GraphOutput {
        std::string fileName;
        uint64_t vertex_count;
        uint64_t edge_count;
        C& edges;
        D& rev_edges;

        GraphOutput
            ( std::string file, uint64_t vCount, uint64_t eCount, C& edges_
            , D& rev_edges_)
            : fileName(file), vertex_count(vCount), edge_count(eCount)
            , edges(edges_), rev_edges(rev_edges_)
        {
            sort_edges<sorted>();
            erase_edges<uniq>();
        }

        GraphOutput(std::string f, uint64_t vCount, C& edges_, D& rev_edges_)
            : GraphOutput(f, vCount, 0, edges_, rev_edges_)
        {
            for (auto&& edge : edges) {
                (void) edge;
                if (vCount == 0) {
                    vertex_count = std::max(vertex_count, std::max(edge.in, edge.out));
                }
                edge_count++;
            }
            if (vCount == 0) vertex_count++; // Vertices are zero indexed
        }

        GraphOutput(std::string f, C& edges_, D& rev_edges_)
            : GraphOutput(f, 0, edges_, rev_edges_)
        {}

        private:
            template<bool SORTED>
            void sort_edges(typename std::enable_if<SORTED>::type* = nullptr)
            {}

            template<bool SORTED>
            void sort_edges(typename std::enable_if<!SORTED>::type* = nullptr)
            {
                sort(edges.begin(), edges.end());
                sort(rev_edges.begin(), rev_edges.end());
            }

            template<bool UNIQ>
            void erase_edges(typename std::enable_if<UNIQ>::type* = nullptr)
            {}

            template<bool UNIQ>
            void erase_edges(typename std::enable_if<!UNIQ>::type* = nullptr)
            {
                edges.erase(unique(edges.begin(), edges.end()), edges.end());
                rev_edges.erase(unique(rev_edges.begin(), rev_edges.end()), rev_edges.end());
            }
    };

    template<bool sorted, bool unique, typename C, typename D = std::vector<Edge<E>>>
    static GraphOutput<sorted, unique, C,D>
    makeGraphOutput( std::string file, uint64_t vCount, uint64_t eCount
                   , C& edges_, D& rev_edges_ = empty_vector())
    { return GraphOutput<sorted, unique, C,D>(file, vCount, eCount, edges_, rev_edges_); }

    template<bool sorted, bool unique, typename C, typename D = std::vector<Edge<E>>>
    static GraphOutput<sorted, unique, C,D>
    makeGraphOutput( std::string file, uint64_t vCount, C& edges_
                   , D& rev_edges_ = empty_vector())
    { return GraphOutput<sorted, unique, C,D>(file, vCount, edges_, rev_edges_); }

    template<bool sorted, bool unique, typename C, typename D = std::vector<Edge<E>>>
    static GraphOutput<sorted, unique, C,D>
    makeGraphOutput(std::string file, C& edges_ , D& rev_edges_ = empty_vector())
    { return GraphOutput<sorted, unique, C,D>(file, edges_, rev_edges_); }


    template<typename C>
    static void writeEdges
        ( uint64_t vertex_count
        , Accessor<V>& vertices
        , Accessor<E>& edges
        , const C& edgeCollection)
    {
        uint64_t vertex = 0, edgeOffset = 0;

        if (edgeCollection.empty()) return;

        vertices[0] = edgeOffset;
        for (auto&& edge : edgeCollection) {
            while (vertex < edge.in) {
               vertices[++vertex] = edgeOffset;
            }
            edges[edgeOffset++] = edge.out;
        }

        while (vertex < vertex_count) {
            vertices[++vertex] = edgeOffset;
        }
    }

  public:
    MutableGraph(std::string file)
      : fileName(file)
      , size(getFileSize(fileName))
      , data(getMmap(fileName, size))
      , version(detectVersion(data))
      , undirected(data[version ? 3 : 0])
      , vertex_size(version ? data[4] : 4)
      , edge_size(version ? data[5] : 4)
      , vertex_count(version ? static_cast<uint64_t*>(data)[3] : data[1])
      , edge_count(version ? static_cast<uint64_t*>(data)[4] : data[2])
      , raw_vertices
          ( data
          , &data[version ? 10 : 3]
          , vertex_count + 1
          , vertex_size
          , edge_count
          , version)
      , raw_edges
          ( data
          , raw_vertices.end_ptr()
          , edge_count
          , edge_size
          , vertex_count
          , version)
      , raw_rev_vertices
          ( data
          , undirected ? raw_vertices.data : raw_edges.end_ptr()
          , vertex_count + 1
          , vertex_size
          , edge_count
          , version)
      , raw_rev_edges
          ( data
          , raw_rev_vertices.end_ptr()
          , edge_count
          , edge_size
          , vertex_count
          , version)
      , vertices(raw_vertices, raw_rev_vertices, raw_edges, raw_rev_edges)
      , edges(raw_vertices, raw_edges)
      , rev_edges(raw_rev_vertices, raw_rev_edges)
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
            // No-op: edge_count can never exceed std::numeric_limits<uint64_t>::max(),
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
            // No-op: vertex_count can never exceed std::numeric_limits<uint64_t>::max(),
        } else {
          reportError("Invalid graph file version or vertex/edge size!");
        }
      }

    MutableGraph
      ( std::string file
      , bool undir
      , uint64_t num_vertex
      , uint64_t num_edge
      )
      : fileName(file)
      , size(initSize(undir, num_vertex, num_edge))
      , data(initFile(fileName, size))
      , version(1)
      , undirected(undir)
      , vertex_size(smallest_size(num_edge))
      , edge_size(smallest_size(num_vertex))
      , vertex_count(num_vertex)
      , edge_count(num_edge)
      , raw_vertices
          ( data
          , &data[10]
          , vertex_count + 1
          , vertex_size
          , edge_count
          , version)
      , raw_edges
          ( data
          , raw_vertices.end_ptr()
          , edge_count
          , edge_size
          , vertex_count
          , version)
      , raw_rev_vertices
          ( data
          , undirected ? raw_vertices.data : raw_edges.end_ptr()
          , vertex_count + 1
          , vertex_size
          , edge_count
          , version)
      , raw_rev_edges
          ( data
          , raw_rev_vertices.end_ptr()
          , edge_count
          , edge_size
          , vertex_count
          , version)
      , vertices(raw_vertices, raw_rev_vertices, raw_edges, raw_rev_edges)
      , edges(raw_vertices, raw_edges)
      , rev_edges(raw_rev_vertices, raw_rev_edges)
    {
      data[0] = version;
      data[1] = 0;
      data[2] = 0;
      data[3] = undirected;
      data[4] = vertex_size;
      data[5] = edge_size;
      static_cast<uint64_t*>(data)[3] = vertex_count;
      static_cast<uint64_t*>(data)[4] = edge_count;
    }

    MutableGraph(const MutableGraph& graph)
      : fileName(graph.fileName)
      , size(graph.size)
      , data(graph.data)
      , version(graph.version)
      , undirected(graph.undirected)
      , vertex_size(graph.vertex_size)
      , edge_size(graph.edge_size)
      , vertex_count(graph.vertex_count)
      , edge_count(graph.edge_count)
      , raw_vertices(graph.raw_vertices)
      , raw_edges(graph.raw_edges)
      , raw_rev_vertices(graph.raw_rev_vertices)
      , raw_rev_edges(graph.raw_rev_edges)
      , vertices(raw_vertices, raw_rev_vertices, raw_edges, raw_rev_edges)
      , edges(raw_vertices, raw_edges)
      , rev_edges(raw_rev_vertices, raw_rev_edges)
    {}

    MutableGraph(MutableGraph&& graph)
      : fileName(graph.fileName)
      , size(graph.size)
      , data(graph.data)
      , version(graph.version)
      , undirected(graph.undirected)
      , vertex_size(graph.vertex_size)
      , edge_size(graph.edge_size)
      , vertex_count(graph.vertex_count)
      , edge_count(graph.edge_count)
      , raw_vertices(graph.raw_vertices)
      , raw_edges(graph.raw_edges)
      , raw_rev_vertices(graph.raw_rev_vertices)
      , raw_rev_edges(graph.raw_rev_edges)
      , vertices(raw_vertices, raw_rev_vertices, raw_edges, raw_rev_edges)
      , edges(raw_vertices, raw_edges)
      , rev_edges(raw_rev_vertices, raw_rev_edges)
    {}

    ~MutableGraph() {}

    template<typename... Args>
    static void output(Args... args)
    { output(makeGraphOutput<false,false>(args...)); }

    template<typename... Args>
    static void outputUniq(Args... args)
    { output(makeGraphOutput<false,true>(args...)); }

    template<typename... Args>
    static void outputSorted(Args... args)
    { output(makeGraphOutput<true,false>(args...)); }

    template<typename... Args>
    static void outputSortedUniq(Args... args)
    { output(makeGraphOutput<true,true>(args...)); }

    template<bool sorted, bool uniq, typename C, typename D>
    static void output(GraphOutput<sorted,uniq,C,D> out)
    {
        bool undirected = out.rev_edges.empty();
        MutableGraph graph(out.fileName, undirected, out.vertex_count, out.edge_count);
        writeEdges(out.vertex_count, graph.raw_vertices, graph.raw_edges, out.edges);
        writeEdges(out.vertex_count, graph.raw_rev_vertices, graph.raw_rev_edges, out.rev_edges);
    }

    StatisticalSummary<double>
    degreeStatistics(Degrees d) const
    {
        std::vector<double> degrees;
        degrees.reserve(vertex_count);

        for (auto v : vertices) {
            size_t degree = 0;

            if (d == Degrees::out || d == Degrees::abs) {
                degree += v.edges.size;
            }

            if (d == Degrees::in || (d == Degrees::abs && !undirected)) {
                degree += v.rev_edges.size;
            }

            degrees.emplace_back(degree);
        }
        return StatisticalSummary<double>(std::move(degrees));
    }

    const bool undirected;
    const uint32_t vertex_size, edge_size;
    const uint64_t vertex_count, edge_count;

    Accessor<V> raw_vertices;
    Accessor<E> raw_edges;
    Accessor<E> raw_rev_vertices;
    Accessor<E> raw_rev_edges;

    Vertices vertices;
    Edges edges;
    Edges rev_edges;
};

template<typename V, typename E>
using Graph = const MutableGraph<V,E>;
#endif
