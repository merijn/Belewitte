#ifndef __GRAPH_H__
#define __GRAPH_H__

template<typename E>
struct edge {
    E in, out;

    edge(E i, E o) : in(i), out(o) {}
};

template<typename E>
bool operator<(const edge<E>& v1, const edge<E>& v2)
{ return v1.in < v2.in || (v1.in == v2.in && v1.out < v2.out); }

template<typename E>
bool operator>(const edge<E>& v1, const edge<E>& v2)
{ return v1.in > v2.in || (v1.in == v2.in && v1.out > v2.out); }
#endif
