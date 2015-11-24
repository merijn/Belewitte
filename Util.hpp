#ifndef __UTIL_HPP__
#define __UTIL_HPP__

#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>

#include <iostream>
#include <string>

constexpr std::size_t operator "" _sz (unsigned long long int x)
{ return x; }

void __attribute__((noreturn)) out_of_memory(void);
void __attribute__((noreturn)) dump_stack_trace(int exit_code);

void printVals(void);

template<typename T, typename... Args>
inline void
printVals(T v, Args... args)
{
    std::cerr << v;
    printVals(args...);
}

template<typename... Args>
inline void __attribute__((noreturn))
reportError(Args... args)
{
    printVals(args...);
    std::cerr << std::endl;
    dump_stack_trace(EXIT_FAILURE);
}

template<typename... Args>
inline void
checkError(bool cond, Args... args)
{
    if (cond) return;
    reportError(args...);
}

template<typename C>
class simple_iterator : public C::const_iterator {
    typedef typename C::const_iterator parent_type;
    parent_type end;

    public:
        simple_iterator(const C& c)
            : parent_type(c.cbegin())
            , end(c.cend())
        {}

        explicit operator bool() const
        { return *this != end; }
};
#endif
