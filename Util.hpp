#ifndef __UTIL_HPP__
#define __UTIL_HPP__

#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>

constexpr std::size_t operator "" _sz (unsigned long long int x)
{ return x; }

void __attribute__((noreturn)) out_of_memory(void);
void __attribute__((noreturn)) dump_stack_trace(int exit_code);
#endif
