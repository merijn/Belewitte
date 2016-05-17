#ifndef __PAGERANK_HPP__
#define __PAGERANK_HPP__

#include <string>
#include "CUDA.hpp"
#include "Timer.hpp"

void pagerank(CUDA&, TimerRegister&, size_t, const std::string, const char *, int, size_t, size_t);
#endif
