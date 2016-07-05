#ifndef __BFS_HPP__
#define __BFS_HPP__

#include <string>
#include "../CUDA.hpp"
#include "../Timer.hpp"

void bfs(CUDA&, TimerRegister&, size_t, const std::string, const std::string, int, size_t, size_t);
#endif
