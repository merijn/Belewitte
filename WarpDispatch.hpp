#ifndef WARPDISPATCH_HPP
#define WARPDISPATCH_HPP

#include <cstring>
#include <iostream>

const size_t max_warp = 64;
const size_t max_multiplier = 10;

template
< template<size_t,size_t> class F
, size_t multiplier
, size_t warp
, size_t chunk = warp * multiplier
>
struct _warp_dispatch {
    template<typename... Args>
    static auto work(size_t x, size_t y, Args... args)
        -> decltype((F<warp,chunk>::work(args...)))
    {
        if (x == warp && y == chunk) {
            return F<warp,chunk>::work(args...);
        } else {
            return _warp_dispatch<F, multiplier, warp, chunk - warp>::work(x, y, args...);
        }
    }
};

template<template<size_t,size_t> class F, size_t multiplier>
struct _warp_dispatch<F, multiplier, 0, 0> {
    template<typename... Args>
    static auto work(size_t w, size_t c, Args... args)
        -> decltype((F<1,1>::work(args...)))
    {
        std::cerr << "Warp function not compiled for warp size " << w
                  << " and chunk size " << c << std::endl;
        exit(EXIT_FAILURE);
    }
};

template<template<size_t,size_t> class F, size_t multiplier, size_t warp>
struct _warp_dispatch<F, multiplier, warp, 0> {
    template<typename... Args>
    static auto work(size_t x, size_t y, Args... args)
        -> decltype((_warp_dispatch<F, multiplier, warp/2>::work(x, y, args...)))
    {
        return _warp_dispatch<F, multiplier, warp/2>::work(x, y, args...);
    }
};

template<template<size_t,size_t> class F>
using warp_dispatch = _warp_dispatch<F, max_multiplier, max_warp>;
#endif
