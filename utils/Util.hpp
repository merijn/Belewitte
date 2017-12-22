#ifndef UTIL_HPP
#define UTIL_HPP

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>

#include <atomic>
#include <iostream>
#include <map>
#include <memory>
#include <string>

#ifdef __clang__
#define FALLTHROUGH [[clang::fallthrough]]
#else
#define FALLTHROUGH
#endif

struct AlgorithmConfig;
class Options;

typedef void kernel_register_t
    ( std::map<std::string, AlgorithmConfig*>& kernels
    , const Options&
    , size_t run_count);

constexpr std::size_t operator "" _sz (unsigned long long int x);
constexpr std::size_t operator "" _sz (unsigned long long int x)
{ return x; }

template<typename T>
bool atomic_max(std::atomic<T>& max, T const& val) noexcept;

template<typename T>
bool atomic_max(std::atomic<T>& max, T const& val) noexcept
{
    bool r;
    T prev = max;
    while (prev < val && !(r = max.compare_exchange_weak(prev, val)));
    return r;
}

template<typename T>
bool atomic_min(std::atomic<T>& min, T const& val) noexcept;

template<typename T>
bool atomic_min(std::atomic<T>& min, T const& val) noexcept
{
    bool r;
    T prev = min;
    while (prev > val && !(r = min.compare_exchange_weak(prev, val)));
    return r;
}

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
    exit(EXIT_FAILURE);
}

template<typename... Args>
inline void __attribute__((noreturn))
stackTraceError(Args... args)
{
    reportError(args...);
    dump_stack_trace(EXIT_FAILURE);
}

template<typename... Args>
inline void
checkError(bool cond, Args... args)
{
    if (cond) return;
    reportError(args...);
}

template<typename T>
T* safe_dlsym(void *hnd, const std::string& name)
{
    void* result = dlsym(hnd, name.c_str());
    if (result == nullptr) {
        reportError("dlsym failed to find '", name, "': ", dlerror());
    }

    return reinterpret_cast<T*>(result);
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

template<typename C>
simple_iterator<C> make_simple_iterator(const C& val);

template<typename C>
simple_iterator<C> make_simple_iterator(const C& val)
{ return simple_iterator<C>(val); }

template<typename T>
class shared_array : public std::shared_ptr<void> {
    public:
      shared_array(void* data, std::function<void(void*)> deleter)
      : std::shared_ptr<void>(data, deleter)
      {}

      template<typename Ptr>
      shared_array(const shared_array<Ptr>& ptr, void* data)
      : std::shared_ptr<void>(ptr, data)
      {}

      T& operator[](size_t n)
      { return static_cast<T*>(this->get())[n]; }

      const T& operator[](size_t n) const
      { return static_cast<T*>(this->get())[n]; }

      template<typename Ptr>
      operator Ptr*() const { return static_cast<Ptr*>(this->get()); }
};

struct FiveDigitSummary
{
  private:
    static std::pair<size_t,size_t>
    medianIndices(size_t count)
    {
        if (count % 2 == 0) return { (count/2) - 1, count/2 };
        return { count/2, count/2 };
    }

  public:
    template<typename T>
    FiveDigitSummary(std::vector<T> values)
    {
        size_t half = values.size()/2;
        std::pair<size_t,size_t> medianPair = medianIndices(values.size());
        std::pair<size_t,size_t> lower = medianIndices(half);
        std::pair<size_t,size_t> upper = lower;

        upper.first += half + (values.size() % 2);
        upper.second += half + (values.size() % 2);

        std::sort(values.begin(), values.end());

        double total = 0;
        double M = 0.0;
        double S = 0.0;
        int k = 1;
        for (auto v : values) {
            total += v;
            double tmpM = M;
            M += (v - tmpM) / k;
            S += (v - tmpM) * (v - M);
            k++;
        }

        min = values.front();
        lowerQuantile = (values[lower.first] + values[lower.second])/2.0;
        median = (values[medianPair.first] + values[medianPair.second])/2.0;
        mean = total / values.size();
        upperQuantile = (values[upper.first] + values[upper.second])/2.0;
        max = values.back();
        stdDev = sqrt(S / (k-1));
    }

    size_t min;
    double lowerQuantile;
    double median;
    double mean;
    double upperQuantile;
    size_t max;
    double stdDev;
};
#endif
