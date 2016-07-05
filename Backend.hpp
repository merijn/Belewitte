#ifndef __BACKEND_HPP__
#define __BACKEND_HPP__

#include <cstdlib>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "Util.hpp"

#define CEIL_DIV(x, y)          (((x) + ((y) - 1)) / (y))

template<typename Platform, typename... Args>
struct kernel {
    typedef void type;
};

template<typename V>
class platform_alloc_t {
    protected:
        V* host;

        platform_alloc_t(size_t N, bool readonly)
            : size(N), read_only(readonly)
        {}

    public:
        const size_t size;
        const bool read_only;

        virtual ~platform_alloc_t() {}

        virtual void copyHostToDev() = 0;
        virtual void copyDevToHost() = 0;
        virtual void free() = 0;

        V& operator[](size_t i)
        {
            checkError(i < size, "Attempting to index invalid offset! Index: ",
                       i, " Max: ", size);
            return host[i];
        }
};

template<typename T>
using alloc_t = std::shared_ptr<platform_alloc_t<T>>;

template<typename Platform>
class Backend {
    protected:
        Backend()
         : devicesPerPlatform(devicesPerPlatform_)
         , numComputeUnits(numComputeUnits_)
         , maxThreadsPerBlock(maxThreadsPerBlock_)
         , maxDims(maxDims_)
         , maxSharedMem(maxSharedMem_)
         , maxBlockSizes(maxBlockSizes_)
         , maxGridSizes(maxGridSizes_)
        {}

    public:
        virtual ~Backend() {}

        size_t platformCount()
        { return devicesPerPlatform.size(); }

        int deviceCount(size_t platform)
        { return devicesPerPlatform[platform]; }

        void listPlatforms(bool verbose)
        {
            for (size_t i = 0; i < devicesPerPlatform.size(); i++) {
                queryPlatform(i, verbose);
            }
        }

        void listDevices(size_t platform, bool verbose)
        {
            for (int i = 0; i < devicesPerPlatform[platform]; i++) {
                queryDevice(platform, i, verbose);
            }
        }

        std::pair<size_t,size_t>
        computeDivision(size_t count)
        {
            size_t i;
            std::pair<size_t,size_t> result;

            result.first = CEIL_DIV(count, numComputeUnits);

            for (i = 1; result.first > maxThreadsPerBlock; i++) {
                result.first = CEIL_DIV(count, i * numComputeUnits);
            }

            size_t adjust = result.first % 32;
            result.first += adjust ? 32 - adjust : 0;

            result.second = std::min(i * numComputeUnits,
                                     CEIL_DIV(count, result.first));

            return result;
        }

        Platform& self() { return *static_cast<Platform*>(this); }

        virtual void queryPlatform(size_t platform, bool verbose) = 0;
        virtual void queryDevice(size_t platform, int device, bool verbose) = 0;
        virtual void setDevice(size_t platform, int device) = 0;

        virtual void setWorkSizes(size_t dim, std::vector<size_t> blockSizes,
                                  std::vector<size_t> gridSizes,
                                  size_t sharedMem = 0) = 0;

        template<typename... Args>
        void runKernel
            ( typename kernel< Platform, Args... >::type kernel
            , Args... args)
        { self().template runKernel(kernel, args...); }

        template<typename V>
        alloc_t<V> alloc(size_t count)
        { return self().template alloc<V>(count); }

        template<typename V>
        alloc_t<V> allocConstant(size_t count)
        { return self().template allocConstant<V>(count); }

    protected:
        std::vector<int> devicesPerPlatform_;
        size_t numComputeUnits_;
        size_t maxThreadsPerBlock_;
        size_t maxDims_;
        size_t maxSharedMem_;
        std::vector<size_t> maxBlockSizes_;
        std::vector<size_t> maxGridSizes_;

    public:
        const std::vector<int> &devicesPerPlatform;
        const size_t &numComputeUnits;
        const size_t &maxThreadsPerBlock;
        const size_t &maxDims;
        const size_t &maxSharedMem;
        const std::vector<size_t> &maxBlockSizes;
        const std::vector<size_t> &maxGridSizes;
};
#endif
