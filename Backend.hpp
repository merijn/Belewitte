#ifndef __BACKEND_HPP__
#define __BACKEND_HPP__

#include <vector>
#include "Util.hpp"

class Backend {
    protected:
        template<typename T>
        class base_alloc_t {
            protected:
                T* ptr;
                size_t size_;
                bool read_only;

                base_alloc_t()
                    : ptr(nullptr), size_(0), read_only(false), size(size_)
                {}

                base_alloc_t(size_t N, bool readonly)
                    : ptr(nullptr), size_(N), read_only(readonly), size(size_)
                {}

            public:
                const size_t &size;

                base_alloc_t(base_alloc_t&& o)
                 : ptr(o.ptr), size_(o.size), read_only(o.read_only)
                 , size(size_)
                {
                    o.ptr = nullptr;
                    o.size_ = 0;
                }

                virtual ~base_alloc_t() {}

                virtual void copyHostToDev() const = 0;
                virtual void copyDevToHost() const = 0;

                base_alloc_t& operator=(base_alloc_t&& other)
                {
                    ptr = other.ptr;
                    other.ptr = nullptr;

                    size_ = other.size;
                    other.size_ = 0;

                    read_only = other.read_only;

                    return *this;
                }

                T& operator*()
                {
                    auto p = const_cast<const base_alloc_t*>(this);
                    return const_cast<T&>(p->operator*());
                }

                const T& operator*() const
                { return ptr; }

                T* operator->()
                {
                    auto p = const_cast<const base_alloc_t*>(this);
                    return const_cast<T*>(p->operator->());
                }

                const T* operator->() const
                { return ptr; }

                T& operator[](size_t i)
                {
                    auto p = const_cast<const base_alloc_t*>(this);
                    return const_cast<T&>(p->operator[](i));
                }

                const T& operator[](size_t i) const
                {
                    checkError(i < size,
                        "Attempting to index invalid offset! Index: ", i,
                        " Max: ", size);
                    return ptr[i];
                }
        };

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
        virtual ~Backend();
        size_t platformCount();
        int deviceCount(size_t platform);
        void listPlatforms(bool verbose);
        void listDevices(size_t platform, bool verbose);
        std::pair<size_t,size_t> computeDivision(size_t count);

        virtual void queryPlatform(size_t platform, bool verbose) = 0;
        virtual void queryDevice(size_t platform, int device, bool verbose) = 0;
        virtual void setDevice(size_t platform, int device) = 0;

        virtual void setWorkSizes(size_t dim, std::vector<size_t> blockSizes,
                                  std::vector<size_t> gridSizes,
                                  size_t sharedMem = 0) = 0;

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
