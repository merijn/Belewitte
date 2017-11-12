#ifndef CUDA_HPP
#define CUDA_HPP

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <vector>

#include <cuda_runtime.h>

#include "Backend.hpp"
#include "utils/Util.hpp"

#define ALIGN_UP(offset, alignment) \
   (offset) = ((offset) + (alignment)-1) & ~((alignment)-1)

using std::cerr;
using std::cout;
using std::endl;

#define CUDA_CHK(ans) { \
    cudaError_t code = ans; \
    if (code != cudaSuccess) { \
        cudaAssert(code, __FILE__, __LINE__); \
    } \
}
void __attribute__((noreturn))
cudaAssert(const cudaError_t code, const char *file, const int line);

class CUDABackend;

extern CUDABackend& CUDA;

class CUDABackend : public Backend {
    friend class Backend;

    class cuda_alloc_t : public base_alloc_t
    {
        static bool isManaged() { return false; }

        static std::shared_ptr<void>
        allocHostPtr(size_t sz, bool managed)
        {
            void *hostPtr;

            if (managed) {
                CUDA_CHK(cudaMallocManaged(&hostPtr, sz, cudaMemAttachGlobal));
                CUDA_CHK(cudaDeviceSynchronize());

                return {hostPtr, cudaFree};
            } else {
                CUDA_CHK(cudaHostAlloc(&hostPtr, sz, cudaHostAllocDefault));
                return {hostPtr, cudaFreeHost};
            }
        }

        static std::shared_ptr<void>
        allocDevPtr(std::shared_ptr<void> p, size_t size, bool managed)
        {
            void *ptr;
            std::shared_ptr<void> result = p;

            if (!managed) {
                CUDA_CHK(cudaMalloc(&ptr, size));
                result = {ptr, cudaFree};
            }
            return result;
        }

      protected:
        bool managed;
        std::shared_ptr<void> devPtr;
        std::vector<cuda_alloc_t> localAllocs;

        void copyHostToDevImpl() final override
        {
            CUDA_CHK(cudaDeviceSynchronize());
            if (!managed) {
                for (auto alloc : localAllocs) alloc.copyHostToDev();

                CUDA_CHK(cudaMemcpy(devPtr.get(), hostPtr.get(), byteSize ,
                                    cudaMemcpyHostToDevice));

                if (associatedPtr) *associatedPtr = devPtr.get();
            }
        }

        void copyDevToHostImpl() final override
        {
            CUDA_CHK(cudaDeviceSynchronize());
            if (!managed) {
                CUDA_CHK(cudaMemcpy(hostPtr.get(), devPtr.get(), byteSize,
                                     cudaMemcpyDeviceToHost));

                for (auto alloc : localAllocs) alloc.copyDevToHost();
            }
        }

        void freeImpl() final override
        {
            CUDA_CHK(cudaDeviceSynchronize());
            devPtr.reset();
            localAllocs.clear();
        }

        void registerAlloc(const cuda_alloc_t& val, void** ptr)
        {
            localAllocs.emplace_back(val);
            localAllocs.back().associatedPtr = ptr;
            *ptr = val.hostPtr.get();
        }

      public:
        cuda_alloc_t() : managed(isManaged())
        {}

        cuda_alloc_t(size_t size, bool readonly)
         : base_alloc_t(allocHostPtr(size, isManaged()), size, readonly)
         , managed(isManaged()), devPtr(allocDevPtr(hostPtr, size, managed))
        {}

        cuda_alloc_t(const cuda_alloc_t& o)
         : base_alloc_t(o), managed(o.managed), devPtr(o.devPtr)
         , localAllocs(o.localAllocs)
        {}

        cuda_alloc_t(cuda_alloc_t&& o)
         : base_alloc_t(std::move(o)), managed(o.managed)
         , devPtr(std::move(o.devPtr)), localAllocs(std::move(o.localAllocs))
        {}

        ~cuda_alloc_t();

        cuda_alloc_t& operator=(cuda_alloc_t&& other)
        {
            base_alloc_t::operator=(std::move(other));
            managed = other.managed;
            devPtr = std::move(other.devPtr);
            localAllocs = std::move(other.localAllocs);
            return *this;
        }

        void registerLocalAlloc(void *ptr, const cuda_alloc_t& val)
        { registerAlloc(val, static_cast<void**>(ptr)); }
    };

  public:
    template<typename V>
    class alloc_t : public typed_alloc_t<V, cuda_alloc_t>
    {
        friend CUDABackend;

      public:
        alloc_t() {}

        alloc_t(alloc_t&& o) : typed_alloc_t<V,cuda_alloc_t>(std::move(o))
        {}

        alloc_t(size_t N, bool ro)
            : typed_alloc_t<V,cuda_alloc_t>(N, sizeof(V) * N, ro)
        {}

        alloc_t& operator=(alloc_t&& o)
        {
            typed_alloc_t<V, cuda_alloc_t>::operator=(std::move(o));
            return *this;
        }

        template<typename T>
        void allocLocal(T * __restrict__ *loc, size_t N)
        { allocLocal(const_cast<T**>(loc), N); }

        template<typename T>
        void allocLocal(T **ptr, size_t N)
        { this->registerLocalAlloc(static_cast<void*>(ptr), alloc_t<T>(N, false)); }
    };

  private:
    void initKernel(size_t) {}

    template<typename V, typename... Args>
    void
    initKernel(size_t off, const alloc_t<V> &val, const Args&... args)
    {
        V *devPtr = static_cast<V*>(val.devPtr.get());
        ALIGN_UP(off, __alignof(devPtr));
        CUDA_CHK(cudaSetupArgument(&devPtr, sizeof devPtr, off));
        initKernel(off + sizeof devPtr, args...);
    }

    template
    < typename T
    , typename... Args
    , typename = typename std::enable_if<std::is_fundamental<T>::value>::type
    >
    void
    initKernel(size_t off, const T &val, const Args&... args)
    {
        ALIGN_UP(off, __alignof(val));
        CUDA_CHK(cudaSetupArgument(&val, sizeof val, off));
        initKernel(off + sizeof val, args...);
    }

    CUDABackend()
    {
        cudaError_t result;

        devicesPerPlatform_.push_back(0);
        result = cudaGetDeviceCount(devicesPerPlatform_.data());
        if (result == cudaErrorNoDevice) return;
        /* Working around broken CUDA setup on DAS5 head-node */
        if (result == cudaErrorUnknown) return;
        CUDA_CHK(result);

        for (int i = 0; i < devicesPerPlatform[0]; i++) {
            props.emplace_back();
            CUDA_CHK( cudaGetDeviceProperties(&props.back(), i));
        }

        CUDA_CHK( cudaSetDevice(0));
        prop = props[0];
        maxDims_ = 3;
        initialised_ = true;
    }

    ~CUDABackend() {}

  public:
    template<typename T>
    struct HostToDev { typedef T type; };

    template<typename T>
    struct HostToDev<alloc_t<T>> { typedef T* type; };

    template<typename T>
    struct HostToDev<alloc_t<T>&> { typedef T* type; };

    template<typename T>
    struct DevToHost { typedef T type; };

    template<typename T>
    struct DevToHost<T*> { typedef alloc_t<T> type; };

    template<typename... Args>
    struct kernel {
        using type = void (*)(typename HostToDev<Args>::type...);
    };

    static CUDABackend& get();

    void queryPlatform(size_t platform, bool verbose) override;
    void queryDevice(size_t platform, int device, bool verbose) override;
    void setDevice(size_t platform, int device) override;
    void setWorkSizes(size_t dims, std::vector<size_t> blockSizes,
                        std::vector<size_t> gridSizes,
                        size_t sharedMem = 0) override;

    template<typename... Args>
    void
    runKernel(typename kernel<Args...>::type kernel, const Args&... args)
    {
        CUDA_CHK(cudaConfigureCall(grid, block, sharedMemSize));
        initKernel(0, args...);
        CUDA_CHK(cudaLaunch(reinterpret_cast<const void*>(kernel)));
    }

    template<typename V>
    alloc_t<V> alloc()
    { return alloc<V>(1); }

    template<typename V>
    alloc_t<V> alloc(size_t count)
    { return alloc_t<V>(count, false); }

    template<typename V>
    alloc_t<V> allocConstant()
    { return allocConstant<V>(1); }

    template<typename V>
    alloc_t<V> allocConstant(size_t count)
    { return alloc_t<V>(count, true); }

  private:
    cudaDeviceProp prop;
    std::vector<cudaDeviceProp> props;

    dim3 block;
    dim3 grid;
    size_t sharedMemSize;
};
#endif
