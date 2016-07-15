#ifndef __CUDA_H__
#define __CUDA_H__

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <vector>

#include <cuda_runtime.h>

#include "Backend.hpp"
#include "Util.hpp"

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

    public:
        template<typename V>
        class alloc_t : public base_alloc_t<V>
        {
            friend CUDABackend;

            std::vector<void*> localAllocs;

            public:
                alloc_t()
                {}

                alloc_t(size_t N, bool readonly)
                    : base_alloc_t<V>(N, readonly)
                {
                    CUDA_CHK(cudaMallocManaged(&this->ptr, N * sizeof(V),
                             cudaMemAttachGlobal));
                    CUDA_CHK(cudaDeviceSynchronize());
                }

                alloc_t(alloc_t&& other)
                    : base_alloc_t<V>(std::move(other))
                    , localAllocs(std::move(other.localAllocs))
                {}

                ~alloc_t()
                {
                    CUDA_CHK(cudaDeviceSynchronize());
                    for (auto ptr : localAllocs) {
                        CUDA_CHK(cudaFree(ptr));
                    }
                    CUDA_CHK(cudaFree(this->ptr));
                }

                alloc_t& operator=(alloc_t&& other)
                {
                    base_alloc_t<V>::operator=(std::move(other));
                    localAllocs = std::move(other.localAllocs);
                    return *this;
                }

                void copyHostToDev() const override
                { CUDA_CHK(cudaDeviceSynchronize()); }

                void copyDevToHost() const override
                { CUDA_CHK(cudaDeviceSynchronize()); }

                template<typename T>
                void allocLocal(T **loc, size_t N)
                {
                    CUDA_CHK(cudaMallocManaged(loc, N * sizeof(T),
                             cudaMemAttachGlobal));
                    CUDA_CHK(cudaDeviceSynchronize());
                    localAllocs.push_back(*loc);
                }
        };

    private:
        void initKernel(size_t) {}

        template<typename V, typename... Args>
        void
        initKernel(size_t offset, const alloc_t<V> &val, const Args&... args)
        {
            ALIGN_UP(offset, __alignof(val.ptr));
            CUDA_CHK(cudaSetupArgument(&val.ptr, sizeof val.ptr, offset));
            initKernel(offset + sizeof val.ptr, args...);
        }

        template<typename T, typename... Args,
            typename = typename std::enable_if<std::is_fundamental<T>::value>::type>
        void
        initKernel(size_t offset, const T &val, const Args&... args)
        {
            ALIGN_UP(offset, __alignof(val));
            CUDA_CHK(cudaSetupArgument(&val, sizeof val, offset));
            initKernel(offset + sizeof val, args...);
        }

        CUDABackend()
        {
            devicesPerPlatform_.push_back(0);
            CUDA_CHK(cudaGetDeviceCount(devicesPerPlatform_.data()));

            for (int i = 0; i < devicesPerPlatform[0]; i++) {
                props.emplace_back();
                CUDA_CHK( cudaGetDeviceProperties(&props.back(), i));
            }

            CUDA_CHK( cudaSetDevice(0));
            prop = props[0];
            maxDims_ = 3;
        }

        ~CUDABackend() {}

    public:
        template<typename... Args>
        struct kernel {
                template<typename T>
                struct ArgType { typedef T type; };

                template<typename T>
                struct ArgType<alloc_t<T>> { typedef T* type; };

            using type = __global__ void (*)(typename ArgType<Args>::type...);
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
