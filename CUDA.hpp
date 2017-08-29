#ifndef CUDA_HPP
#define CUDA_HPP

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
            struct local_alloc {
                void **dest;
                void *h_ptr;
                void *d_ptr;
                size_t size;

                template<typename T>
                local_alloc(T **l, T *h, T *d, size_t n)
                    : dest(reinterpret_cast<void**>(l))
                    , h_ptr(reinterpret_cast<void*>(h))
                    , d_ptr(reinterpret_cast<void*>(d))
                    , size(n * sizeof(T))
                {}

                template<typename T>
                local_alloc(T *p)
                    : dest(nullptr)
                    , h_ptr(p)
                    , d_ptr(nullptr)
                    , size(0)
                {}
            };

            bool managed;
            V* dev_ptr;
            std::vector<local_alloc> localAllocs;

            void free()
            {
                if (managed) {
                    CUDA_CHK(cudaDeviceSynchronize());
                    for (auto alloc : localAllocs) {
                        CUDA_CHK(cudaFree(alloc.h_ptr));
                    }
                    CUDA_CHK(cudaFree(this->ptr));
                } else {
                    for (auto alloc : localAllocs) {
                        CUDA_CHK(cudaFreeHost(alloc.h_ptr));
                        CUDA_CHK(cudaFree(alloc.d_ptr));
                    }
                    CUDA_CHK(cudaFreeHost(this->ptr));
                    CUDA_CHK(cudaFree(this->dev_ptr));
                }
            }

            public:
                alloc_t() : dev_ptr(nullptr)
                {}

                alloc_t(size_t N, bool readonly, const cudaDeviceProp&)
                    : base_alloc_t<V>(N, readonly), managed(false)
                {
                    if (managed) {
                        CUDA_CHK(cudaMallocManaged(&this->ptr, N * sizeof(V),
                                 cudaMemAttachGlobal));
                        CUDA_CHK(cudaDeviceSynchronize());
                        dev_ptr = this->ptr;
                    } else {
                        CUDA_CHK(cudaHostAlloc(&this->ptr, N * sizeof(V),
                                 cudaHostAllocDefault));
                        CUDA_CHK(cudaMalloc(&dev_ptr, N * sizeof(V)));
                    }
                }

                alloc_t(alloc_t&& other)
                    : base_alloc_t<V>(std::move(other))
                    , managed(other.managed)
                    , dev_ptr(other.dev_ptr)
                    , localAllocs(std::move(other.localAllocs))
                { other.dev_ptr = nullptr; }

                ~alloc_t()
                { free(); }

                alloc_t& operator=(alloc_t&& other)
                {
                    free();
                    base_alloc_t<V>::operator=(std::move(other));
                    managed = other.managed;
                    dev_ptr = other.dev_ptr;
                    other.dev_ptr = nullptr;
                    localAllocs = std::move(other.localAllocs);
                    return *this;
                }

                void copyHostToDev() override
                {
                    if (managed) {
                        CUDA_CHK(cudaDeviceSynchronize());
                    } else {
                        for (auto alloc : localAllocs) {
                            CUDA_CHK(cudaMemcpy
                                ( alloc.d_ptr, alloc.h_ptr, alloc.size
                                , cudaMemcpyHostToDevice));

                            *alloc.dest = alloc.d_ptr;
                        }
                        CUDA_CHK(cudaMemcpy
                            ( dev_ptr, this->ptr, this->size * sizeof(V)
                            , cudaMemcpyHostToDevice));
                    }
                }

                void copyDevToHost() override
                {
                    if (managed) {
                        CUDA_CHK(cudaDeviceSynchronize());
                    } else {
                        CUDA_CHK(cudaMemcpy
                            ( this->ptr, dev_ptr, this->size * sizeof(V)
                            , cudaMemcpyDeviceToHost));

                        for (auto alloc : localAllocs) {
                            CUDA_CHK(cudaMemcpy
                                ( alloc.h_ptr, alloc.d_ptr, alloc.size
                                , cudaMemcpyDeviceToHost));

                            *alloc.dest = alloc.h_ptr;
                        }
                    }
                }

                template<typename T>
                void allocLocal(T * __restrict__ *loc, size_t N)
                { allocLocal(const_cast<T**>(loc), N); }

                template<typename T>
                void allocLocal(T **loc, size_t N)
                {
                    if (managed) {
                        CUDA_CHK(cudaMallocManaged(loc, N * sizeof(T),
                                 cudaMemAttachGlobal));
                        CUDA_CHK(cudaDeviceSynchronize());
                        localAllocs.emplace_back(*loc);
                    } else {
                        T *d_ptr;
                        CUDA_CHK(cudaHostAlloc(loc, N * sizeof(T),
                                 cudaHostAllocDefault));
                        CUDA_CHK(cudaMalloc(&d_ptr, N * sizeof(T)));
                        localAllocs.emplace_back(loc, *loc, d_ptr, N);
                    }
                }
        };

    private:
        void initKernel(size_t) {}

        template<typename V, typename... Args>
        void
        initKernel(size_t off, const alloc_t<V> &val, const Args&... args)
        {
            ALIGN_UP(off, __alignof(val.dev_ptr));
            CUDA_CHK(cudaSetupArgument(&val.dev_ptr, sizeof val.dev_ptr, off));
            initKernel(off + sizeof val.dev_ptr, args...);
        }

        template<typename T, typename... Args,
            typename = typename std::enable_if<std::is_fundamental<T>::value>::type>
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
        { return alloc_t<V>(count, false, prop); }

        template<typename V>
        alloc_t<V> allocConstant()
        { return allocConstant<V>(1); }

        template<typename V>
        alloc_t<V> allocConstant(size_t count)
        { return alloc_t<V>(count, true, prop); }

    private:
        cudaDeviceProp prop;
        std::vector<cudaDeviceProp> props;

        dim3 block;
        dim3 grid;
        size_t sharedMemSize;
};
#endif
