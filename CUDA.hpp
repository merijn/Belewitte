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

template<typename... Args>
struct kernel<CUDABackend, Args...> {
        template<typename T>
        struct ArgType { typedef T type; };

        template<typename T>
        struct ArgType<alloc_t<T>> { typedef T* type; };

    using type = __global__ void (*)(typename ArgType<Args>::type...);
};

class CUDABackend : public Backend<CUDABackend> {
    friend class Backend<CUDABackend>;

    private:
        template<typename V>
        class cuda_alloc_t : public platform_alloc_t<V>
        {
            friend CUDABackend;

            V* device;

            public:
                cuda_alloc_t(size_t N, bool readonly)
                    : platform_alloc_t<V>(N, readonly)
                {
                    CUDA_CHK( cudaHostAlloc(&this->host, N * sizeof(V),
                                            cudaHostAllocWriteCombined));

                    CUDA_CHK( cudaMalloc(&device, N * sizeof(V)));
                }

                void copyHostToDev() override
                {
                    CUDA_CHK( cudaMemcpy(device, this->host,
                                         this->size * sizeof(V),
                                         cudaMemcpyHostToDevice));
                }

                void copyDevToHost() override
                {
                    CUDA_CHK( cudaMemcpy(this->host, device,
                                         this->size * sizeof(V),
                                         cudaMemcpyDeviceToHost));
                }

                void free() override
                {
                    cudaFreeHost(this->host);
                    cudaFree(device);
                }
        };

        void initKernel(size_t) {}

        template<typename V, typename... Args>
        void initKernel(size_t offset, alloc_t<V> val_, Args... args)
        {
            auto val = std::dynamic_pointer_cast<cuda_alloc_t<V>>(val_);
            ALIGN_UP(offset, __alignof(val->device));
            CUDA_CHK(cudaSetupArgument(&val->device, sizeof val->device, offset));
            initKernel(offset + sizeof val->device, args...);
        }

        template<typename T, typename... Args,
            typename = typename std::enable_if<std::is_fundamental<T>::value>::type>
        void initKernel(size_t offset, T val, Args... args)
        {
            ALIGN_UP(offset, __alignof(val));
            CUDA_CHK(cudaSetupArgument(&val, sizeof val, offset));
            initKernel(offset + sizeof val, args...);
        }

        CUDABackend() : Backend<CUDABackend>()
        {
            devicesPerPlatform_.push_back(0);
            CUDA_CHK( cudaGetDeviceCount(devicesPerPlatform_.data()));

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
        static CUDABackend& get();

        void queryPlatform(size_t platform, bool verbose) override;
        void queryDevice(size_t platform, int device, bool verbose) override;
        void setDevice(size_t platform, int device) override;
        void setWorkSizes(size_t dims, std::vector<size_t> blockSizes,
                          std::vector<size_t> gridSizes,
                          size_t sharedMem = 0) override;

        template<typename... Args>
        void runKernel(typename kernel<CUDABackend, Args...>::type kernel, Args... args)
        {
            CUDA_CHK(cudaConfigureCall(grid, block, sharedMemSize));
            initKernel(0, args...);
            CUDA_CHK(cudaLaunch(reinterpret_cast<const void*>(kernel)));
        }

        template<typename V>
        alloc_t<V> alloc(size_t count)
        { return std::make_shared<cuda_alloc_t<V>>(count, false); }

        template<typename V>
        alloc_t<V> allocConstant(size_t count)
        { return std::make_shared<cuda_alloc_t<V>>(count, true); }

    private:
        cudaDeviceProp prop;
        std::vector<cudaDeviceProp> props;

        dim3 block;
        dim3 grid;
        size_t sharedMemSize;
};
#endif
