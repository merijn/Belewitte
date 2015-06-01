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

#define CUDA_CHK(ans) { cudaAssert((ans), __FILE__, __LINE__); }
inline void cudaAssert(const cudaError_t code, const char *file, const int line)
{
    if (code != cudaSuccess) {
        cout.flush();
        cerr << "CUDA error #"
             << code
             << " ("
             << file
             << ":"
             << line
             << "):"
             << endl
             << cudaGetErrorString(code)
             << endl
             << endl
             << "Callstack:"
             << endl;
        dump_stack_trace(code);
    }
}

class CUDA;

template<typename... Args>
struct kernel<CUDA, Args...> {
        template<typename T>
        struct ArgType { typedef T type; };

        template<typename T>
        struct ArgType<std::shared_ptr<alloc_t<T>>> { typedef T* type; };

    using type = __global__ void (*)(typename ArgType<Args>::type...);
};

class CUDA : public Backend<CUDA> {
    private:
        template<typename V>
        class cuda_alloc_t : public alloc_t<V>
        {
            friend CUDA;

            V* device;

            public:
                cuda_alloc_t(size_t N, bool readonly) : alloc_t<V>(N, readonly)
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
        void initKernel(size_t offset, std::shared_ptr<alloc_t<V>> val_, Args... args)
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

    public:
        CUDA() : Backend<CUDA>("CUDA")
        {
            devicesPerPlatform.push_back(0);
            CUDA_CHK( cudaGetDeviceCount(devicesPerPlatform.data()));

            for (int i = 0; i < devicesPerPlatform[0]; i++) {
                props.emplace_back();
                CUDA_CHK( cudaGetDeviceProperties(&props.back(), i));
            }

            CUDA_CHK( cudaSetDevice(0));
            prop = props[0];
            maxDims = 3;
        }

        ~CUDA() {}

        void queryPlatform(size_t platform, bool verbose) override;
        void queryDevice(size_t platform, int device, bool verbose) override;
        void setDevice(size_t platform, int device) override;
        void setWorkSizes(size_t dims, std::vector<size_t> blockSizes,
                          std::vector<size_t> gridSizes,
                          size_t sharedMem = 0) override;

        template<typename... Args>
        void runKernel(typename kernel<CUDA, Args...>::type kernel, Args... args)
        {
            CUDA_CHK(cudaConfigureCall(grid, block, sharedMemSize));
            initKernel(0, args...);
            CUDA_CHK(cudaLaunch(reinterpret_cast<const void*>(kernel)));
        }

        template<typename V>
        std::shared_ptr<alloc_t<V>> alloc(size_t count)
        { return std::make_shared<cuda_alloc_t<V>>(count, false); }

        template<typename V>
        std::shared_ptr<alloc_t<V>> allocConstant(size_t count)
        { return std::make_shared<cuda_alloc_t<V>>(count, true); }

    private:
        cudaDeviceProp prop;
        std::vector<cudaDeviceProp> props;

        dim3 block;
        dim3 grid;
        size_t sharedMemSize;
};
#endif
