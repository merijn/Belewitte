#ifndef OPENCL_HPP
#define OPENCL_HPP

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.h>
#endif

#include <array>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>

#include "Backend.hpp"
#include "utils/Util.hpp"

using std::cout;
using std::cerr;
using std::endl;

const char *openCLErrorToString(const cl_int code);

#define OPENCL_CHK(ans) { openCLAssert((ans), __FILE__, __LINE__); }
inline void openCLAssert(const cl_int code, const char *file, const int line)
{
    if (code != CL_SUCCESS) {
        cout.flush();
        cerr << "OpenCL error #"
             << code
             << " ("
             << file
             << ":"
             << line
             << "):"
             << endl
             << openCLErrorToString(code)
             << endl
             << endl
             << "Callstack:"
             << endl;
        dump_stack_trace(code);
    }
}

inline void __attribute__((noreturn))
opencl_error_callback(const char *errinfo, const void *, size_t , void *)
{
    cerr << "OpenCL Error: " << errinfo << endl;
    dump_stack_trace(EXIT_FAILURE);
}

class OpenCLBackend;

extern OpenCLBackend &OpenCL;

class OpenCLBackend : public Backend {
    friend class Backend;

    class opencl_alloc_t : public base_alloc_t
    {
        using mem = typename std::remove_pointer<cl_mem>::type;

        static std::shared_ptr<void>
        allocHostPtr(size_t sz)
        {
            void *hostPtr = new char[sz];
            return std::shared_ptr<void>(hostPtr, [] (void* p) {
                    delete[] static_cast<char*>(p);
            });
        }

        static std::shared_ptr<mem>
        allocDevPtr(cl_context ctx, size_t size, bool readonly)
        {
            cl_int ret;
            cl_mem_flags type = readonly ? CL_MEM_READ_ONLY : CL_MEM_READ_WRITE;
            cl_mem device = clCreateBuffer(ctx, type, size, nullptr, &ret);
            OPENCL_CHK(ret);
            return {device, clReleaseMemObject};
        }

      protected:
        std::shared_ptr<mem> devPtr;
        std::vector<opencl_alloc_t> localAllocs;

      public:
        opencl_alloc_t() {}

        opencl_alloc_t(cl_context ctx, size_t size, bool readonly)
         : base_alloc_t(allocHostPtr(size), size, readonly)
         , devPtr(allocDevPtr(ctx, size, readonly))
        {}

        opencl_alloc_t(const opencl_alloc_t& o)
         : base_alloc_t(o), devPtr(o.devPtr)
         , localAllocs(o.localAllocs)
        {}

        opencl_alloc_t(opencl_alloc_t&& o)
         : base_alloc_t(std::move(o)), devPtr(std::move(o.devPtr))
         , localAllocs(std::move(o.localAllocs))
        {}

        ~opencl_alloc_t();

        opencl_alloc_t& operator=(opencl_alloc_t&& other)
        {
            base_alloc_t::operator=(std::move(other));
            devPtr = std::move(other.devPtr);
            localAllocs = std::move(other.localAllocs);
            return *this;
        }

        void copyHostToDevImpl() final override
        {
            for (auto alloc : localAllocs) alloc.copyHostToDev();

            /*
            OPENCL_CHK(clEnqueueWriteBuffer(queue, devPtr.get(), CL_FALSE, 0,
                                            byteSize, hostPtr.get(), 0,
                                            nullptr, nullptr));
                                            */

            //if (associatedPtr) *associatedPtr = devPtr.get();
        }

        void copyDevToHostImpl() final override
        {
            /*
            OPENCL_CHK(clEnqueueReadBuffer(queue, devPtr.get(), CL_TRUE, 0,
                                           byteSize, hostPtr.get(), 0,
                                           nullptr, nullptr));
                                        */

            for (auto alloc : localAllocs) alloc.copyDevToHost();
        }

        void freeImpl() final override
        {
            devPtr.reset();
            localAllocs.clear();
        }

        void registerAlloc(const opencl_alloc_t& val, void** ptr)
        {
            localAllocs.emplace_back(val);
            localAllocs.back().associatedPtr = ptr;
            *ptr = val.hostPtr.get();
        }

        void registerAlloc(const opencl_alloc_t& val, void* ptr)
        { registerAlloc(val, static_cast<void**>(ptr)); }
    };

  public:
    template<typename V>
    class alloc_t : public typed_alloc_t<V, opencl_alloc_t>
    {
        friend OpenCLBackend;

      public:
        alloc_t() {}

        alloc_t(alloc_t&& o) : typed_alloc_t<V, opencl_alloc_t>(std::move(o))
        {}

        alloc_t(size_t N, bool ro)
            : typed_alloc_t<V, opencl_alloc_t>(N, sizeof(V) * N, ro)
        {}

        alloc_t& operator=(alloc_t&& o)
        {
            typed_alloc_t<V, opencl_alloc_t>::operator=(std::move(o));
            return *this;
        }

        template<typename T>
        void allocLocal(T * __restrict__ *loc, size_t N)
        { allocLocal(const_cast<T**>(loc), N); }

        template<typename T>
        void allocLocal(T **ptr, size_t N)
        { this->registerAlloc(alloc_t<T>(N, false), static_cast<void*>(ptr)); }
    };

    void initKernel(cl_uint) {}

    template<typename V, typename... Args>
    void initKernel(cl_kernel kernel, cl_uint offset,
                    alloc_t<V> val, Args... args)
    {
        OPENCL_CHK(clSetKernelArg(kernel, offset, sizeof val.device, val.device));
        initKernel(kernel, offset + 1, args...);
    }

    template<typename T, typename... Args>
    void initKernel(cl_kernel kernel, cl_uint offset, T val, Args... args)
    {
        OPENCL_CHK(clSetKernelArg(kernel, offset, sizeof val, val));
        initKernel(kernel, offset + 1, args...);
    }

    OpenCLBackend()
    {
        cl_int ret;
        cl_uint platformCount;
        cl_uint deviceCount;

        OPENCL_CHK(clGetPlatformIDs(0, NULL, &platformCount));
        platforms.resize(platformCount);
        OPENCL_CHK(clGetPlatformIDs(platformCount, platforms.data(), NULL));

        for (size_t i = 0; i < platformCount; i++) {
            OPENCL_CHK(
                clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0,
                                NULL, &deviceCount));
            devicesPerPlatform_.push_back(static_cast<int>(deviceCount));

            devices.emplace_back(deviceCount);
            OPENCL_CHK(
                clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL,
                                deviceCount, devices.back().data(), NULL));
        }

        ctxt = clCreateContext(NULL, static_cast<cl_uint>(devices[0].size()),
                                devices[0].data(), opencl_error_callback,
                                NULL, &ret);
        OPENCL_CHK(ret);

        queue = clCreateCommandQueue(ctxt, devices[0][0], 0, &ret);
        OPENCL_CHK(ret);

        activePlatform = 0;
        activeDevice = 0;
    }

    ~OpenCLBackend()
    {
        clFinish(queue);
        clReleaseCommandQueue(queue);
        clReleaseContext(ctxt);
    }

  public:
    static OpenCLBackend &get();

    void queryPlatform(size_t platform, bool verbose) override;
    void queryDevice(size_t platform, int dev, bool verbose) override;
    void setDevice(size_t platform, int device) override;
    void setWorkSizes(size_t dims, std::vector<size_t> blockSizes,
                        std::vector<size_t> gridSizes,
                        size_t sharedMem = 0) override;

    template<typename V>
    alloc_t<V> alloc(size_t count)
    { return alloc_t<V>(count, false); }

    template<typename V>
    alloc_t<V> allocConstant(int count)
    { return alloc_t<V>(count, true); }

    template<size_t N>
    cl_kernel createKernel(const char *kernelName, std::array<const char*, N> files, std::array<size_t, N> sizes)
    {
        cl_int ret;
        cl_kernel kernel;
        cl_program program;

        program = clCreateProgramWithSource(ctxt, N, files.data(), sizes.data(), &ret);
        OPENCL_CHK(ret);

        ret = clBuildProgram(program, 1, &activeDevice, NULL, NULL, NULL);
        if (ret != CL_SUCCESS) {
            char *buildLog;
            size_t logSize;

            OPENCL_CHK(clGetProgramBuildInfo(program, activeDevice,
                                                CL_PROGRAM_BUILD_LOG, 0, NULL, &logSize));
            buildLog = new char[logSize+1];
            OPENCL_CHK(clGetProgramBuildInfo(program, activeDevice,
                                                CL_PROGRAM_BUILD_LOG, logSize, buildLog, NULL));
            buildLog[logSize] = '\0';
            cerr << "OpenCL Build Error:" << endl << buildLog << endl;
            OPENCL_CHK(ret);
            delete[] buildLog;
        }

        kernel = clCreateKernel(program, kernelName, &ret);
        OPENCL_CHK(ret);

        return kernel;
    }

    void runKernel(cl_kernel kernel) const;

  private:
    std::vector<cl_platform_id> platforms;
    std::vector<std::vector<cl_device_id>> devices;

    cl_platform_id activePlatform;
    cl_device_id activeDevice;

    cl_context ctxt;
    cl_command_queue queue;
};
#endif
