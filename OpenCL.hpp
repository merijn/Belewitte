#ifndef __OPENCL_H__
#define __OPENCL_H__

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
#include "Util.hpp"

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

inline void __attribute__((noreturn)) opencl_error_callback(const char *errinfo, const void *, size_t , void *)
{
    cerr << "OpenCL Error: " << errinfo << endl;
    dump_stack_trace(EXIT_FAILURE);
}

class OpenCL : public Backend<OpenCL> {
    private:
        template<typename V>
        class opencl_alloc_t : public alloc_t<V>
        {
            cl_mem device;

            public:
                opencl_alloc_t(OpenCL &backend, size_t N, bool readonly)
                    : alloc_t<V>(N, readonly)
                {
                    cl_int ret;
                    int type = readonly ? CL_MEM_READ_ONLY : CL_MEM_READ_WRITE;

                    this->host = new V[N];
                    device = clCreateBuffer(backend.ctxt, type,
                                            N * sizeof(V), nullptr, &ret);
                    OPENCL_CHK(ret);
                }

                void copyHostToDev() override
                {
                    OPENCL_CHK(
                        clEnqueueWriteBuffer(queue, device, CL_FALSE, 0,
                                             this->size * sizeof(V),
                                             this->host, 0, nullptr, nullptr));
                }

                void copyDevToHost() override
                {
                    OPENCL_CHK(
                        clEnqueueReadBuffer(queue, this->device, CL_TRUE, 0,
                                            this->size * sizeof(V),
                                            this->host, 0, nullptr, nullptr));
                }

                void free()
                {
                    clReleaseMemObject(device);
                    delete[] this->host;
                }
        };

        void initKernel(cl_uint) {}

        template<typename V, typename... Args>
        void initKernel(cl_kernel kernel, cl_uint offset, alloc_t<V> val,
                        Args... args)
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

    public:
        typedef cl_kernel kernel_t;

        OpenCL() : Backend<OpenCL>("OpenCL")
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
                devicesPerPlatform.push_back(static_cast<int>(deviceCount));

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

        ~OpenCL()
        {
            clFinish(queue);
            clReleaseCommandQueue(queue);
            clReleaseContext(ctxt);
        }

        void queryPlatform(size_t platform, bool verbose) override;
        void queryDevice(size_t platform, int dev, bool verbose) override;
        void setDevice(size_t platform, int device) override;
        void setWorkSizes(size_t dims, std::vector<size_t> blockSizes,
                          std::vector<size_t> gridSizes,
                          size_t sharedMem = 0) override;


        template<typename V>
        alloc_t<V> alloc(size_t count)
        { return opencl_alloc_t<V>(this, count, false); }

        template<typename V>
        alloc_t<V> allocConstant(int count)
        { return opencl_alloc_t<V>(this, count, true); }

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
