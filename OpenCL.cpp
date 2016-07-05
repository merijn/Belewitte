#include "OpenCL.hpp"

using std::cout;
using std::cerr;
using std::endl;

OpenCLBackend &OpenCL = OpenCLBackend::get();

OpenCLBackend& OpenCLBackend::get()
{
    static OpenCLBackend opencl;
    return opencl;
}

const char *openCLErrorToString(const cl_int error)
{
    switch (error) {
        case CL_SUCCESS: return "Success";
        case CL_DEVICE_NOT_FOUND: return "Device not found";
        case CL_DEVICE_NOT_AVAILABLE: return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE: return "Compiler not availabe";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:
            return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES: return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY: return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:
            return "Profiling info not available";
        case CL_MEM_COPY_OVERLAP: return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH: return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:
            return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE: return "Program build failure";
        case CL_MAP_FAILURE: return "Image mapping failure";
        case CL_MISALIGNED_SUB_BUFFER_OFFSET:
            return "Misaligned sub-buffer offeset";
        case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST:
            return "Execution status error for events in wait list";
#ifdef CL_VERSION_1_2
        case CL_COMPILE_PROGRAM_FAILURE: return "Program compilation failure";
        case CL_LINKER_NOT_AVAILABLE: return "Linker not available";
        case CL_LINK_PROGRAM_FAILURE: return "Program linking failure";
        case CL_DEVICE_PARTITION_FAILED: return "Device partition failed";
        case CL_KERNEL_ARG_INFO_NOT_AVAILABLE:
            return "Kernel argument info not available";
#endif

        case CL_INVALID_VALUE: return "Invalid value";
        case CL_INVALID_DEVICE_TYPE: return "Invalid device type";
        case CL_INVALID_PLATFORM: return "Invalid platform";
        case CL_INVALID_DEVICE: return "Invalid device";
        case CL_INVALID_CONTEXT: return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:
            return "Invalid command queue properties";
        case CL_INVALID_COMMAND_QUEUE: return "Invalid command queue";
        case CL_INVALID_HOST_PTR: return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT: return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
            return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE: return "Invalid image size";
        case CL_INVALID_SAMPLER: return "Invalid sampler";
        case CL_INVALID_BINARY: return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS: return "Invalid build options";
        case CL_INVALID_PROGRAM: return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:
            return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME: return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION: return "Invalid kernel definition";
        case CL_INVALID_KERNEL: return "Invalid kernel";
        case CL_INVALID_ARG_INDEX: return "Invalid argument index";
        case CL_INVALID_ARG_VALUE: return "Invalid argument value";
        case CL_INVALID_ARG_SIZE: return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS: return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION: return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE: return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE: return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET: return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST: return "Invalid event wait list";
        case CL_INVALID_EVENT: return "Invalid event";
        case CL_INVALID_OPERATION: return "Invalid operation";
        case CL_INVALID_GL_OBJECT: return "Invalid GL object";
        case CL_INVALID_BUFFER_SIZE: return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL: return "Invalid MIP level";
        case CL_INVALID_GLOBAL_WORK_SIZE: return "Invalid global work size";
        case CL_INVALID_PROPERTY: return "Invalid property";
#ifdef CL_VERSION_1_2
        case CL_INVALID_IMAGE_DESCRIPTOR: return "Invalid image descriptor";
        case CL_INVALID_COMPILER_OPTIONS: return "Invalid compiler options";
        case CL_INVALID_LINKER_OPTIONS: return "Invalid linker options";
        case CL_INVALID_DEVICE_PARTITION_COUNT:
            return "Invalid device partition count";
#endif
        default:
            return "Unknown error";
    }
}

template<typename T>
static void oclQueryPlatform(cl_platform_id platform, cl_platform_info info, T **result)
{
    size_t size;

    OPENCL_CHK(clGetPlatformInfo(platform, info, 0, NULL, &size));
    *result = new T[size / sizeof (T)];
    OPENCL_CHK(clGetPlatformInfo(platform, info, size, *result, NULL));
}

template<typename T>
static void oclQueryDevice(cl_device_id device, cl_device_info info, T **result)
{
    size_t size;

    OPENCL_CHK(clGetDeviceInfo(device, info, 0, NULL, &size));
    *result = new T[size / sizeof (T)];
    OPENCL_CHK(clGetDeviceInfo(device, info, size, *result, NULL));
}

template<typename T>
static void oclQueryKernel(cl_kernel kernel, cl_device_id dev, cl_kernel_work_group_info info, T **result)
{
    size_t size;

    OPENCL_CHK(clGetKernelWorkGroupInfo(kernel, dev, info, 0, NULL, &size));
    *result = new T[size];
    OPENCL_CHK(clGetKernelWorkGroupInfo(kernel, dev, info, size, *result, NULL));
}

static void printSpaceSeperated(char *str, bool deepIndent)
{
    char *start = str;
    while (*str != '\0') {
        if (*str == ' ') {
            *str = '\0';
            if (deepIndent) cout << "\t    " << start << endl;
            else            cout << "\t" << start << endl;
            start = str + 1;
        }
        str++;
    }
}

void OpenCLBackend::queryPlatform(size_t platform, bool verbose)
{
    char *info;

    if (platform >= devicesPerPlatform.size()) {
        cerr << "Non-existent platform #"
             << platform
             << ", platform count is "
             << devicesPerPlatform.size()
             << endl;
        exit(EXIT_FAILURE);
    }

    cout << "Platform: " << platform << endl;

    oclQueryPlatform(platforms[platform], CL_PLATFORM_PROFILE, &info);
    cout << "    Profile: " << info << endl;
    delete[] info;

    oclQueryPlatform(platforms[platform], CL_PLATFORM_VERSION, &info);
    cout << "    Version: " << info << endl;
    delete[] info;

    oclQueryPlatform(platforms[platform], CL_PLATFORM_NAME, &info);
    cout << "    Name: " << info << endl;
    delete[] info;

    oclQueryPlatform(platforms[platform], CL_PLATFORM_VENDOR, &info);
    cout << "    Vendor: " << info << endl;
    delete[] info;

    oclQueryPlatform(platforms[platform], CL_PLATFORM_EXTENSIONS, &info);
    cout << "    Extensions:" << endl;
    printSpaceSeperated(info, false);
    delete[] info;

    cout << endl;

    listDevices(platform, verbose);
}

void OpenCLBackend::queryDevice(size_t platform, int dev, bool verbose)
{
    char *info;
    size_t *sizeVal;
    cl_uint *uintVal;
    cl_ulong *longVal;
    size_t idx = static_cast<size_t>(dev);

    if (platform >= devicesPerPlatform.size()) {
        cerr << "Non-existent platform #"
             << platform
             << ", platform count is "
             << devicesPerPlatform.size()
             << endl;
        exit(EXIT_FAILURE);
    } else if (dev >= devicesPerPlatform[platform]) {
        cerr << "Non-existent device #"
             << dev
             << ", device count for platform "
             << platform
             << " is "
             << devicesPerPlatform[idx]
             << endl;
        exit(EXIT_FAILURE);
    }

    cout << "    Device Number: " << dev << endl;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_NAME, &info);
    cout << "\tDevice Name: " << info << endl;
    delete[] info;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_VENDOR, &info);
    cout << "\tVendor Name: " << info << endl;
    delete[] info;

    oclQueryDevice(devices[platform][idx], CL_DRIVER_VERSION, &info);
    cout << "\tDriver Version: " << info << endl;
    delete[] info;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_VERSION, &info);
    cout << "\tOpenCL Version: " << info << endl;
    delete[] info;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_OPENCL_C_VERSION, &info);
    cout << "\tOpenCL C Version: " << info << endl;
    delete[] info;

    cout << endl;

    if (!verbose) return;

    cl_device_type *devType;
    oclQueryDevice(devices[platform][idx], CL_DEVICE_TYPE, &devType);
    /* FIXME */
    delete[] devType;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_COMPUTE_UNITS, &uintVal);
    cout << "\tMax Compute Units: " << *uintVal << endl;
    delete[] uintVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_GROUP_SIZE, &sizeVal);
    cout << "\tMax Workgroup Size: " << *sizeVal << endl;
    delete[] sizeVal;

    cout << endl;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, &uintVal);
    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_ITEM_SIZES, &sizeVal);
    cout << "\tMax Work Item Dimensions: " << sizeVal[0];

    for (cl_uint i = 1; i < *uintVal; i++) {
        cout << " x " << sizeVal[i];
    }
    cout << endl;

    delete[] sizeVal;
    delete[] uintVal;

    cout << endl;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_GLOBAL_MEM_SIZE, &longVal);
    cout << "\tGlobal Mem: " << *longVal << endl;
    delete[] longVal;

    cl_device_local_mem_type *memType;
    oclQueryDevice(devices[platform][idx], CL_DEVICE_LOCAL_MEM_TYPE, &memType);
    /* FIXME */
    delete[] memType;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_LOCAL_MEM_SIZE, &longVal);
    cout << "\tMax Local Mem: " << *longVal << endl;
    delete[] longVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_MEM_ALLOC_SIZE, &longVal);
    cout << "\tMax Mem Allocation: " << *longVal << endl;
    delete[] longVal;

    cout << endl;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_EXTENSIONS, &info);
    cout << "\tExtensions:" << endl;
    printSpaceSeperated(info, true);
    delete[] info;

    cout << endl;
}

void OpenCLBackend::setDevice(size_t platform, int device)
{
    cl_int ret;
    cl_uint *uintVal;
    cl_ulong *longVal;
    size_t *sizeVal;

    size_t idx = static_cast<size_t>(device);

    if (platform >= devicesPerPlatform.size()) {
        cerr << "Non-existent platform #"
             << platform
             << ", platform count is "
             << devicesPerPlatform.size()
             << endl;
        exit(EXIT_FAILURE);
    } else if (device >= devicesPerPlatform[platform]) {
        cerr << "Non-existent device #"
             << device
             << ", device count for platform "
             << platform
             << " is "
             << devicesPerPlatform[platform]
             << endl;
        exit(EXIT_FAILURE);
    }

    clFinish(queue);
    clReleaseCommandQueue(queue);
    clReleaseContext(ctxt);

    ctxt = clCreateContext(NULL, static_cast<cl_uint>(devicesPerPlatform[platform]),
                           devices[platform].data(), opencl_error_callback,
                           NULL, &ret);
    OPENCL_CHK(ret);
    queue = clCreateCommandQueue(ctxt, devices[platform][idx], 0, &ret);
    OPENCL_CHK(ret);

    activePlatform = platforms[platform];
    activeDevice = devices[platform][idx];

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, &uintVal);
    maxDims_ = static_cast<size_t>(*uintVal);
    delete[] uintVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_COMPUTE_UNITS, &uintVal);
    numComputeUnits_ = static_cast<size_t>(*uintVal);
    delete[] uintVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_GROUP_SIZE, &sizeVal);
    maxThreadsPerBlock_ = static_cast<size_t>(*sizeVal);
    delete[] sizeVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_LOCAL_MEM_SIZE, &longVal);
    maxSharedMem_ = static_cast<size_t>(*longVal);
    delete[] longVal;

    oclQueryDevice(devices[platform][idx], CL_DEVICE_MAX_WORK_ITEM_SIZES, &sizeVal);
    maxBlockSizes_.reserve(maxDims);
    for (size_t i = 0; i < maxDims; i++) {
        maxBlockSizes_.push_back(sizeVal[i]);
        maxGridSizes_.push_back(maxDims);
    }
    delete[] sizeVal;
}

void OpenCLBackend::runKernel(cl_kernel kernel) const
{
    cl_event evt;
    cl_uint *computeUnits;
    size_t globalSize;
    size_t *workgroupSize;

    oclQueryDevice(activeDevice, CL_DEVICE_MAX_COMPUTE_UNITS, &computeUnits);
    oclQueryKernel(kernel, activeDevice, CL_KERNEL_WORK_GROUP_SIZE, &workgroupSize);
    globalSize = *computeUnits * *workgroupSize;

    OPENCL_CHK(clEnqueueNDRangeKernel(queue, kernel, 1, NULL, &globalSize, workgroupSize, 0, NULL, &evt));
    OPENCL_CHK(clWaitForEvents(1, &evt));

    delete[] workgroupSize;
    delete[] computeUnits;
}

void OpenCLBackend::setWorkSizes
( size_t dims
, std::vector<size_t> blockSizes
, std::vector<size_t> gridSizes
, size_t)
{
    if (dims < 1 || dims > 3) {
        cerr << "Invalid number of dimensions: " << dims << endl;
        exit(EXIT_FAILURE);
    }

    if (blockSizes.size() != dims) {
        cerr << "Number of block sizes ("
             << blockSizes.size()
             << ") don't match specified number of dimensions ("
             << dims
             << ")"
             << endl;
        exit(EXIT_FAILURE);
    }

    if (gridSizes.size() != dims) {
        cerr << "Number of grid sizes ("
             << gridSizes.size()
             << ") don't match specified number of dimensions ("
             << dims
             << ")"
             << endl;
        exit(EXIT_FAILURE);
    }

    /* FIXME */
    exit(EXIT_FAILURE);
}
