#include "CUDA.hpp"

using std::cerr;
using std::cout;
using std::endl;

void cudaAssert(const cudaError_t code, const char *file, const int line)
{
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

CUDABackend::cuda_alloc_t::~cuda_alloc_t()
{}

CUDABackend& CUDA = CUDABackend::get();

CUDABackend& CUDABackend::get()
{
    static CUDABackend cuda;
    return cuda;
}

void CUDABackend::queryPlatform(size_t platform, bool verbose)
{
    if (platform >= devicesPerPlatform.size()) {
        cerr << "Non-existent platform #"
             << platform
             << ", platform count is "
             << devicesPerPlatform.size()
             << endl;
        exit(EXIT_FAILURE);
    }

    cout << "Platform #" << platform << ":" << endl;

    listDevices(platform, verbose);
}

void CUDABackend::queryDevice(size_t platform, int dev, bool verbose)
{
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
             << devicesPerPlatform[platform]
             << endl;
        exit(EXIT_FAILURE);
    }

    size_t idx = static_cast<size_t>(dev);
    cout << "    Device Number: " << dev << endl;
    cout << "\tDevice Name: " << props[idx].name << endl;
    cout << "\tCompute Capability: "
         << props[idx].major << "." << props[idx].minor << endl;

    cout << endl;

    if (!verbose) return;

    const char *alt = props[idx].concurrentKernels ? "Yes" : "No";
    cout << "\tConcurrent Kernels: " << alt << endl;

    if (props[idx].asyncEngineCount == 0) {
        cout << "\tConcurrent Copying: No" << endl;
    } else if (props[idx].asyncEngineCount == 1) {
        cout << "\tConcurrent Copying: Half-duplex copy & execution" << endl;
    } else if (props[idx].asyncEngineCount == 2) {
        cout << "\tConcurrent Copying: Full-duplex copy & execution" << endl;
    } else {
        cout << "\tConcurrent Copying: Unknown value" << endl;
    }

    alt = props[idx].streamPrioritiesSupported ? "Yes" : "No";
    cout << "\tStream Priorities: " << alt << endl;
    alt = props[idx].canMapHostMemory ? "Yes" : "No";
    cout << "\tCan Map Host Memory: " << alt << endl;
#if CUDART_VERSION >= 6000
    alt = props[idx].managedMemory ? "Yes" : "No";
    cout << "\tManaged Memory: " << alt << endl;
#endif

    cout << endl;

    cout << "\tWarp Size: " << props[idx].warpSize << endl;
    cout << "\tSMP Count: " << props[idx].multiProcessorCount << endl;
    cout << "\tThreads Per Block: " << props[idx].maxThreadsPerBlock << endl;
    cout << "\tThreads Per SMP: " << props[idx].maxThreadsPerMultiProcessor
         << endl;

    cout << endl;

    cout << "\tMax Block Dimensions: "
         << props[idx].maxThreadsDim[0]
         << " x "
         << props[idx].maxThreadsDim[1]
         << " x "
         << props[idx].maxThreadsDim[2]
         << endl;

    cout << "\tMax Grid Dimensions: "
         << props[idx].maxGridSize[0]
         << " x "
         << props[idx].maxGridSize[1]
         << " x "
         << props[idx].maxGridSize[2]
         << endl;

    cout << endl;

    cout << "\tGlobal Mem: " << props[idx].totalGlobalMem << endl;
    cout << "\tConstant Mem: " << props[idx].totalConstMem << endl;

    cout << endl;

    cout << "\tShared Mem Per Block: " << props[idx].sharedMemPerBlock << endl;
    cout << "\tRegisters Per Block: " << props[idx].regsPerBlock << endl;

    cout << endl;

#if CUDART_VERSION >= 6000
    cout << "\tShared Mem Per SMP: "
         << props[idx].sharedMemPerMultiprocessor
         << endl;

    cout << "\tRegisters Mem Per SMP: "
         << props[idx].regsPerMultiprocessor
         << endl;

    cout << endl;
#endif
}

void CUDABackend::setDevice(size_t platform, int device)
{
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

    CUDA_CHK(cudaSetDevice(device));
    prop = props[static_cast<size_t>(device)];

    numComputeUnits_ = static_cast<size_t>(prop.multiProcessorCount);
    maxThreadsPerBlock_ = static_cast<size_t>(prop.maxThreadsPerBlock);
    maxSharedMem_ = prop.sharedMemPerBlock;
    maxBlockSizes_ = { static_cast<size_t>(prop.maxThreadsDim[0])
                    , static_cast<size_t>(prop.maxThreadsDim[1])
                    , static_cast<size_t>(prop.maxThreadsDim[2])
    };

    maxGridSizes_ = { static_cast<size_t>(prop.maxGridSize[0])
                   , static_cast<size_t>(prop.maxGridSize[1])
                   , static_cast<size_t>(prop.maxGridSize[2])
    };
}

void CUDABackend::setWorkSizes
( size_t dims
, std::vector<size_t> blockSizes
, std::vector<size_t> gridSizes
, size_t sharedMem)
{
    if (dims < 1 || dims > 3) {
        cerr << "Invalid number of dimensions: " << dims << endl;
        exit(EXIT_FAILURE);
    }

    if (sharedMem > maxSharedMem) {
        cerr << "Insufficient shared memory, " << sharedMem << " request, "
             << maxSharedMem << " available." << endl;
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

    block.x = static_cast<unsigned int>(blockSizes[0]);
    block.y = dims >= 2 ? static_cast<unsigned int>(blockSizes[1]) : 1;
    block.z = dims >= 3 ? static_cast<unsigned int>(blockSizes[2]) : 1;

    grid.x = static_cast<unsigned int>(gridSizes[0]);
    grid.y = dims >= 2 ? static_cast<unsigned int>(gridSizes[1]) : 1;
    grid.z = dims >= 3 ? static_cast<unsigned int>(gridSizes[2]) : 1;

    sharedMemSize = sharedMem;
}
