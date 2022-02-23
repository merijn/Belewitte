#include "Backend.hpp"

#define CEIL_DIV(x, y)          (((x) + ((y) - 1)) / (y))

Backend::base_alloc_t::~base_alloc_t()
{}

void
Backend::base_alloc_t::copyHostToDev()
{ copyHostToDevImpl(); }

void
Backend::base_alloc_t::copyDevToHost()
{
    copyDevToHostImpl();
    if (associatedPtr) *associatedPtr = hostPtr.get();
}

void
Backend::base_alloc_t::free()
{
    freeImpl();
    hostPtr.reset();
    byteSize = 0;
    read_only = true;
    associatedPtr = nullptr;
}

Backend::~Backend()
{}

size_t Backend::platformCount()
{ return devicesPerPlatform.size(); }

int Backend::deviceCount(size_t platform)
{ return devicesPerPlatform[platform]; }

void Backend::listPlatforms(bool verbose)
{
    for (size_t i = 0; i < devicesPerPlatform.size(); i++) {
        queryPlatform(i, verbose);
    }
}

void Backend::listDevices(size_t platform, bool verbose)
{
    for (int i = 0; i < devicesPerPlatform[platform]; i++) {
        queryDevice(platform, i, verbose);
    }
}

std::pair<size_t,size_t> Backend::computeDivision(size_t count)
{
    size_t i;
    std::pair<size_t,size_t> result;

    result.first = CEIL_DIV(count, numComputeUnits);

    for (i = 1; result.first > maxThreadsPerBlock; i++) {
        result.first = CEIL_DIV(count, i * numComputeUnits);
    }

    size_t adjust = result.first % 32;
    result.first += adjust ? 32 - adjust : 0;

    result.second = std::min(i * numComputeUnits,
                                CEIL_DIV(count, result.first));

    return result;
}
