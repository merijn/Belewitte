#ifndef CUDA_UTILS_HPP
#define CUDA_UTILS_HPP

#define CUDA_CHK(ans) cudaAssert((ans), __FILE__, __LINE__)
void cudaAssert(const cudaError_t code, const char *file, const int line);

#ifdef __CUDACC__
template<typename T>
static __device__ inline void
memcpy_SIMD(size_t warp_size, size_t warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}
#endif
#endif
