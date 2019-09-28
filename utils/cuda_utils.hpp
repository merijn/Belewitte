template<typename T>
static __device__ inline void
memcpy_SIMD(size_t warp_size, size_t warp_offset, int cnt, T *dest, T *src)
{
    for (int i = warp_offset; i < cnt; i += warp_size) {
        dest[i] = src[i];
    }
    __threadfence_block();
}
