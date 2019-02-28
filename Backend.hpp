#ifndef BACKEND_HPP
#define BACKEND_HPP

#include <cstddef>
#include <vector>
#include "utils/Util.hpp"

class Backend {
  protected:
    class base_alloc_t
    {
      protected:
        std::shared_ptr<void> hostPtr;
        size_t byteSize;
        bool read_only;
        void **associatedPtr;

        base_alloc_t() {}

        base_alloc_t(std::shared_ptr<void> p, size_t N, bool ro, void** assoc)
         : hostPtr(p), byteSize(N), read_only(ro), associatedPtr(assoc)
        {}

        base_alloc_t(std::shared_ptr<void> p, size_t N, bool readonly)
         : base_alloc_t(p, N, readonly, nullptr)
        {}

        base_alloc_t(const base_alloc_t& o)
         : hostPtr(o.hostPtr), byteSize(o.byteSize), read_only(o.read_only)
         , associatedPtr(o.associatedPtr)
        {}

        virtual void copyHostToDevImpl() = 0;
        virtual void copyDevToHostImpl() = 0;
        virtual void freeImpl() = 0;

      public:
        base_alloc_t(base_alloc_t&& o)
         : hostPtr(std::move(o.hostPtr)), byteSize(o.byteSize)
         , read_only(o.read_only)
         , associatedPtr(o.associatedPtr)
        {
            o.byteSize = 0;
            o.associatedPtr = nullptr;
        }

        operator bool() const { return bool(hostPtr); }

        virtual ~base_alloc_t();
        void copyHostToDev();
        void copyDevToHost();
        void free();

        base_alloc_t& operator=(base_alloc_t&& other)
        {
            hostPtr = std::move(other.hostPtr);

            byteSize = other.byteSize;
            other.byteSize = 0;

            read_only = other.read_only;

            associatedPtr = other.associatedPtr;
            other.associatedPtr = nullptr;

            return *this;
        }
    };

    template<typename T, typename Base>
    class typed_alloc_t : public Base
    {
        template<bool is_const>
        class Iterator {
          public:
            typedef typename std::conditional
                <is_const, const typed_alloc_t, typed_alloc_t>::type alloc_t;
            typedef ptrdiff_t difference_type;
            typedef typename std::conditional
                <is_const, const T, T>::type value_type;
            typedef value_type& reference;
            typedef value_type* pointer;
            typedef size_t size_type;
            typedef std::random_access_iterator_tag iterator_category;

          private:
            alloc_t& alloc;
            size_type offset;

          public:
            Iterator(alloc_t& a, size_type i)
              : alloc(a), offset(i)
              {}

            Iterator(const Iterator& it)
              : alloc(it.alloc), offset(it.offset)
              {}

            ~Iterator() {}

            Iterator& operator=(const Iterator& it)
            {
                checkError(alloc == it.alloc,
                        "Can only assign iterators for same data!");
                offset = it.offset;
                return *this;
            }

            Iterator& operator++()
            { ++offset; return *this; }

            Iterator operator++(int)
            {
                Iterator tmp(*this);
                ++*this;
                return tmp;
            }

            Iterator& operator--()
            { --offset; return *this; }

            Iterator operator--(int)
            {
                Iterator tmp(*this);
                --*this;
                return tmp;
            }

            Iterator& operator+=(size_type i)
            { offset += i; return *this; }

            Iterator operator+(size_type i) const
            {
                Iterator tmp(*this);
                tmp += i;
                return tmp;
            }

            Iterator& operator-=(size_type i)
            { offset -= i; return *this; }

            Iterator operator-(size_type i) const
            {
                Iterator tmp(*this);
                tmp -= i;
                return tmp;
            }

            difference_type operator-(const Iterator& it) const
            {
                difference_type diff = static_cast<difference_type>(offset);
                diff -= static_cast<difference_type>(it.offset);
                return diff;
            }

            reference operator*() const
            { return alloc[offset]; }

            pointer operator->() const;
            reference operator[](size_type i) const
            { return alloc[offset + i]; }

            bool operator==(const Iterator& it) const
            { return alloc == it.alloc && offset == it.offset; }

            bool operator!=(const Iterator& it) const
            { return !operator==(it); }

            bool operator<(const Iterator& it) const
            {
                checkError(alloc == it.alloc,
                        "Iterators from different allocations!");
                return offset < it.offset;
            }

            bool operator<=(const Iterator& it) const
            { return operator<(it) || operator==(it); }

            bool operator>(const Iterator& it) const
            { return !operator<=(it); }

            bool operator>=(const Iterator& it) const
            { return !operator<(it); }
        };

      protected:
        using Base::hostPtr;
        size_t max;

      public:
        const size_t& size;

        typed_alloc_t() : max(0), size(max)
        {}

        typed_alloc_t(size_t N, size_t sz, bool readonly)
         : Base(sz, readonly), max(N), size(max)
        {}

        typed_alloc_t(typed_alloc_t&& o)
         : Base(std::move(o)), max(o.max), size(max)
        { o.max = 0; }

        typed_alloc_t& operator=(typed_alloc_t&& other)
        {
            Base::operator=(std::move(other));
            max = other.max;
            other.max = 0;
            return *this;
        }

        T& operator*()
        {
            auto p = const_cast<const typed_alloc_t*>(this);
            return const_cast<T&>(p->operator*());
        }

        const T& operator*() const
        { return hostPtr; }

        T* operator->()
        {
            auto p = const_cast<const typed_alloc_t*>(this);
            return const_cast<T*>(p->operator->());
        }

        const T* operator->() const
        { return static_cast<T*>(hostPtr.get()); }

        T& operator[](size_t i)
        {
            auto p = const_cast<const typed_alloc_t*>(this);
            return const_cast<T&>(p->operator[](i));
        }

        const T& operator[](size_t i) const
        {
            checkError(i < max, "Attempting to index invalid offset! Index: ",
                       i, " Max: ", max);
            return static_cast<T*>(hostPtr.get())[i];
        }

        typedef Iterator<false> iterator;
        typedef Iterator<true> const_iterator;
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

        iterator begin() { return iterator(*this, 0); }
        const_iterator begin() const { return const_iterator(*this, 0); }
        const_iterator cbegin() const { return const_iterator(*this, 0); }

        iterator end() { return iterator(*this, size); }
        const_iterator end() const { return const_iterator(*this, size); }
        const_iterator cend() const { return const_iterator(*this, size); }

        reverse_iterator rbegin() { return reverse_iterator(end()); }
        const_reverse_iterator rbegin() const
        { return const_reverse_iterator(end()); }
        const_reverse_iterator crbegin() const
        { return const_reverse_iterator(end()); }

        reverse_iterator rend() { return reverse_iterator(begin()); }
        const_reverse_iterator rend() const
        { return const_reverse_iterator(begin()); }
        const_reverse_iterator crend() const
        { return const_reverse_iterator(begin()); }
    };

    Backend()
     : numComputeUnits_(0)
     , maxThreadsPerBlock_(0)
     , maxDims_(0)
     , maxSharedMem_(0)
     , maxBlockSizes_(0)
     , maxGridSizes_(0)
     , initialised_(false)
     , devicesPerPlatform(devicesPerPlatform_)
     , numComputeUnits(numComputeUnits_)
     , maxThreadsPerBlock(maxThreadsPerBlock_)
     , maxDims(maxDims_)
     , maxSharedMem(maxSharedMem_)
     , maxBlockSizes(maxBlockSizes_)
     , maxGridSizes(maxGridSizes_)
     , initialised(initialised_)
    {}

  public:
    virtual ~Backend();
    size_t platformCount();
    int deviceCount(size_t platform);
    void listPlatforms(bool verbose);
    void listDevices(size_t platform, bool verbose);
    std::pair<size_t,size_t> computeDivision(size_t count);

    virtual void queryPlatform(size_t platform, bool verbose) = 0;
    virtual void queryDevice(size_t platform, int device, bool verbose) = 0;
    virtual void setDevice(size_t platform, int device) = 0;

    virtual void setWorkSizes(size_t dim, std::vector<size_t> blockSizes,
                                std::vector<size_t> gridSizes,
                                size_t sharedMem = 0) = 0;

  protected:
    std::vector<int> devicesPerPlatform_;
    size_t numComputeUnits_;
    size_t maxThreadsPerBlock_;
    size_t maxDims_;
    size_t maxSharedMem_;
    std::vector<size_t> maxBlockSizes_;
    std::vector<size_t> maxGridSizes_;
    bool initialised_;

  public:
    const std::vector<int> &devicesPerPlatform;
    const size_t &numComputeUnits;
    const size_t &maxThreadsPerBlock;
    const size_t &maxDims;
    const size_t &maxSharedMem;
    const std::vector<size_t> &maxBlockSizes;
    const std::vector<size_t> &maxGridSizes;
    const bool &initialised;
};

template<typename>
struct isBackendAllocTrait : public std::false_type
{};

template<typename T>
constexpr bool isDeviceAlloc()
{ return isBackendAllocTrait<typename std::remove_reference<T>::type>::value; }
#endif
