//------------------------------------------------------------------------------
// BumpAllocator.h
// Fast allocator based on pointer bumping.
//
// File is under the MIT license; see LICENSE for details.
//------------------------------------------------------------------------------
#pragma once

#include "util/Util.h"

namespace slang {

/// BumpAllocator - Fast O(1) allocator.
///
/// Allocates items sequentially in memory, with underlying memory allocated in
/// blocks of a configurable size. Individual items cannot be deallocated;
/// the entire thing must be destroyed to release the memory.
class BumpAllocator {
public:
    BumpAllocator();
    ~BumpAllocator();

    BumpAllocator(BumpAllocator&& other) noexcept;
    BumpAllocator& operator=(BumpAllocator&& other) noexcept;

    BumpAllocator(const BumpAllocator&) = delete;
    BumpAllocator& operator=(const BumpAllocator&) = delete;

    /// Construct a new item using the allocator.
    template<typename T, typename... Args>
    T* emplace(Args&&... args) {
        static_assert(std::is_trivially_destructible_v<T>);
        return new (allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
    }

    /// Allocate @a size bytes of memory with the given @a alignment.
    byte* allocate(size_t size, size_t alignment) {
        byte* base = alignPtr(head->current, alignment);
        byte* next = base + size;
        if (next > endPtr)
            return allocateSlow(size, alignment);

        head->current = next;
        return base;
    }

protected:
    // Allocations are tracked as a linked list of segments.
    struct Segment {
        Segment* prev;
        byte* current;
    };

    Segment* head;
    byte* endPtr;

    enum {
        INITIAL_SIZE = 512,
        SEGMENT_SIZE = 4096
    };

    // Slow path handling of allocation.
    byte* allocateSlow(size_t size, size_t alignment);

    static byte* alignPtr(byte* ptr, size_t alignment) {
        return reinterpret_cast<byte*>(
            (reinterpret_cast<uintptr_t>(ptr) + alignment - 1) & ~(alignment - 1));
    }

    static Segment* allocSegment(Segment* prev, size_t size);
};

template<typename T>
class TypedBumpAllocator : public BumpAllocator {
public:
    TypedBumpAllocator() = default;
    TypedBumpAllocator(TypedBumpAllocator&& other) noexcept : BumpAllocator(std::move(other)) {}
    ~TypedBumpAllocator() {
        Segment* seg = head;
        while (seg) {
            for (T* cur = (T*)(seg + 1); cur != (T*)seg->current; cur++)
                cur->~T();
            seg = seg->prev;
        }
    }

    /// Construct a new item using the allocator.
    template<typename... Args>
    T* emplace(Args&&... args) {
        return new (allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
    }
};

}
