//------------------------------------------------------------------------------
//! @file ConstantValue.h
//! @brief Compile-time constant representation
//
// SPDX-FileCopyrightText: Michael Popoloski
// SPDX-License-Identifier: MIT
//------------------------------------------------------------------------------
#pragma once

#include <deque>
#include <map>
#include <string>
#include <variant>
#include <vector>

#include "slang/numeric/SVInt.h"
#include "slang/util/CopyPtr.h"
#include "slang/util/Iterator.h"

namespace slang {

struct AssociativeArray;
struct SVQueue;
struct SVUnion;

/// Represents an IEEE754 double precision floating point number.
/// This is a separate type from `double` to make it less likely that
/// an implicit C++ conversion will mess us up somewhere.
struct real_t {
    double v;

    real_t() : v(0.0) {}
    real_t(double v) : v(v) {}
    operator double() const { return v; }
};

/// Represents an IEEE754 single precision floating point number.
/// This is a separate type from `double` to make it less likely that
/// an implicit C++ conversion will mess us up somewhere.
struct shortreal_t {
    float v;

    shortreal_t() : v(0.0) {}
    shortreal_t(float v) : v(v) {}
    operator float() const { return v; }
};

/// Represents a constant (compile-time evaluated) value, of one of a few possible types.
/// By default the value is indeterminate, or "bad". Expressions involving bad
/// values result in bad values, as you might expect.
///
class ConstantValue {
public:
    /// This type represents the null value (class handles, etc) in expressions.
    struct NullPlaceholder : std::monostate {};

    /// This type represents the unbounded value ($) in expressions.
    struct UnboundedPlaceholder : std::monostate {};

    using Elements = std::vector<ConstantValue>;
    using Map = CopyPtr<AssociativeArray>;
    using Queue = CopyPtr<SVQueue>;
    using Union = CopyPtr<SVUnion>;

    using Variant = std::variant<std::monostate, SVInt, real_t, shortreal_t, NullPlaceholder,
                                 Elements, std::string, Map, Queue, Union, UnboundedPlaceholder>;

    ConstantValue() = default;
    ConstantValue(nullptr_t) {}

    ConstantValue(const SVInt& integer) : value(integer) {}
    ConstantValue(SVInt&& integer) : value(std::move(integer)) {}
    ConstantValue(real_t real) : value(real) {}
    ConstantValue(shortreal_t real) : value(real) {}

    ConstantValue(NullPlaceholder nul) : value(nul) {}
    ConstantValue(UnboundedPlaceholder unbounded) : value(unbounded) {}
    ConstantValue(const Elements& elements) : value(elements) {}
    ConstantValue(Elements&& elements) : value(std::move(elements)) {}
    ConstantValue(const std::string& str) : value(str) {}
    ConstantValue(std::string&& str) : value(std::move(str)) {}

    ConstantValue(const Map& map) : value(map) {}
    ConstantValue(Map&& map) : value(std::move(map)) {}
    ConstantValue(const AssociativeArray& aa) : value(Map(aa)) {}
    ConstantValue(AssociativeArray&& aa) : value(Map(std::move(aa))) {}

    ConstantValue(const Queue& queue) : value(queue) {}
    ConstantValue(Queue&& queue) : value(std::move(queue)) {}
    ConstantValue(const SVQueue& queue) : value(Queue(queue)) {}
    ConstantValue(SVQueue&& queue) : value(Queue(std::move(queue))) {}

    ConstantValue(const Union& unionVal) : value(unionVal) {}
    ConstantValue(Union&& unionVal) : value(std::move(unionVal)) {}
    ConstantValue(const SVUnion& unionVal) : value(Union(unionVal)) {}
    ConstantValue(SVUnion&& unionVal) : value(Union(std::move(unionVal))) {}

    bool bad() const { return std::holds_alternative<std::monostate>(value); }
    explicit operator bool() const { return !bad(); }

    bool isInteger() const { return std::holds_alternative<SVInt>(value); }
    bool isReal() const { return std::holds_alternative<real_t>(value); }
    bool isShortReal() const { return std::holds_alternative<shortreal_t>(value); }
    bool isNullHandle() const { return std::holds_alternative<NullPlaceholder>(value); }
    bool isUnbounded() const { return std::holds_alternative<UnboundedPlaceholder>(value); }
    bool isUnpacked() const { return std::holds_alternative<Elements>(value); }
    bool isString() const { return std::holds_alternative<std::string>(value); }
    bool isMap() const { return std::holds_alternative<Map>(value); }
    bool isQueue() const { return std::holds_alternative<Queue>(value); }
    bool isUnion() const { return std::holds_alternative<Union>(value); }

    bool isContainer() const { return isUnpacked() || isQueue() || isMap(); }

    SVInt& integer() & { return std::get<SVInt>(value); }
    const SVInt& integer() const& { return std::get<SVInt>(value); }
    SVInt integer() && { return std::get<SVInt>(std::move(value)); }
    SVInt integer() const&& { return std::get<SVInt>(std::move(value)); }

    real_t real() const { return std::get<real_t>(value); }
    shortreal_t shortReal() const { return std::get<shortreal_t>(value); }

    span<ConstantValue> elements() { return std::get<Elements>(value); }
    span<ConstantValue const> elements() const { return std::get<Elements>(value); }

    std::string& str() & { return std::get<std::string>(value); }
    const std::string& str() const& { return std::get<std::string>(value); }
    std::string str() && { return std::get<std::string>(std::move(value)); }
    std::string str() const&& { return std::get<std::string>(std::move(value)); }

    Map& map() & { return std::get<Map>(value); }
    const Map& map() const& { return std::get<Map>(value); }
    Map map() && { return std::get<Map>(std::move(value)); }
    Map map() const&& { return std::get<Map>(std::move(value)); }

    Queue& queue() & { return std::get<Queue>(value); }
    const Queue& queue() const& { return std::get<Queue>(value); }
    Queue queue() && { return std::get<Queue>(std::move(value)); }
    Queue queue() const&& { return std::get<Queue>(std::move(value)); }

    Union& unionVal() & { return std::get<Union>(value); }
    const Union& unionVal() const& { return std::get<Union>(value); }
    Union unionVal() && { return std::get<Union>(std::move(value)); }
    Union unionVal() const&& { return std::get<Union>(std::move(value)); }

    ConstantValue getSlice(int32_t upper, int32_t lower, const ConstantValue& defaultValue) const;

    Variant& getVariant() { return value; }
    const Variant& getVariant() const { return value; }

    std::string toString() const;
    size_t hash() const;

    [[nodiscard]] bool empty() const;
    size_t size() const;

    ConstantValue& at(size_t index);
    const ConstantValue& at(size_t index) const;

    bool isTrue() const;
    bool isFalse() const;
    bool hasUnknown() const;

    ConstantValue convertToInt() const;
    ConstantValue convertToInt(bitwidth_t width, bool isSigned, bool isFourState) const;
    ConstantValue convertToReal() const;
    ConstantValue convertToShortReal() const;
    ConstantValue convertToStr() const;
    ConstantValue convertToByteArray(bitwidth_t size, bool isSigned) const;
    ConstantValue convertToByteQueue(bool isSigned) const;

    /// Gets the size of this value when converted to a bitstream.
    size_t bitstreamWidth() const;

    static const ConstantValue Invalid;

    friend std::ostream& operator<<(std::ostream& os, const ConstantValue& cv);
    friend bool operator==(const ConstantValue& lhs, const ConstantValue& rhs);
    friend bool operator!=(const ConstantValue& lhs, const ConstantValue& rhs);
    friend bool operator<(const ConstantValue& lhs, const ConstantValue& rhs);

private:
    Variant value;
};

/// Represents a SystemVerilog associative array, for use during constant evaluation.
struct AssociativeArray : public std::map<ConstantValue, ConstantValue> {
    using std::map<ConstantValue, ConstantValue>::map;
    ConstantValue defaultValue;
};

/// Represents a SystemVerilog queue, for use during constant evaluation.
struct SVQueue : public std::deque<ConstantValue> {
    using std::deque<ConstantValue>::deque;
    uint32_t maxBound = 0;

    void resizeToBound() {
        if (maxBound && size() > maxBound + 1)
            resize(maxBound + 1);
    }
};

/// Represents a SystemVerilog unpacked union, for use during constant evaluation.
struct SVUnion {
    ConstantValue value;
    std::optional<uint32_t> activeMember;
};

/// An iterator for child elements in a ConstantValue, if it represents an
/// array, map, or queue.
class CVIterator
    : public iterator_facade<CVIterator, std::bidirectional_iterator_tag, ConstantValue> {
public:
    CVIterator(ConstantValue::Elements::iterator it) : current(it) {}
    CVIterator(AssociativeArray::iterator it) : current(it) {}
    CVIterator(SVQueue::iterator it) : current(it) {}
    CVIterator(const CVIterator& other) : current(other.current) {}

    CVIterator& operator=(const CVIterator& other) {
        current = other.current;
        return *this;
    }

    bool operator==(const CVIterator& other) const { return current == other.current; }

    const ConstantValue& operator*() const;
    ConstantValue& operator*();

    using iterator_facade::operator++;
    using iterator_facade::operator--;
    CVIterator& operator++();
    CVIterator& operator--();

    const ConstantValue& key() const;

private:
    std::variant<ConstantValue::Elements::iterator, AssociativeArray::iterator, SVQueue::iterator>
        current;
};

/// A constant iterator for child elements in a ConstantValue, if it represents an
/// array, map, or queue.
class CVConstIterator : public iterator_facade<CVConstIterator, std::bidirectional_iterator_tag,
                                               const ConstantValue> {
public:
    CVConstIterator(ConstantValue::Elements::const_iterator it) : current(it) {}
    CVConstIterator(AssociativeArray::const_iterator it) : current(it) {}
    CVConstIterator(SVQueue::const_iterator it) : current(it) {}
    CVConstIterator(const CVConstIterator& other) : current(other.current) {}

    CVConstIterator& operator=(const CVConstIterator& other) {
        current = other.current;
        return *this;
    }

    bool operator==(const CVConstIterator& other) const { return current == other.current; }

    const ConstantValue& operator*() const;

    using iterator_facade::operator++;
    using iterator_facade::operator--;
    CVConstIterator& operator++();
    CVConstIterator& operator--();

    const ConstantValue& key() const;

private:
    std::variant<ConstantValue::Elements::const_iterator, AssociativeArray::const_iterator,
                 SVQueue::const_iterator>
        current;
};

CVIterator begin(ConstantValue& cv);
CVIterator end(ConstantValue& cv);
CVConstIterator begin(const ConstantValue& cv);
CVConstIterator end(const ConstantValue& cv);

/// Represents a simple constant range, fully inclusive. SystemVerilog allows negative
/// indices, and for the left side to be less, equal, or greater than the right.
///
/// Note that this class makes no attempt to handle overflow of the underlying integer;
/// SystemVerilog places tighter bounds on possible ranges anyway so it shouldn't be an issue.
///
struct ConstantRange {
    int32_t left = 0;
    int32_t right = 0;

    /// Gets the width of the range, regardless of the order in which
    /// the bounds are specified.
    bitwidth_t width() const {
        int32_t diff = left - right;
        return bitwidth_t(diff < 0 ? -diff : diff) + 1;
    }

    /// Gets the lower bound of the range, regardless of the order in which
    /// the bounds are specified.
    int32_t lower() const { return std::min(left, right); }

    /// Gets the upper bound of the range, regardless of the order in which
    /// the bounds are specified.
    int32_t upper() const { return std::max(left, right); }

    /// "Little endian" bit order is when the msb is >= the lsb.
    bool isLittleEndian() const { return left >= right; }

    /// Reverses the bit ordering of the range.
    [[nodiscard]] ConstantRange reverse() const { return {right, left}; }

    /// Selects a subrange of this range, correctly handling both forms of
    /// bit endianness. This will assert that the given subrange is not wider.
    [[nodiscard]] ConstantRange subrange(ConstantRange select) const;

    /// Translates the given index to be relative to the range.
    /// For example, if the range is [7:2] and you pass in 3, the result will be 1.
    /// If the range is [2:7] and you pass in 3, the result will be 4.
    int32_t translateIndex(int32_t index) const;

    /// Determines whether the given point is within the range.
    bool containsPoint(int32_t index) const;

    /// Determines whether the given range overlaps with this one
    /// (including cases where one is wholly contained in the other).
    bool overlaps(ConstantRange other) const;

    /// Creates a constant range based on a left / right value that is either indexed up
    /// or indexed down. This implements the SystemVerilog range operators of '+:' and '-:'
    static std::optional<ConstantRange> getIndexedRange(int32_t l, int32_t r, bool littleEndian,
                                                        bool indexedUp);

    std::string toString() const;

    bool operator==(const ConstantRange& rhs) const {
        return left == rhs.left && right == rhs.right;
    }

    bool operator!=(const ConstantRange& rhs) const { return !(*this == rhs); }
    friend std::ostream& operator<<(std::ostream& os, const ConstantRange& cr);
};

} // namespace slang

namespace std {

template<>
struct hash<slang::ConstantValue> {
    size_t operator()(const slang::ConstantValue& cv) const { return cv.hash(); }
};

} // namespace std
