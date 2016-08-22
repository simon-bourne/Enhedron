//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <limits>

namespace Enhedron
{
namespace Impl_Math
{
using std::numeric_limits;

// Assumes numerator + denominator + 1 doesn't overflow.
template <typename NumeratorType, typename DenominatorType>
constexpr NumeratorType
divideRoundingUp(NumeratorType numerator, DenominatorType denominator)
{
    // Doesn't work for -ve numbers.
    static_assert(
        !numeric_limits<NumeratorType>::is_signed,
        "NumeratorType must be unsigned");
    static_assert(
        !numeric_limits<DenominatorType>::is_signed,
        "DenominatorType must be unsigned");

    static_assert(
        numeric_limits<NumeratorType>::is_integer,
        "NumeratorType must be integer");
    static_assert(
        numeric_limits<DenominatorType>::is_integer,
        "DenominatorType must be integer");

    return (numerator + denominator - NumeratorType(1u)) / denominator;
}

template <typename Value, typename Modulus>
constexpr Value makeDivisibleByRoundingDown(Value value, Modulus modulus)
{
    // Doesn't work for -ve numbers.
    static_assert(!numeric_limits<Value>::is_signed, "Value must be unsigned");
    static_assert(
        !numeric_limits<Modulus>::is_signed, "Modulus must be unsigned");

    static_assert(numeric_limits<Value>::is_integer, "Value must be integer");
    static_assert(
        numeric_limits<Modulus>::is_integer, "Modulus must be integer");

    return (value / modulus) * modulus;
}

template <typename Value, typename Modulus>
constexpr Value makeDivisibleByRoundingUp(Value value, Modulus modulus)
{
    // Doesn't work for -ve numbers.
    static_assert(!numeric_limits<Value>::is_signed, "Value must be unsigned");
    static_assert(
        !numeric_limits<Modulus>::is_signed, "Modulus must be unsigned");

    static_assert(numeric_limits<Value>::is_integer, "Value must be integer");
    static_assert(
        numeric_limits<Modulus>::is_integer, "Modulus must be integer");

    return ((value + modulus - 1) / modulus) * modulus;
}

//! Is numerator divisible by denominator
template <typename NumeratorType, typename DenominatorType>
constexpr bool isDivisible(NumeratorType numerator, DenominatorType denominator)
{
    static_assert(
        numeric_limits<NumeratorType>::is_integer,
        "NumeratorType must be integer");
    static_assert(
        numeric_limits<DenominatorType>::is_integer,
        "DenominatorType must be integer");

    return (numerator / denominator) * denominator == numerator;
}
}
}

namespace Enhedron
{
namespace Math
{
using Impl_Math::divideRoundingUp;
using Impl_Math::makeDivisibleByRoundingDown;
using Impl_Math::makeDivisibleByRoundingUp;
using Impl_Math::isDivisible;
}
}
