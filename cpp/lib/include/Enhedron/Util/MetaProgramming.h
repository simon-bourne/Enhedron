//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"

#include <functional>
#include <tuple>
#include <type_traits>
#include <utility>

namespace Enhedron
{
namespace Util
{
namespace Impl
{
namespace Impl_MetaProgramming
{
template <size_t argCount>
struct TailArgPlaceHolder
{
    static TailArgPlaceHolder instance;
};

template <size_t argCount>
TailArgPlaceHolder<argCount> TailArgPlaceHolder<argCount>::instance;
}
}
}
}

namespace std
{
template <size_t argCount>
struct is_placeholder<
    ::Enhedron::Util::Impl::Impl_MetaProgramming::TailArgPlaceHolder<argCount>>
    : integral_constant<size_t, argCount>
{
};
}

namespace Enhedron
{
namespace Util
{
namespace Impl
{
namespace Impl_MetaProgramming
{
using std::tuple;
using std::index_sequence;
using std::index_sequence_for;
using std::forward;
using std::get;
using std::remove_reference_t;
using std::remove_extent_t;
using std::conditional_t;
using std::is_array;
using std::is_function;
using std::add_pointer_t;
using std::enable_if_t;

template <typename BoundFunctor, class Value, size_t... indices>
auto bindFirst(BoundFunctor&& f, Value value, index_sequence<indices...>)
{
    return bind(
        forward<BoundFunctor>(f),
        value,
        TailArgPlaceHolder<indices + 1>::instance...);
}

template <typename Functor, size_t... indices, typename... Args>
auto extractParameterPack(
    Functor&& f,
    index_sequence<indices...>,
    const tuple<Args...>& args)
{
    return f(get<indices>(args)...);
}

template <typename Functor, size_t... indices, typename... Args>
auto extractParameterPack(
    Functor&& f,
    index_sequence<indices...>,
    tuple<Args...>&& args)
{
    return f(get<indices>(forward<tuple<Args...>>(args))...);
}

template <typename Functor, typename... Args>
auto extractParameterPack(Functor&& f, const tuple<Args...>& args)
{
    return extractParameterPack(
        forward<Functor>(f), index_sequence_for<Args...>(), args);
}

template <typename Functor, typename... Args>
auto extractParameterPack(Functor&& f, tuple<Args...>&& args)
{
    return extractParameterPack(
        forward<Functor>(f),
        index_sequence_for<Args...>(),
        forward<tuple<Args...>>(args));
}

template <typename Functor>
void mapParameterPack(Functor&&)
{
}

template <typename Functor, typename Head, typename... Args>
void mapParameterPack(Functor&& f, const Head& head, const Args&... args)
{
    f(head);
    mapParameterPack(forward<Functor>(f), args...);
}

template <typename T>
class DecayArrayAndFunction
{
public:
    using U = remove_reference_t<T>;
    using type = conditional_t<
        is_array<U>::value,
        remove_extent_t<U>*,
        conditional_t<is_function<U>::value, add_pointer_t<U>, T>>;
};

template <typename T>
using DecayArrayAndFunction_t = typename DecayArrayAndFunction<T>::type;

template <typename... Args>
class StoreArgs final : public NoCopy
{
    using TupleType = tuple<remove_reference_t<Args>...>;
    TupleType args;

    template <typename Functor, size_t... indices, typename... ExtraArgs>
    auto applyExtraBeforeImpl(
        Functor&& functor,
        index_sequence<indices...>,
        ExtraArgs&&... extraArgs)
    {
        return functor(
            forward<ExtraArgs>(extraArgs)...,
            forward<Args>(get<indices>(args))...);
    }

    template <typename Functor, size_t... indices, typename... ExtraArgs>
    auto applyExtraAfterImpl(
        Functor&& functor,
        index_sequence<indices...>,
        ExtraArgs&&... extraArgs)
    {
        return functor(
            forward<Args>(get<indices>(args))...,
            forward<ExtraArgs>(extraArgs)...);
    }

public:
    // RValue reference arguments will be moved into this container. Everything
    // else will be copied.
    StoreArgs(Args&&... args) : args(forward<Args>(args)...) {}
    template <typename Functor>
    auto apply(Functor&& functor)
    {
        return extractParameterPack(forward<Functor>(functor), args);
    }

    template <typename Functor, typename... ExtraArgs>
    auto applyExtraBefore(Functor&& functor, ExtraArgs&&... extraArgs)
    {
        return applyExtraBeforeImpl(
            forward<Functor>(functor),
            index_sequence_for<Args...>(),
            forward<ExtraArgs>(extraArgs)...);
    }

    template <typename Functor, typename... ExtraArgs>
    auto applyExtraAfter(Functor&& functor, ExtraArgs&&... extraArgs)
    {
        return applyExtraAfterImpl(
            forward<Functor>(functor),
            index_sequence_for<Args...>(),
            forward<ExtraArgs>(extraArgs)...);
    }
};
}
}
}
}

namespace Enhedron
{
namespace Util
{
using Impl::Impl_MetaProgramming::StoreArgs;
using Impl::Impl_MetaProgramming::DecayArrayAndFunction;
using Impl::Impl_MetaProgramming::DecayArrayAndFunction_t;
using Impl::Impl_MetaProgramming::bindFirst;
using Impl::Impl_MetaProgramming::mapParameterPack;
using Impl::Impl_MetaProgramming::extractParameterPack;
}
}
