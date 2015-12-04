//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include <utility>
#include <functional>

namespace Enhedron { namespace Util { namespace Impl { namespace Impl_Lazy {
    using std::move;
    using std::function;

    template<typename Value>
    class Lazy {
        optional<Value> value_;
        function<Value ()> eval_;
    public:
        template<typename Functor>
        Lazy(Functor eval) : eval_(move(eval)) {}

        const Value& get() {
            if ( ! value_) value_ = eval_();
            return *value_;
        }

        const Value& operator*() { return get(); }

        const Value* operator->() { return &(get()); }
    };
}}}}

namespace Enhedron { namespace Util {
    using Impl::Impl_Lazy::Lazy;
}}