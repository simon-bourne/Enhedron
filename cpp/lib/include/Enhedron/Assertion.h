//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Assertion/Configurable.h"
#include "Enhedron/Util/Optional.h"

#include <iostream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace Enhedron
{
namespace Impl
{
namespace Impl_Assertion
{
using ::Enhedron::Util::optional;

using std::forward;
using std::terminate;
using std::vector;
using std::string;
using std::cerr;
using std::runtime_error;

using namespace ::Enhedron::Assertion;

struct CerrFailureHandler final : FailureHandler
{
    virtual ~CerrFailureHandler() override {}
    virtual bool notifyPassing() const override { return false; }
    virtual void
    pass(optional<string>, const string&, const vector<Variable>&) override
    {
    }

    virtual void fail(
        optional<string> description,
        const string& expressionText,
        const vector<Variable>& variableList) override
    {
        cerr << "Assert failed: ";

        if (description) {
            cerr << *description << " (" << expressionText << ")\n";
        }
        else
        {
            cerr << expressionText << "\n";
        }

        for (const auto& variable : variableList) {
            cerr << "    " << variable.name() << " = " << variable.value()
                 << ": in file " << variable.file() << ", line "
                 << variable.line() << "\n";
        }

        cerr.flush();

#ifndef NDEBUG
        terminate();
#endif
    }
};

inline Out<CerrFailureHandler> failureHandler()
{
    static CerrFailureHandler instance;
    return out(instance);
}

template <typename... Args>
void Assert(Args&&... args)
{
    CheckWithFailureHandler(failureHandler(), forward<Args>(args)...);
}

template <typename Exception, typename... Args>
void AssertThrows(Args&&... args)
{
    CheckThrowsWithFailureHandler<Exception>(
        failureHandler(), forward<Args>(args)...);
}

// Always throws an exception or aborts
template <typename... Args>
[[noreturn]] void AssertUnreachable(Args&&... args)
{
    processFailure(
        failureHandler(),
        string("unreachableViolation"),
        forward<Args>(args)...);

    throw runtime_error("unreachableViolation");
}
}
}
}

namespace Enhedron
{
using Impl::Impl_Assertion::Assert;
using Impl::Impl_Assertion::AssertThrows;
using Impl::Impl_Assertion::AssertUnreachable;
}
