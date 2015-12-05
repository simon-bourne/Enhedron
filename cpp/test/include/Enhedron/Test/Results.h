//
//          Copyright Simon Bourne 2015.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
//

#pragma once

#include "Enhedron/Util.h"
#include "Enhedron/Util/Optional.h"

#include <memory>
#include <string>
#include <stdexcept>
#include <vector>
#include <algorithm>

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;
    using std::vector;
    using std::move;
    using std::max;

    using Assertion::FailureHandler;
    using Assertion::Variable;
    using Util::optional;

    class Stats {
        uint64_t fixtures_ = 0;
        uint64_t tests_ = 0;
        uint64_t checks_ = 0;
        uint64_t failedTests_ = 0;
        uint64_t failedChecks_ = 0;
    public:
        Stats& operator+=(Stats rhs) {
            fixtures_ += rhs.fixtures_;
            tests_ += rhs.tests_;
            checks_ += rhs.checks_;
            failedTests_ += rhs.failedTests_;
            failedChecks_ += rhs.failedChecks_;
            return *this;
        }

        void addFixture() { ++fixtures_; }
        void addTest() { ++tests_; }
        void addCheck() { ++checks_; }

        void failTest() { ++failedTests_; }
        void failCheck() { ++failedChecks_; }

        uint64_t fixtures() const { return fixtures_; }
        uint64_t tests() const { return tests_; }
        uint64_t checks() const { return checks_; }
        uint64_t failedTests() const { return failedTests_; }
        uint64_t failedChecks() const { return failedChecks_; }
    };

    // Wrapper, as we may add more functionality here. Such as tracking how much of the stack has
    // been seen before.
    class NameStack final: public NoCopy {
        vector<string> stack_;
    public:
        const vector<string>& stack() const { return stack_; }

        void push(string name) {
            stack_.emplace_back(move(name));
        }

        void pop() {
            Assert( ! VAR(stack_.empty()));
            stack_.pop_back();
        }
    };

    struct Results: public FailureHandler {
        virtual ~Results() {}

        virtual void finish(const Stats& stats) = 0;

        virtual void beginContext(const NameStack& contextStack, const string& name) = 0;
        virtual void endContext(const Stats& stats, const NameStack& contextStack, const string& name) = 0;

        virtual void beginGiven(const NameStack& context, const string& given) = 0;
        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) = 0;

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) = 0;
        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) = 0;

        virtual void failByException(const exception& e) = 0;

    };

    enum class Verbosity {
        SILENT,
        SUMMARY,
        CONTEXTS,
        FIXTURES,
        SECTIONS,
        EXHAUSTIVE_SECTIONS,
        CHECKS,
        CHECKS_EXPRESSION,
        VARIABLES
    };

    class HumanResults final: public Results {
        Out<ostream> output_;
        Verbosity verbosity_;
        bool contextWritten_ = false;
        size_t whenDepth_ = 0;

        void writeContext(const NameStack& contextStack) {
            if (verbosity_ >= Verbosity::CONTEXTS && ! contextWritten_) {
                if ( ! contextStack.stack().empty()) {
                    *output_ << contextStack.stack().front();

                    for (
                            auto contextIter = contextStack.stack().begin() + 1;
                            contextIter != contextStack.stack().end();
                            ++contextIter
                        )
                    {
                        *output_ << "/" << *contextIter;
                    }

                    *output_ << "\n";
                }

                contextWritten_ = true;
            }
        }

        void printVariables(const vector <Variable> &variableList) {
            for (const auto& variable : variableList) {
                indent(whenDepth_ + 1);
                (*output_) << variable.name() << " = " << variable.value()
                << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }

        void indent(size_t indent) {
            while (indent > 0) {
                *output_ << "    ";
                --indent;
            }
        }
    public:
        HumanResults(Out<ostream> output, Verbosity verbosity) :
            output_(output), verbosity_(verbosity)
        {}

        virtual void finish(const Stats& stats) override {}

        virtual void beginContext(const NameStack& contextStack, const string& name) override {
            contextWritten_ = false;
        }

        virtual void endContext(const Stats& stats, const NameStack& context, const string& name) override {
            contextWritten_ = false;
        }

        virtual void beginGiven(const NameStack& context, const string& given) override {
            if (verbosity_ >= Verbosity::CONTEXTS) {
                writeContext(context);
            }

            if (verbosity_ >= Verbosity::FIXTURES) {
                indent(1);
                *output_ << "Given: " << given << "\n";
            }
        }

        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) override { }

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) override {
            ++whenDepth_;

            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(whenDepth_);
                *output_ << " When: " << when << "\n";
            }
        }

        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) override {
            --whenDepth_;

            if (whenDepth_ == 0 && verbosity_ >= Verbosity::CHECKS) {
                *output_ << "\n";
            }
        }

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            indent(whenDepth_);
            (*output_) << "CHECK FAILED: " << expressionText << "\n";
            printVariables(variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            static constexpr const size_t minDepth = 1;
            indent(max(minDepth, whenDepth_));
            (*output_) << " Then: ";

            if (description) {
                (*output_) << *description;
            }

            if (verbosity_ >= Verbosity::CHECKS_EXPRESSION || ! description) {
                (*output_) << expressionText;
            }

            (*output_) << "\n";

            if (verbosity_ >= Verbosity::VARIABLES) {
                printVariables(variableList);
            }
        }

        virtual void failByException(const exception& e) override {
            indent(whenDepth_);
            (*output_) << "TEST FAILED WITH EXCEPTION: " << e.what() << endl;
        }
    };
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::NameStack;
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::HumanResults;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}
