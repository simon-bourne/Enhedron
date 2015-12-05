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

namespace Enhedron { namespace Test { namespace Impl { namespace Impl_Results {
    using std::exception;
    using std::string;
    using std::unique_ptr;
    using std::make_unique;
    using std::ostream;
    using std::endl;
    using std::vector;
    using std::move;

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
        size_t depth_ = 0;
        Verbosity verbosity_;
        bool contextWritten_ = false;
    public:
        HumanResults(Out<ostream> output, Verbosity verbosity) :
            output_(output), verbosity_(verbosity)
        {}

        virtual void finish(const Stats& stats) override {}

        virtual void beginContext(const NameStack& contextStack, const string& name) override {
            if (verbosity_ >= Verbosity::CONTEXTS) {
                if ( ! contextStack.stack().empty()) {
                    for (const auto& parentName : contextStack.stack()) {
                        *output_ << parentName << "/";
                    }

                }

                *output_ << name << "\n";
                contextWritten_ = true;
            }
            else {
                contextWritten_ = false;
            }
        }

        virtual void endContext(const Stats& stats, const NameStack& context, const string& name) override {
            // TODO: Write context here.
            contextWritten_ = false;
        }

        virtual void beginGiven(const NameStack& context, const string& given) {}
        virtual void endGiven(const Stats& stats, const NameStack& context, const string& given) {}

        virtual void beginWhen(const NameStack& context,
                               const string& given,
                               const NameStack& whenStack,
                               const string& when) override {}
        virtual void endWhen(const Stats& stats,
                             const NameStack& context,
                             const string& given,
                             const NameStack& whenStack,
                             const string& when) override {}

        virtual void failByException(const exception& e) override {}

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {

        }
    };
/*
    class HumanResultTest final: public ResultTest {
        Out<ostream> outputStream_;
        size_t depth_;
        Verbosity verbosity_;

        void indent(size_t relativeDepth) {
            for (size_t totalDepth = 1 + depth_ + relativeDepth; totalDepth > 0; --totalDepth) {
                (*outputStream_) << "    ";
            }
        }

        void printVariables(const vector <Variable> &variableList) {
            for (const auto& variable : variableList) {
                indent(2);
                (*outputStream_) << variable.name() << " = " << variable.value()
                << ": file \"" << variable.file() << "\", line " << variable.line() << ".\n";
            }
        }
    public:
        HumanResultTest(Out<ostream> outputStream, size_t depth, Verbosity verbosity) :
                outputStream_(outputStream), depth_(depth), verbosity_(verbosity)
        {}

        virtual unique_ptr<ResultTest> section(string description) override {
            if (verbosity_ >= Verbosity::SECTIONS) {
                indent(1);
                (*outputStream_) << "when: " << description << "\n";
            }

            return make_unique<HumanResultTest>(outputStream_, depth_ + 1, verbosity_);
        }

        virtual void beforeFirstFailure() override {}

        virtual bool notifyPassing() const override { return verbosity_ >= Verbosity::CHECKS; }

        virtual void fail(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            indent(1);
            (*outputStream_) << "CHECK FAILED: " << expressionText << "\n";
            printVariables(variableList);
        }

        virtual void pass(optional<string> description, const string &expressionText, const vector <Variable> &variableList) override {
            indent(1);
            (*outputStream_) << "then";

            if (description) {
                (*outputStream_) << ": " << *description;
            }

            if (verbosity_ >= Verbosity::CHECKS_EXPRESSION) {
                (*outputStream_) << ": " << expressionText;
            }

            (*outputStream_) << "\n";

            if (verbosity_ >= Verbosity::VARIABLES) {
                printVariables(variableList);
            }
        }

        virtual void failByException(const exception& e) override {
            indent(0);
            (*outputStream_) << "TEST FAILED WITH EXCEPTION: " << e.what() << endl;
        }

        virtual void finish(const Stats& stats) override {}

    };

    class HumanResultContext final: public ResultContext {
        Out <ostream> outputStream_;
        Verbosity verbosity_;
        string path_;

    public:
        HumanResultContext(Out<ostream> outputStream, Verbosity verbosity, const string& name) :
                outputStream_(outputStream), verbosity_(verbosity), path_(name) {}

        virtual void beforeFirstTestRuns() override {
            if (verbosity_ >= Verbosity::CONTEXTS) {
                *outputStream_ << path_ << "\n";
            }
        }

        virtual void beforeFirstFailure() override { }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            string childPath(path_);

            if ( ! path_.empty()) {
                childPath += "/";
            }

            childPath += name;

            return make_unique<HumanResultContext>(outputStream_, verbosity_, childPath);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            if (verbosity_ >= Verbosity::FIXTURES) {
                *outputStream_ << "    Given: " << name << endl;
            }

            return make_unique<HumanResultTest>(outputStream_, 0, verbosity_);
        }

        virtual void finish(const Stats& stats) override {}
    };

    class HumanResultRootContext final: public ResultContext {
        Out <ostream> outputStream_;
        Verbosity verbosity_;
    public:
        HumanResultRootContext(Out<ostream> outputStream, Verbosity verbosity) :
            outputStream_(outputStream), verbosity_(verbosity) {}

        virtual void beforeFirstTestRuns() override { }

        virtual void beforeFirstFailure() override { }

        virtual unique_ptr<ResultContext> child(const string& name) override {
            return make_unique<HumanResultContext>(outputStream_, verbosity_, name);
        }

        virtual unique_ptr<ResultTest> test(const string& name) override {
            return make_unique<HumanResultTest>(outputStream_, 0, verbosity_);
        }

        virtual void finish(const Stats& stats) override {
            if (verbosity_ >= Verbosity::SUMMARY) {
                if (stats.failedTests() > 0) {
                    *outputStream_ << "FAILED TESTS: " << stats.failedTests() << "\n";
                }

                if (stats.failedChecks() > 0) {
                    *outputStream_ << "FAILED CHECKS: " << stats.failedChecks() << "\n";
                }

                *outputStream_ << "Totals: " <<
                stats.tests() << " tests, " <<
                stats.checks() << " checks, " <<
                stats.fixtures() << " fixtures\n";
            }
        }
    };
    */
}}}}

namespace Enhedron { namespace Test {
    using Impl::Impl_Results::NameStack;
    using Impl::Impl_Results::Results;
    using Impl::Impl_Results::HumanResults;
    using Impl::Impl_Results::Stats;
    using Impl::Impl_Results::Verbosity;
}}
