#include "MosquitoNet.h"

#include <vector>
#include <stdexcept>

using namespace Enhedron::Test;
using std::vector;
using std::runtime_error;

// The comments in this file are used when building the documentation.

// Assertion example 1:
static Suite s("examples", context("assertion",
    given("some constants to assert with", [] (auto& check) {
        int a = 0;
        int b = 1;

        check(VAR(a) == 0 && VAR(b) == 1);
    })
));
// Assertion example 1 end.

void throwRuntimeError() {
    throw runtime_error("Expected exception");
}

static Suite t("examples", context("assertion",
    given("some constants to assert with", [] (auto& check) {
        int a = 0;
        int b = 1;
        int c = 2;
        int d = 4;

        // Assertion example 2:
        check("a is one and b is two", VAR(a) == 0 && VAR(b) == 1);
        // Assertion example 2 end.

        // Assertion example 3:
        check("`c` and `d` are provided for context", VAR(a) == 0 && VAR(b) == 1, VAR(c), VAR(d));
        // Assertion example 3 end.

        // Assertion example 4:
        check("a is one and b is two", VAR(a) == 0 && b == 1 && VAR(a + c) == c);
        // Assertion example 4 end.

        // Assertion example 5:
        check.throws("an exception is thrown", VAR(throwRuntimeError)());
        // Assertion example 5 end.

        // Assertion example 6:
        check. template throws<runtime_error>("a runtime_error is thrown", VAR(throwRuntimeError)());
        // Assertion example 6 end.

        // Assertion example 7:
        check.throws("`a` and `b` are provided for context", VAR(throwRuntimeError)(), VAR(a), VAR(b));
        // Assertion example 7 end.

        // Assertion example 8:
        check.fail(VAR("explicit failure"));
        // Assertion example 8 end.

        // Assertion example 9:
        check.fail(VAR("`a` and `b` are provided for context"), VAR(a), VAR(b));
        // Assertion example 9 end.
    })
));
