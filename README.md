# MosquitoNet [![Linux / OS X Build Status](https://img.shields.io/travis/simon-bourne/MosquitoNet/master.svg?label=Linux%20/%20OS%20X%20build)](https://travis-ci.org/simon-bourne/MosquitoNet) [![Windows Build Status](https://img.shields.io/appveyor/ci/simon-bourne/mosquitonet/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/simon-bourne/mosquitonet)

Keep the bugs out on Linux, OS X and Windows.

This is currently under heavy development and the API is subject to change. If it looks interesting to you, but there's
a feature or improvement you'd like, please create an issue.

## Features

- Exhaustively test every combination of parameters for a test. 
- Extensible assertions.
- `VAL` is the only macro you'll need, and that's just adding file and line info to a function. If the name clashes,
`#undef` it and `#define MY_VAL M_ENHEDRON_VAL`.
- Nested contexts.
- Simple or BDD style tests.
- Tests will run through all checks and report all errors so you see everything that failed in a test, not just the
first thing.

## Motivating Examples

We'll start with the coolest feature to hopefully inspire you to continue.
We're testing a function that escapes json strings, and we think that escape codes at the start, middle and end of
the string will be corner cases. We also want to test each individual escape code. This code will exhaustively
test every combination of these arguments.

```C++
static const auto escapedPairs = choice(
        make_pair('\"', '\"'),
        make_pair('\\', '\\'),
        make_pair('\b', 'b'),
        make_pair('\f', 'f'),
        make_pair('\n', 'n'),
        make_pair('\r', 'r'),
        make_pair('\t', 't')
);

static Test::Unit u(context("Util",
    exhaustive(
            escapedPairs,
            choice("", "prefix"),
            choice("", "postfix")
        ).
        simple("jsonEscapingCombo", [] (
                    Check& check,
                    pair<char, char> c,
                    const string& prefix,
                    const string& postfix
                )
            {
                string input(prefix);
                input += c.first;
                input += postfix;

                string result(prefix);
                result += '\\';
                result += c.second;
                result += postfix;

                check(VAL(jsonEscape(input)) == result);
            }
    )
);
```

Some things to note:

- `choice` simply returns a `vector<decltype(firstArgument)>`. We could have used anything that works in a range based
`for` loop. Perhaps a `boost::irange` or a container that generates `N` random numbers, or even a constant.
- `static Test::Unit u(...)` creates a context to which we can add many unit tests or sub-contexts.
- `context("Util", ...)` creates a nested context. There's no limit on the depth of nesting.

Can I customize checks?

Yes. There's not much, if anything, you need to do. For example, if you wrote this function:

```C++
int sum3(int x, int y, int z) {
    return x + y + z;
}
```

You can automatically use it in assertions, and it will print the values of each argument:

```C++
int a = 1;
int b = 2;
int c = 3;
check(VAL(sum3)(a, b, c) == 6); // This will pass.
check(VAL(sum3)(a, b, c) == 7); // This will fail with the message:
    // Test failed: (sum3(1, 2, 3) == 7)
    //    sum3 = function: in file /path/to/file.cpp, line 5
```

For each argument type, you'll need to have an `operator<<` or specialize `Enhedron::Assertion::Convert` to be able to 
print the values.

You can also have more complex expressions and add context expressions to give more detail on the output:

```C++
int a = 1;
int b = 2;
int c = 3;
int contextVariable1 = 10;
const char* contextVariable2 = "Looks like something went wrong!";
check(
    VAL(sum3)(a, b, c) == 7 && VAL(a) == VAL(b),
    VAL(contextVariable1),
    VAL(contextVariable2)
);
```

will fail with the message:
```
Test failed: ((sum3(1, 2, 3) == 7) && (a == b))
    sum3 = function: in file /path/to/file.cpp, line 195
    a = 1: in file /path/to/file.cpp, line 195
    b = 2: in file /path/to/file.cpp, line 195
    context1 = 10: in file /path/to/file.cpp, line 196
    context2 = Looks like something went wrong!: in file /path/to/file.cpp, line 197
```

Checks can also have descriptions:

```C++
check("a is 10", VAL(a) == 10);
```

That's all very cool, but I really just want a quick and simple test.

No problem:

```C++
simple("jsonEscape", [] (Check& check) {
    check(VAL(jsonEscape("")) == "");
    check(VAL(jsonEscape(helloWorld)) == helloWorld);
    check(VAL(jsonEscape("prefix\u0001postfix")) == "prefix\\u0001postfix");
    check(VAL(jsonEscape("prefix\u0010postfix")) == "prefix\\u0010postfix");
})
```

What about BDD? 
 
MosquitoNet avoids the almost aspect oriented DSL's of other testing frameworks, and sticks to a familiar C++ style.
This does cost a bit of boiler plate, but allows you to compose tests with standard C++ paradigms and makes control flow
clear.

```C++
struct BDDTest : GWT {
    BDDTest() : GWT(
        Given("some starting point"),
        When("we do something"),
        Then("we can verify the result")
    )
    // Set up the test.
    {}

    void when() {
        // Do something
    }

    void then() {
        // Check the result
    }
};

static Test::Unit u(
    gwt<BDDTest>("A bdd test")
);
```

And if I want to parameterize my test?

Easy.

```C++
class BDDTest final: GWT {
    int x_;
    int y_;
public:
    BDDTest(int x, int y) : GWT(
        Given("some starting point"),
        When("we do something"),
        Then("we can verify the result")
    ) : x_(x), y_(y) {}

    void when() {}
    void then() { check(VAL(x) == VAL(y)); }
};

void simpleTest(Check& check, int number, int anotherNumber);

static Test::Unit u(
    context("SomeTest",
        gwt<BDDTest>("A BDD test", 1, 1),
        simple("A simple test", simpleTest, 1, 2)
    )
);
```

## Further Examples

Take a look at the tests for [Util](cpp/test/src/TestUtil.cpp) or [Container](cpp/test/src/TestContainer.cpp) 
