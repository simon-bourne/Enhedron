#include "MosquitoNet.h"

#include <vector>

using namespace Enhedron::Test;
using std::vector;

static Suite u("a minimal test suite",
    given("a very simple test", [] (auto& check) {
        int a = 1;

        check(VAR(a) == 1);
    })
);
