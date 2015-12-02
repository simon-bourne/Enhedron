cmake_minimum_required(VERSION 2.8.7)
project(Enhedron)

include (CMakeLists.flags.txt)

if (CMAKE_CXX_COMPILER_ID STREQUAL Clang OR CMAKE_CXX_COMPILER_ID STREQUAL GNU)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wconversion")
endif()

include_directories(cpp/test/include cpp/lib/include)

add_executable(test-harness
        cpp/test/src/Test/Harness.cpp
        cpp/test/src/Test/Util.cpp
        cpp/test/src/Test/Assert.cpp
        cpp/test/src/Test/CommandLine.cpp)

add_executable(integration-test cpp/test/src/Test/Harness.cpp cpp/test/src/Test/Integration.cpp)
