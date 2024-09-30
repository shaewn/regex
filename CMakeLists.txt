cmake_minimum_required(VERSION 3.15)

project(regex)

set(SOURCES
    src/regex.cpp)

add_executable(regex ${SOURCES})
target_compile_features(regex PUBLIC cxx_std_20)