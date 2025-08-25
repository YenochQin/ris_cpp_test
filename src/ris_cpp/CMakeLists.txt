cmake_minimum_required(VERSION 3.10)
project(standalone_ris4 CXX)

enable_testing()

add_executable(standalone_ris4 main.cpp io.cpp debug.cpp machine.cpp state.cpp constants.cpp summary.cpp csl.cpp def.cpp smd.cpp facts.cpp mixblock.cpp ris_cal.cpp globals.cpp fortran_interface.cpp)

find_package(GTest REQUIRED)

add_executable(run_tests test/test_modern_cpp.cpp io.cpp debug.cpp machine.cpp state.cpp constants.cpp summary.cpp csl.cpp def.cpp smd.cpp facts.cpp mixblock.cpp ris_cal.cpp globals.cpp fortran_interface.cpp)

target_link_libraries(run_tests GTest::gtest_main)

add_test(NAME standalone_ris4_tests COMMAND run_tests)
