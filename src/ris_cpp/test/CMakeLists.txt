enable_testing()

add_executable(run_tests test_main.cpp)

target_link_libraries(run_tests gtest gtest_main)

add_test(NAME run_tests COMMAND run_tests)