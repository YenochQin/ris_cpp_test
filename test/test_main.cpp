#include "gtest/gtest.h"

TEST(HelloTest, BasicAssertions) {
  EXPECT_STREQ("Hello, world!", "Hello, world!");
}
