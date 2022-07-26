#include "lib.hpp"

#include <gtest/gtest.h>

TEST(LibSuite, Foo) { ASSERT_EQ(lib::sum("3", "39"), 42); }
