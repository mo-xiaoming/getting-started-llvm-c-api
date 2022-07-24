#include "lib.hpp"

#include <gtest/gtest.h>

TEST(LibSuite, Foo) { ASSERT_EQ(lib::foo("hello"), std::string("hello")); }
