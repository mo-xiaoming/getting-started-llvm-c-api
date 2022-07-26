#ifndef LIB_H__
#define LIB_H__

#include <cstdint>

namespace lib {
using native_int_t = std::int64_t;

native_int_t sum(char const* a, char const* b);
} // namespace lib

#endif
