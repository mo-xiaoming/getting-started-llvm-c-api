#ifndef PL0_UTILS_CHARS_HPP__
#define PL0_UTILS_CHARS_HPP__

#include <cctype>

namespace utils::chars {
[[nodiscard]] inline constexpr bool isdigit_s(char c) noexcept {
  return std::isdigit(static_cast<unsigned char>(c)) != 0;
}

[[nodiscard]] inline constexpr bool isspace_s(char c) noexcept {
  return std::isspace(static_cast<unsigned char>(c)) != 0;
}

[[nodiscard]] inline constexpr bool isalpha_s(char c) noexcept {
  return std::isalpha(static_cast<unsigned char>(c)) != 0;
}

[[nodiscard]] inline constexpr bool isident_s(char c) noexcept { return isdigit_s(c) || isalpha_s(c) || c == '_'; }
} // namespace utils::chars

#endif
