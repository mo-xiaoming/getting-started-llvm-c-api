#ifndef PL0_ANNOTATION_HPP__
#define PL0_ANNOTATION_HPP__

#include <ostream>

struct annotation_t {
  std::string_view source;
  std::size_t start = 0;
  std::size_t length = 0;

  [[nodiscard]] friend constexpr bool operator==(annotation_t const& lhs, annotation_t const& rhs) noexcept = default;

  friend std::ostream& operator<<(std::ostream& os, annotation_t const& v) {
    return os << "{ .start=" << static_cast<void const*>(v.source.data() + v.start) << ", .length=" << v.length << " }";
  }
};

[[nodiscard]] constexpr std::string_view to_sv(annotation_t const& annotation) noexcept {
  return annotation.source.substr(annotation.start, annotation.length);
}

[[nodiscard]] std::string annotation_to_error_string(annotation_t const& annotation);
#endif
