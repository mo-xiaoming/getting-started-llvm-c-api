#include "annotation.hpp"

#include <iomanip>

std::string annotation_to_error_string(annotation_t const& annotation) {
  std::ostringstream oss;
  auto const prev_nl_n = annotation.source.substr(0, annotation.start).rfind('\n');
  auto const prev_nl = (prev_nl_n == std::string_view::npos ? 0 : prev_nl_n + 1);
  auto const next_nl = annotation.source.substr(prev_nl).find('\n');
  constexpr auto leading_space = 8UL;
  oss << std::setw(leading_space) << "00|";
  auto const line = annotation.source.substr(prev_nl, next_nl);
  oss << line;
  if (line.back() != '\n') {
    oss << '\n';
  }
  for (auto i = 0UL; i != leading_space + annotation.start - prev_nl; ++i) {
    oss << ' ';
  }
  for (auto i = 0UL; i != annotation.length; ++i) {
    oss << '^';
  }
  oss << '\n';
  return std::move(oss).str();
}
