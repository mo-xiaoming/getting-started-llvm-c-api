#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include "lexer_symbols.hpp"
#include "utils/chars.hpp"
#include "utils/static_map.hpp"

#include <optional>
#include <variant>

namespace lexer {
struct lex_error_file_unreadable_t {
  std::string source_path;
};
struct lex_unexpected_char_t {
  annotation_t annotation;
  std::string_view expected;
};
struct lex_unknown_char_t {
  annotation_t annotation;
};
using lex_result_t = std::variant<tokens_t, lex_unexpected_char_t, lex_unknown_char_t, lex_error_file_unreadable_t>;

namespace internal {
template <typename T>
concept small_pod = sizeof(T) < 16;

template <small_pod InternalType, typename Tag> struct strong_type_t {
  using base_t = strong_type_t<InternalType, Tag>;

  constexpr explicit strong_type_t(InternalType value) noexcept : m_value(value) {}

  constexpr InternalType value() const noexcept { return m_value; }

private:
  InternalType m_value;
};

struct source_nth_t : strong_type_t<std::size_t, struct source_nth_t_tag> {
  using base_t::base_t;
};
struct source_position_t : strong_type_t<std::size_t, struct source_position_t_tag> {
  using base_t::base_t;
};
struct source_size_t : strong_type_t<std::size_t, struct source_size_t_tag> {
  using base_t::base_t;
};
struct source_content_t : strong_type_t<char const*, struct source_content_t_tag> {
  using base_t::base_t;
};

[[nodiscard]] constexpr std::optional<symbol_t> find_keyword(std::string_view ident) noexcept {
  using namespace std::literals::string_view_literals;
  constexpr auto keywords_map = utils::container::make_static_map(std::array{
      std::pair("call"sv, symbol_t::call),
      std::pair("odd"sv, symbol_t::odd),
      std::pair("begin"sv, symbol_t::begin),
      std::pair("end"sv, symbol_t::end),
      std::pair("if"sv, symbol_t::if_),
      std::pair("while"sv, symbol_t::while_),
      std::pair("then"sv, symbol_t::then),
      std::pair("do"sv, symbol_t::do_),
      std::pair("const"sv, symbol_t::const_),
      std::pair("var"sv, symbol_t::var),
      std::pair("procedure"sv, symbol_t::proc),
  });
  return keywords_map.find(ident);
}

struct source_cursor_t {
  constexpr source_cursor_t(source_content_t content, source_size_t size, source_position_t cur_pos) noexcept
      : m_content(content), m_size(size), m_cur_pos(cur_pos) {}

  using const_iterator = char const*;
  using iterator = const_iterator;

  [[nodiscard]] constexpr std::string_view source() const noexcept { return m_content.value(); }
  [[nodiscard]] constexpr std::size_t from_begin() const noexcept { return m_cur_pos.value(); }

  [[nodiscard]] constexpr const_iterator begin() const noexcept { return m_content.value(); }
  [[nodiscard]] constexpr const_iterator end() const noexcept { return m_content.value() + m_size.value(); }
  [[nodiscard]] constexpr const_iterator cbegin() const noexcept { return m_content.value(); }
  [[nodiscard]] constexpr const_iterator cend() const noexcept { return m_content.value() + m_size.value(); }
  [[nodiscard]] constexpr const_iterator cur_iter() const noexcept { return m_content.value() + m_cur_pos.value(); }
  [[nodiscard]] constexpr const_iterator next_nth_iter(source_nth_t nth) const noexcept {
    return m_content.value() + m_cur_pos.value() + nth.value();
  }

  [[nodiscard]] constexpr std::optional<char> peek_next_nth_char(source_nth_t nth) const noexcept {
    auto const* const nth_iter = next_nth_iter(nth);
    if (nth_iter >= cend()) {
      return std::nullopt;
    }
    return *nth_iter;
  }
  [[nodiscard]] constexpr std::optional<char> peek_cur_char() const noexcept {
    return peek_next_nth_char(source_nth_t(0U));
  }
  [[nodiscard]] constexpr std::string_view get_number() const noexcept {
    const_iterator it = std::find_if_not(cur_iter(), end(), utils::chars::isdigit_s);
    return {cur_iter(), it};
  }
  [[nodiscard]] constexpr std::string_view get_identifier() const noexcept {
    const_iterator it = std::find_if_not(cur_iter(), cend(), utils::chars::isident_s);
    return {cur_iter(), it};
  }
  [[nodiscard]] constexpr source_cursor_t advance(source_nth_t nth) const noexcept {
    auto cursor = *this;
    cursor.m_cur_pos = source_position_t(m_cur_pos.value() + nth.value());
    return cursor;
  }

private:
  source_content_t m_content;
  source_size_t m_size;
  source_position_t m_cur_pos;
};

[[nodiscard]] lex_result_t lex(source_cursor_t cursor);
} // namespace internal

std::pair<std::vector<char>, lex_result_t> lex_source_file(std::string const& source_path);
} // namespace lexer

#endif
