#include "lexer.hpp"

#include <fstream>

namespace lexer {

std::pair<std::vector<char>, lex_result_t> lex_source_file(std::string const& source_path) {
  auto const err_fn = [&source_path] { return lex_error_file_unreadable_t{.source_path = source_path}; };

  auto file = std::ifstream(source_path);
  if (!file) {
    return {{}, err_fn()};
  }

  std::vector<char> buffer(static_cast<std::size_t>(file.seekg(0, std::ios::end).tellg()));
  if (file.seekg(0, std::ios::beg).read(buffer.data(), static_cast<std::streamsize>(buffer.size()))) {
    auto const cursor =
        internal::source_cursor_t(internal::source_content_t(buffer.data()), internal::source_size_t(buffer.size()),
                                  internal::source_position_t(0U));
    return {std::move(buffer), lex(cursor)};
  }
  return {std::move(buffer), err_fn()};
}

namespace internal {
lex_result_t lex(source_cursor_t cursor) {
  auto tokens = tokens_t();

  auto const add_n_chars_token = [&tokens](source_cursor_t const& csr, symbol_t sym, std::size_t n) {
    tokens.emplace_back(sym, annotation_t{.source = csr.source(), .start = csr.from_begin(), .length = n});
    return csr.advance(source_nth_t(n));
  };
  auto const add_ident_or_keyword_token = [&add_n_chars_token](source_cursor_t const& csr, std::string_view ident) {
    auto const sym = [ident] {
      if (auto const possible_keyword = find_keyword(ident); possible_keyword) {
        return *possible_keyword;
      }
      return symbol_t::ident;
    }();
    return add_n_chars_token(csr, sym, ident.size());
  };

  constexpr auto const try_match_next_char = [](source_cursor_t const& csr, char expected) {
    const auto pc = csr.peek_next_nth_char(source_nth_t(1U));
    return pc && *pc == expected;
  };

  static constexpr auto single_char_to_token_map = utils::container::make_static_map(std::array{
      std::pair(';', symbol_t::semicolon),
      std::pair(',', symbol_t::comma),
      std::pair('(', symbol_t::lparen),
      std::pair(')', symbol_t::rparen),
      std::pair('*', symbol_t::times),
      std::pair('/', symbol_t::divide),
      std::pair('+', symbol_t::plus),
      std::pair('-', symbol_t::minus),
      std::pair('=', symbol_t::equal),
      std::pair('#', symbol_t::not_equal),
      std::pair('?', symbol_t::in),
      std::pair('!', symbol_t::out),
      std::pair('.', symbol_t::period),
  });

  while (auto const pc = cursor.peek_cur_char()) {
    if (!pc.has_value()) {
      break;
    }
    char const c = *pc;
    if (auto const sym = single_char_to_token_map.find(c); sym) {
      cursor = add_n_chars_token(cursor, *sym, 1U);
    } else if (c == '<') {
      if (try_match_next_char(cursor, '=')) {
        cursor = add_n_chars_token(cursor, symbol_t::less_equal, 2U);
      } else {
        cursor = add_n_chars_token(cursor, symbol_t::less, 1U);
      }
    } else if (c == '>') {
      if (try_match_next_char(cursor, '=')) {
        cursor = add_n_chars_token(cursor, symbol_t::greater_equal, 2U);
      } else {
        cursor = add_n_chars_token(cursor, symbol_t::greater, 1U);
      }
    } else if (c == ':') {
      if (try_match_next_char(cursor, '=')) {
        cursor = add_n_chars_token(cursor, symbol_t::becomes, 2U);
      } else {
        return lex_unexpected_char_t{
            .annotation = annotation_t{.source = cursor.source(), .start = cursor.from_begin(), .length = 2U},
            .expected = ":=",
        };
      }
    } else if (utils::chars::isdigit_s(c)) {
      auto const num = cursor.get_number();
      cursor = add_n_chars_token(cursor, symbol_t::number, num.size());
    } else if (utils::chars::isalpha_s(c)) {
      auto const ident = cursor.get_identifier();
      cursor = add_ident_or_keyword_token(cursor, ident);
    } else {
      if (!utils::chars::isspace_s(c)) {
        return lex_unknown_char_t{
            .annotation = annotation_t{.source = cursor.source(), .start = cursor.from_begin(), .length = 1U}};
      }
      cursor = cursor.advance(source_nth_t(1U));
    }
  }
  return tokens;
}
} // namespace internal
} // namespace lexer
