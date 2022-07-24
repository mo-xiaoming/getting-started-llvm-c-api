#ifndef PL0_UTILS_FOR_TEST_HPP__
#define PL0_UTILS_FOR_TEST_HPP__

#include "lexer.hpp"

inline lexer::lex_result_t lex_string(std::string_view content) {
  auto const cursor = lexer::internal::source_cursor_t(lexer::internal::source_content_t(content.data()),
                                                       lexer::internal::source_size_t(content.size()),
                                                       lexer::internal::source_position_t(0U));
  return lexer::internal::lex(cursor);
}

#endif
