#include "lexer.hpp"
#include "utils_for_test.hpp"

#include <gtest/gtest.h>

#include <filesystem>

namespace {
std::string const& get_mock_source_path() {
  static auto const mock_source_path = std::string("hopefully_this_does_not_exist");
  return mock_source_path;
}
} // namespace

// NOLINTNEXTLINE
TEST(LexerTestSuite, NonExistSourceFileShouldReturnFileUnreadableError) {
  auto const& mock_source_path = get_mock_source_path();

  auto err = std::error_code();
  auto const exists = std::filesystem::exists(mock_source_path, err);
  ASSERT_TRUE(!exists && !err) << "exists: " << exists << ", err: " << err.message();

  auto const& [_, ret] = lexer::lex_source_file(mock_source_path);
  auto const* const error = std::get_if<lexer::lex_error_file_unreadable_t>(&ret);
  ASSERT_NE(error, nullptr);
  ASSERT_EQ(error->source_path, mock_source_path);
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, IncompleteBecomes) {
  constexpr auto src = std::string_view(R"(a : b;)");
  auto const ret = lex_string(src);
  auto const* const error = std::get_if<lexer::lex_unexpected_char_t>(&ret);
  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->expected, ":=");
  EXPECT_EQ(error->annotation, (annotation_t{.source = src, .start = 2, .length = 2U}));
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, UnrecognizedChar) {
  constexpr auto src = std::string_view(R"(a@ : b;)");
  auto const ret = lex_string(src);
  auto const* const error = std::get_if<lexer::lex_unknown_char_t>(&ret);
  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->annotation, (annotation_t{.source = src, .start = 1, .length = 1U}));
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, Normal) {
  constexpr auto src = std::string_view(R"(var x, squ;

procedure square;
const f := 3;
begin
  while squ > 0 do if odd b then y := x + 3 / 4;
  while x <= y do if a>=b then w:=2*w-7;
  while x = y do if x#b then ?a;
  if x < y then b := (3 + 4) / 42;
  if b then call square;
end;

!a
.
)");

  auto const ret = lex_string(src);

  using enum lexer::symbol_t;
  constexpr auto tokens_oracle = std::array{
      lexer::token_t{.symbol = var, .annotation = {.source = src, .start = 0U, .length = 3U}},             // var
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 4U, .length = 1U}},           // x
      lexer::token_t{.symbol = comma, .annotation = {.source = src, .start = 5U, .length = 1U}},           // ,
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 7U, .length = 3U}},           // squ
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 10U, .length = 1U}},      // ;
      lexer::token_t{.symbol = proc, .annotation = {.source = src, .start = 13U, .length = 9U}},           // procedure
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 23U, .length = 6U}},          // square
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 29U, .length = 1U}},      // ;
      lexer::token_t{.symbol = const_, .annotation = {.source = src, .start = 31U, .length = 5U}},         // const
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 37U, .length = 1U}},          // f
      lexer::token_t{.symbol = becomes, .annotation = {.source = src, .start = 39U, .length = 2U}},        // :=
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 42U, .length = 1U}},         // 3
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 43U, .length = 1U}},      // ;
      lexer::token_t{.symbol = begin, .annotation = {.source = src, .start = 45U, .length = 5U}},          // begin
      lexer::token_t{.symbol = while_, .annotation = {.source = src, .start = 53U, .length = 5U}},         // while
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 59U, .length = 3U}},          // x
      lexer::token_t{.symbol = greater, .annotation = {.source = src, .start = 63U, .length = 1U}},        // <=
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 65U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.source = src, .start = 67U, .length = 2U}},            // do
      lexer::token_t{.symbol = if_, .annotation = {.source = src, .start = 70U, .length = 2U}},            // if
      lexer::token_t{.symbol = odd, .annotation = {.source = src, .start = 73U, .length = 3U}},            // odd
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 77U, .length = 1U}},          // b
      lexer::token_t{.symbol = then, .annotation = {.source = src, .start = 79U, .length = 4U}},           // then
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 84U, .length = 1U}},          // y
      lexer::token_t{.symbol = becomes, .annotation = {.source = src, .start = 86U, .length = 2U}},        // :=
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 89U, .length = 1U}},          // x
      lexer::token_t{.symbol = plus, .annotation = {.source = src, .start = 91U, .length = 1U}},           // +
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 93U, .length = 1U}},         // 3
      lexer::token_t{.symbol = divide, .annotation = {.source = src, .start = 95U, .length = 1U}},         // /
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 97U, .length = 1U}},         // 4
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 98U, .length = 1U}},      // ;
      lexer::token_t{.symbol = while_, .annotation = {.source = src, .start = 102U, .length = 5U}},        // while
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 108U, .length = 1U}},         // x
      lexer::token_t{.symbol = less_equal, .annotation = {.source = src, .start = 110U, .length = 2U}},    // <=
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 113U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.source = src, .start = 115U, .length = 2U}},           // do
      lexer::token_t{.symbol = if_, .annotation = {.source = src, .start = 118U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 121U, .length = 1U}},         // a
      lexer::token_t{.symbol = greater_equal, .annotation = {.source = src, .start = 122U, .length = 2U}}, // >=
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 124U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.source = src, .start = 126U, .length = 4U}},          // then
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 131U, .length = 1U}},         // w
      lexer::token_t{.symbol = becomes, .annotation = {.source = src, .start = 132U, .length = 2U}},       // :=
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 134U, .length = 1U}},        // 2
      lexer::token_t{.symbol = times, .annotation = {.source = src, .start = 135U, .length = 1U}},         // *
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 136U, .length = 1U}},         // w
      lexer::token_t{.symbol = minus, .annotation = {.source = src, .start = 137U, .length = 1U}},         // -
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 138U, .length = 1U}},        // 7
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 139U, .length = 1U}},     // ;
      lexer::token_t{.symbol = while_, .annotation = {.source = src, .start = 143U, .length = 5U}},        // while
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 149U, .length = 1U}},         // x
      lexer::token_t{.symbol = equal, .annotation = {.source = src, .start = 151U, .length = 1U}},         // =
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 153U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.source = src, .start = 155U, .length = 2U}},           // do
      lexer::token_t{.symbol = if_, .annotation = {.source = src, .start = 158U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 161U, .length = 1U}},         // x
      lexer::token_t{.symbol = not_equal, .annotation = {.source = src, .start = 162U, .length = 1U}},     // #
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 163U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.source = src, .start = 165U, .length = 4U}},          // then
      lexer::token_t{.symbol = in, .annotation = {.source = src, .start = 170U, .length = 1U}},            // ?
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 171U, .length = 1U}},         // a
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 172U, .length = 1U}},     // ;
      lexer::token_t{.symbol = if_, .annotation = {.source = src, .start = 176U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 179U, .length = 1U}},         // x
      lexer::token_t{.symbol = less, .annotation = {.source = src, .start = 181U, .length = 1U}},          // <
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 183U, .length = 1U}},         // y
      lexer::token_t{.symbol = then, .annotation = {.source = src, .start = 185U, .length = 4U}},          // then
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 190U, .length = 1U}},         // b
      lexer::token_t{.symbol = becomes, .annotation = {.source = src, .start = 192U, .length = 2U}},       // :=
      lexer::token_t{.symbol = lparen, .annotation = {.source = src, .start = 195U, .length = 1U}},        // (
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 196U, .length = 1U}},        // 3
      lexer::token_t{.symbol = plus, .annotation = {.source = src, .start = 198U, .length = 1U}},          // +
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 200U, .length = 1U}},        // 4
      lexer::token_t{.symbol = rparen, .annotation = {.source = src, .start = 201U, .length = 1U}},        // )
      lexer::token_t{.symbol = divide, .annotation = {.source = src, .start = 203U, .length = 1U}},        // /
      lexer::token_t{.symbol = number, .annotation = {.source = src, .start = 205U, .length = 2U}},        // 42
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 207U, .length = 1U}},     // ;
      lexer::token_t{.symbol = if_, .annotation = {.source = src, .start = 211U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 214U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.source = src, .start = 216U, .length = 4U}},          // then
      lexer::token_t{.symbol = call, .annotation = {.source = src, .start = 221U, .length = 4U}},          // call
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 226U, .length = 6U}},         // square
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 232U, .length = 1U}},     // ;
      lexer::token_t{.symbol = end, .annotation = {.source = src, .start = 234U, .length = 3U}},           // end
      lexer::token_t{.symbol = semicolon, .annotation = {.source = src, .start = 237U, .length = 1U}},     // ;
      lexer::token_t{.symbol = out, .annotation = {.source = src, .start = 240U, .length = 1U}},           // !
      lexer::token_t{.symbol = ident, .annotation = {.source = src, .start = 241U, .length = 1U}},         // a
      lexer::token_t{.symbol = period, .annotation = {.source = src, .start = 243U, .length = 1U}},        // .
  };

  auto const* const tokens = std::get_if<lexer::tokens_t>(&ret);
  ASSERT_NE(tokens, nullptr);
  EXPECT_EQ(tokens->size(), tokens_oracle.size());
  for (std::size_t i = 0; i != std::min(tokens_oracle.size(), tokens->size()); ++i) {
    ASSERT_EQ((*tokens)[i], tokens_oracle[i]) << "at loop " << i;
  }
}
