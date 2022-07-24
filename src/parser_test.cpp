#include "lexer.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

#include <gtest/gtest-param-test.h>
#include <gtest/gtest.h>
#include <variant>

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, EmptyFile) {
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"()"));

  auto parser = parser::parser_t{tokens};
  auto const result = parser.parse();
  ASSERT_TRUE(std::holds_alternative<parser::parse_error_t>(result));
  auto const pe = std::get<parser::parse_error_t>(result);
  ASSERT_TRUE(std::holds_alternative<parser::parse_error_empty_file_t>(pe));
}

namespace {
struct test_data_t {
  std::string_view source;
  std::string_view expected;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, test_data_t v) {
    return os << utils::str::to_str("source:\n", v.source, "\n\n", "expected:\n", v.expected);
  }
};
// NOLINTNEXTLINE(hicpp-avoid-c-arrays, modernize-avoid-c-arrays, cppcoreguidelines-avoid-c-arrays)
constexpr test_data_t test_data[] = {
    // statements
    {R"(const a = 3;.)", "const a=3\n"},
    {R"(const b = 4, c = 5;.)", "const b=4\nconst c=5\n"},
    {R"(var e, f;.)", "var e\nvar f\n"},
    {R"(var g;.)", "var g\n"},
    {R"(var g;
?g.)",
     "var g\n?g\n"},
    {R"(call f.)", "call f\n"},
    {R"(var a;
begin
  ?a
end.)",
     R"(var a
begin
?a
end
)"},
    {R"(var a, b;
begin
  ?a;
  ?b
end.)",
     R"(var a
var b
begin
?a
?b
end
)"},
    {R"(var a, b;
begin
  begin
    ?b
  end
end.)",
     R"(var a
var b
begin
begin
?b
end
end
)"},
    {R"(var a, b;
begin
  ?a;
  begin
    ?b
  end
end.)",
     R"(var a
var b
begin
?a
begin
?b
end
end
)"},
    {R"(var a, b;
begin
  begin
    ?b
  end;
  ?a
end.)",
     R"(var a
var b
begin
begin
?b
end
?a
end
)"},
    {R"(var a, b, c;
begin
  begin
    ?b
  end;

  ?a;

  begin
    ?c
  end
end.)",
     R"(var a
var b
var c
begin
begin
?b
end
?a
begin
?c
end
end
)"},
    {R"(var a, b; if a # b then !a.)", "var a\nvar b\nif a#b then !a\n"},
    {R"(if odd 3 then !4.)", "if odd 3 then !4\n"},
    {R"(if 3 <= 4 then
begin !4 end.)",
     "if 3<=4 then begin\n!4\nend\n"},
    {R"(while odd 3 do !4.)", "while odd 3 do !4\n"},
    {R"(while 3 <= 4 do
begin !4 end.)",
     "while 3<=4 do begin\n!4\nend\n"},
    {R"(procedure xyz;
const x = 3;
var squ;
begin
  squ:=x*x
end;
call squ.)",
     "procedure xyz;const x=3\nvar squ\nbegin\nsqu:=(* x x)\nend\n;\ncall squ\n"},
    {R"(
procedure abc;
var x;
begin
!x
end;

procedure xyz;
var x;
begin
!x
end;

call squ.)",
     "procedure abc;var x\nbegin\n!x\nend\n;\nprocedure xyz;var x\nbegin\n!x\nend\n;\ncall squ\n"},
    // expressions
    {R"(!3.)", "!3\n"},
    {R"(var x;!x.)", "var x\n!x\n"},
    {R"(!+42.)", "!42\n"},
    {R"(!-42.)", "!(* -1 42)\n"},
    {R"(var x;!+x.)", "var x\n!x\n"},
    {R"(var x;!-x.)", "var x\n!(* -1 x)\n"},
    {R"(!3+4.)", "!(+ 3 4)\n"},
    {R"(!3*4.)", "!(* 3 4)\n"},
    {R"(!-3/4.)", "!(/ (* -1 3) 4)\n"},
    {R"(!-3-4.)", "!(- (* -1 3) 4)\n"},
    {R"(!(3).)", "!3\n"},
    {R"(!-(3).)", "!(* -1 3)\n"},
    {R"(!(-4).)", "!(* -1 4)\n"},
    {R"(!-(-4).)", "!(* -1 (* -1 4))\n"},
    {R"(!(3-4).)", "!(- 3 4)\n"},
    {R"(!(-3-4).)", "!(- (* -1 3) 4)\n"},
    {R"(!3+4*5.)", "!(+ 3 (* 4 5))\n"},
    {R"(!3*4+5.)", "!(+ (* 3 4) 5)\n"},
    {R"(!3+4-5.)", "!(- (+ 3 4) 5)\n"},
    {R"(!3+(4-5).)", "!(+ 3 (- 4 5))\n"},
    {R"(!(3+(4-5)).)", "!(+ 3 (- 4 5))\n"},
    {R"(!(3+4)-5.)", "!(- (+ 3 4) 5)\n"},
    {R"(!3+4*5/6-7.)", "!(- (+ 3 (/ (* 4 5) 6)) 7)\n"},
    {R"(!3+4*5/(6-7).)", "!(+ 3 (/ (* 4 5) (- 6 7)))\n"},
    {R"(!(3+4)*5/6-7.)", "!(- (/ (* (+ 3 4) 5) 6) 7)\n"},
    {R"(var a, b;!(3+a)/4+7*(b-42).)", "var a\nvar b\n!(+ (/ (+ 3 a) 4) (* 7 (- b 42)))\n"},
    {R"(var a, b;!(3+a)/(4+7*(b-42)).)", "var a\nvar b\n!(/ (+ 3 a) (+ 4 (* 7 (- b 42))))\n"},
    {R"(var a,b,c,abc;abc:=3+a-b*c.)", "var a\nvar b\nvar c\nvar abc\nabc:=(- (+ 3 a) (* b c))\n"},
};
} // namespace
struct ParserTestSuite : public testing::TestWithParam<test_data_t> {};

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
INSTANTIATE_TEST_SUITE_P(BasicData, ParserTestSuite, testing::ValuesIn(test_data));

// NOLINTNEXTLINE(cppcoreguidelines*, hicpp-special-member-functions)
TEST_P(ParserTestSuite, Basic) {
  auto const [source, expected] = GetParam();

  auto const tokens = std::get<lexer::tokens_t>(lex_string(source));
  auto parser = parser::parser_t{tokens};
  auto const result = parser.parse();
  ASSERT_TRUE(std::holds_alternative<parser::ast_t>(result))
      << "got error: " << std::get<parser::parse_error_t>(result);
  auto const& ast = std::get<parser::ast_t>(result);
  EXPECT_EQ(utils::str::to_str(ast), expected);
}
