context("parse-errors")

test_that("useful errors are returned", {
    get_error <- function(css) {
        parse(css)
        NULL
    }

    expect_that(get_error("attributes(href)/html/body/a"),
                throws_error("Unexpected character"))
    expect_that(get_error("attributes(href)"),
                throws_error("Expected selector"))
    expect_that(get_error("html/body/a"),
                throws_error("Unexpected character"))
    expect_that(get_error(" "),
                throws_error("Expected selector, got <EOF at 2>"))
    expect_that(get_error("div, "),
                throws_error("Expected selector, got <EOF at 6>"))
    expect_that(get_error(" , div"),
                throws_error("Expected selector, got <DELIM ',' at 2>"))
    expect_that(get_error("p, , div"),
                throws_error("Expected selector, got <DELIM ',' at 4>"))
    expect_that(get_error("div > "),
                throws_error("Expected selector, got <EOF at 7>"))
    expect_that(get_error("  > div"),
                throws_error("Expected selector, got <DELIM '>' at 3>"))
    expect_that(get_error("foo|#bar"),
                throws_error("Expected ident or '\\*'"))
    expect_that(get_error("#.foo"),
                throws_error("Expected selector, got <DELIM '#' at 1>"))
    expect_that(get_error(".#foo"),
                throws_error("Expected ident, got <HASH 'foo' at 2>"))
    expect_that(get_error(":#foo"),
                throws_error("Expected ident, got <HASH 'foo' at 2>"))
    expect_that(get_error("[*]"),
                throws_error("Expected '|'"))
    expect_that(get_error("[foo|]"),
                throws_error("Expected ident, got <DELIM ']' at 6>"))
    expect_that(get_error("[#]"),
                throws_error("Expected ident or '\\*', got <DELIM '#' at 2>"))
    expect_that(get_error("[foo=#]"),
                throws_error("Expected string or ident, got <DELIM '#' at 6>"))
    expect_that(get_error(":nth-child()"),
                throws_error("Expected at least one argument, got <DELIM ')' at 12>"))
    expect_that(get_error("[href]a"),
                throws_error("Expected selector, got <IDENT 'a' at 7>"))
    expect_that(get_error("[rel=stylesheet]"),
                equals(NULL))
    expect_that(get_error("[rel:stylesheet]"),
                throws_error("Operator expected, got <DELIM ':' at 5>"))
    expect_that(get_error("[rel=stylesheet"),
                throws_error("Expected ']', got <EOF at 16>"))
    expect_that(get_error(":lang(fr)"),
                equals(NULL))
    expect_that(get_error(":lang(fr"),
                throws_error("Expected an argument, got <EOF at 9>"))
    expect_that(get_error(':contains("foo'),
                throws_error("Unclosed string at 11"))
    expect_that(get_error(':contains("foo\\"'),
                throws_error("Unclosed string at 11"))
    expect_that(get_error("foo!"),
                throws_error("Unexpected character"))
    expect_that(get_error("a:not(b;)"),
                throws_error("Unexpected character"))

    # Mis-placed pseudo-elements
    expect_that(get_error("a:before:empty"),
                throws_error("Got pseudo-element ::before not at the end of a selector"))
    expect_that(get_error("li:before a"),
                throws_error("Got pseudo-element ::before not at the end of a selector"))
    expect_that(get_error(":not(:before)"),
                throws_error("Got pseudo-element ::before inside :not\\(\\) at 13"))
    expect_that(get_error(":not(a,)"),
                throws_error("Expected ')', got .*"))
    expect_that(get_error(":not(:not(a))"),
                throws_error("Got nested :not()"))
})
