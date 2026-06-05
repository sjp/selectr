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
    expect_that(get_error("[rel=stylesheet k]"),
                throws_error("Expected ']', got <IDENT 'k' at 17>"))
    expect_that(get_error("[rel=stylesheet i i]"),
                throws_error("Expected ']', got <IDENT 'i' at 19>"))
    # A case-sensitivity flag requires an operator and value
    expect_that(get_error("[rel i]"),
                throws_error("Operator expected, got <IDENT 'i' at 6>"))
    expect_that(get_error(":lang(fr)"),
                equals(NULL))
    # EOF only auto-closes a construct (see below); a missing interior
    # part still errors, exactly as its closed form would
    expect_that(get_error("[foo="),
                throws_error("Expected string or ident, got <EOF at 6>"))
    expect_that(get_error("["),
                throws_error("Expected ident or '\\*', got <EOF at 2>"))
    expect_that(get_error(":lang("),
                throws_error("Expected at least one argument, got <EOF at 7>"))
    expect_that(get_error(":is(a,"),
                throws_error("Expected selector, got <EOF at 7>"))
    expect_that(get_error("foo!"),
                throws_error("Unexpected character"))
    # The non-standard != attribute operator is not supported
    expect_that(get_error("a[rel!=nofollow]"),
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
    expect_that(get_error(":is(:before)"),
                throws_error("Got pseudo-element ::before inside :is\\(\\) at 12"))
    expect_that(get_error(":matches(:before)"),
                throws_error("Got pseudo-element ::before inside :matches\\(\\) at 17"))
    # pseudo-elements are rejected anywhere in a complex argument
    expect_that(get_error(":is(a:before b)"),
                throws_error("Got pseudo-element ::before inside :is\\(\\)"))
    expect_that(get_error(":is(a b:before)"),
                throws_error("Got pseudo-element ::before inside :is\\(\\)"))
    # trailing combinators in arguments
    expect_that(get_error(":is(a >)"),
                throws_error("Expected selector, got <DELIM '\\)' at 8>"))
})

test_that("constructs unclosed at EOF translate as their closed forms", {
    # css-syntax-3 auto-closes open blocks, functions, and strings at
    # EOF: the parse error is flagged, not fatal, and browsers accept
    # these selectors
    eof <- function(unclosed, closed) {
        for (translator in c("generic", "html", "xhtml")) {
            expect_that(css_to_xpath(unclosed, translator = translator),
                        equals(css_to_xpath(closed, translator = translator)))
        }
    }

    eof("[rel", "[rel]")
    eof("[rel=stylesheet", "[rel=stylesheet]")
    eof("[rel=stylesheet i", "[rel=stylesheet i]")
    eof('[foo="bar', '[foo="bar"]')
    eof('[foo="', '[foo=""]')
    eof(":lang(fr", ":lang(fr)")
    eof(":nth-child(2n+1", ":nth-child(2n+1)")
    eof(":is(a", ":is(a)")
    eof("e:is(a, b", "e:is(a, b)")
    eof(":not(a", ":not(a)")
    eof(":has(> a", ":has(> a)")
    # An ident ending in an escaped backslash, then an unclosed
    # attribute block: tokenizes as <IDENT 'di\'> <DELIM '['>
    # <IDENT 'v'> and auto-closes to an existence test
    eof("di\\\\[v", "di\\\\[v]")
    # The unclosed string is auto-closed at parse time; the
    # pseudo-class is then rejected at translation time either way
    expect_error(css_to_xpath(':contains("foo'),
                 "The pseudo-class :contains\\(\\) is unknown")
})
