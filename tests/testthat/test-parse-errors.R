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
    expect_that(get_error(":lang(en, fr)"),
                equals(NULL))
    expect_that(get_error(":lang( en , fr )"),
                equals(NULL))
    # A second range without a preceding comma is rejected, not
    # silently treated as comma-separated (whitespace is not a
    # substitute for ',')
    expect_that(get_error(":lang(en fr)"),
                throws_error("Expected ',' or '\\)', got <IDENT 'fr' at 10>"))
    expect_that(get_error(":lang(en *)"),
                throws_error("Expected ',' or '\\)', got <DELIM '\\*' at 10>"))
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
    # A trailing comma is reported as the missing selector it is, not
    # as an unexpected ',' that was in fact expected
    expect_that(get_error(":not(a,)"),
                throws_error("Expected selector after ',', got <DELIM '\\)' at 8>"))
    expect_that(get_error(":is(a,)"),
                throws_error("Expected selector after ',', got <DELIM '\\)' at 7>"))
    expect_that(get_error(":is(a, )"),
                throws_error("Expected selector after ',', got <DELIM '\\)' at 8>"))
    expect_that(get_error(":has(a,)"),
                throws_error("Expected selector after ',', got <DELIM '\\)' at 8>"))
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

test_that("unsupported column constructs are rejected by name", {
    # The Selectors 4 column combinator and column pseudo-classes
    # depend on table-layout arithmetic that XPath 1.0 cannot express;
    # the combinator is named in its parse error rather than falling
    # through to a stray-token message
    expect_error(css_to_xpath("a || b"),
                 "The column combinator '||' is not supported",
                 fixed = TRUE)
    expect_error(css_to_xpath("a||b"),
                 "The column combinator '||' is not supported",
                 fixed = TRUE)

    # The unknown-pseudo-class error keeps the user's hyphenated
    # spelling (not the method-ised ':nth_col()')
    expect_error(css_to_xpath("e:nth-col(2)"),
                 "The pseudo-class :nth-col() is unknown",
                 fixed = TRUE)
    expect_error(css_to_xpath("e:nth-last-col(2)"),
                 "The pseudo-class :nth-last-col() is unknown",
                 fixed = TRUE)

    # Single-pipe namespace syntax is unaffected
    expect_that(css_to_xpath("*|b", prefix = ""),
                equals("*[local-name() = 'b']"))
    expect_that(css_to_xpath("|b", prefix = ""),
                equals("b"))
})

test_that("parse errors include a caret-pointer gutter", {
    # Verify that the caret-pointer gutter block is appended to the message.
    # The gutter lines are:  "  |"  /  "  | <css>"  /  "  | <spaces>^"
    err <- function(css) {
        tryCatch(parse(css), error = function(e) conditionMessage(e))
    }

    # caret under the EOF that follows "div > "
    expect_match(err("div > "),
                 "\n  \\|\n  \\| div > \n  \\|       \\^",
                 perl = TRUE)

    # caret under the '#' that cannot start an attribute value
    expect_match(err("[foo=#]"),
                 "\n  \\|\n  \\| \\[foo=#\\]\n  \\|      \\^",
                 perl = TRUE)

    # caret under the '/' that is not a valid CSS character
    expect_match(err("html/body"),
                 "\n  \\|\n  \\| html/body\n  \\|     \\^",
                 perl = TRUE)

    # message text is still the first line (existing assertions stay valid)
    expect_match(err("div > "), "^Expected selector, got <EOF at 7>")
})

test_that("the caret gutter accounts for tabs and wide characters", {
    # A tab is echoed as a tab in the padding (rather than a single
    # space) so the terminal applies the same tab stops to the source
    # line and the caret line, keeping the two aligned regardless of
    # the tab's rendered width.
    err <- tryCatch(parse("\tdiv >"), error = identity)
    expect_equal(err$pos, 7)
    expect_match(conditionMessage(err),
                 "\n  \\|\n  \\| \tdiv >\n  \\| \t     \\^",
                 perl = TRUE)

    # CJK characters render as double-width; the padding uses two spaces
    # per such character so the caret still lands under the offending
    # column instead of one column short of it.
    css <- "日本語 >"
    err2 <- tryCatch(parse(css), error = identity)
    expect_equal(err2$pos, 6)
    lines <- strsplit(conditionMessage(err2), "\n", fixed = TRUE)[[1]]
    caret_line <- sub("^  \\| ", "", lines[4])
    padding <- sub("\\^$", "", caret_line)
    expect_equal(nchar(padding, type = "width"),
                 nchar(substr(css, 1, err2$pos - 1L), type = "width"))
})

test_that("parse errors are structured conditions", {
    e <- tryCatch(css_to_xpath("div >"), error = identity)
    expect_s3_class(e, "selectr_parse_error")
    expect_s3_class(e, "selectr_error")
    expect_s3_class(e, "error")
    expect_equal(e$pos, 6)
    expect_equal(e$selector, "div >")

    # the class is catchable by a dedicated handler, not just error =
    expect_equal(tryCatch(css_to_xpath("div >"),
                          selectr_parse_error = function(e) "structured",
                          error = function(e) "plain"),
                 "structured")

    # message text and the caret gutter are both preserved on the condition
    expect_match(conditionMessage(e), "^Expected selector, got <EOF at 6>")
    expect_match(conditionMessage(e), "\n  \\|\n  \\| div >\n  \\|      \\^",
                 perl = TRUE)

    # parse() itself signals the same structured condition
    p <- tryCatch(parse("[foo=#]"), error = identity)
    expect_s3_class(p, "selectr_parse_error")
    expect_equal(p$pos, 6)
    expect_equal(p$selector, "[foo=#]")

    # positionless failures still carry the class, with a NULL pos
    tok <- tryCatch(parse("\\"), error = identity)
    expect_s3_class(tok, "selectr_parse_error")
})

test_that("an ID selector must be identifier-shaped", {
    # Selectors requires the hash of an ID selector to be of type "id",
    # i.e. its name must start an identifier. Browsers throw a
    # SyntaxError for document.querySelector("#1").
    expect_error(css_to_xpath("#1"),
                 "Invalid ID selector '#1' at position 1; an identifier ",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("#-1"), "Escape it: '#-\\31 '",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("#-"), "an ID cannot be '-' alone",
                 fixed = TRUE, class = "selectr_parse_error")
    # the caret points at the '#', not at the offending digit's position
    expect_equal(tryCatch(css_to_xpath("div #1"), error = identity)$pos, 5)

    # names that do start an identifier stay legal, on both the fast
    # path (#-x) and through the tokenizer (#\31 )
    expect_equal(css_to_xpath("#-x", prefix = ""), "*[@id = '-x']")
    expect_equal(css_to_xpath("#_x", prefix = ""), "*[@id = '_x']")
    expect_equal(css_to_xpath("#--a", prefix = ""), "*[@id = '--a']")
    expect_equal(css_to_xpath("#\\31 ", prefix = ""), "*[@id = '1']")
    expect_equal(css_to_xpath("#\\31 23", prefix = ""), "*[@id = '123']")
})
