test_that("useful errors are returned", {
    get_error <- function(css) {
        parse(css)
        NULL
    }

    expect_error(get_error("attributes(href)/html/body/a"),
                 "Unexpected character")
    expect_error(get_error("attributes(href)"),
                 "Expected selector")
    expect_error(get_error("html/body/a"),
                 "Unexpected character")
    expect_error(get_error(" "),
                 "Expected selector, got <EOF at 2>")
    expect_error(get_error("div, "),
                 "Expected selector, got <EOF at 6>")
    expect_error(get_error(" , div"),
                 "Expected selector, got <DELIM ',' at 2>")
    expect_error(get_error("p, , div"),
                 "Expected selector, got <DELIM ',' at 4>")
    expect_error(get_error("div > "),
                 "Expected selector, got <EOF at 7>")
    expect_error(get_error("  > div"),
                 "Expected selector, got <DELIM '>' at 3>")
    expect_error(get_error("foo|#bar"),
                 "Expected ident or '\\*'")
    expect_error(get_error("#.foo"),
                 "Expected selector, got <DELIM '#' at 1>")
    expect_error(get_error(".#foo"),
                 "Expected ident, got <HASH 'foo' at 2>")
    expect_error(get_error(":#foo"),
                 "Expected ident, got <HASH 'foo' at 2>")
    expect_error(get_error("[*]"),
                 "Expected '|'")
    expect_error(get_error("[foo|]"),
                 "Expected ident, got <DELIM ']' at 6>")
    expect_error(get_error("[#]"),
                 "Expected ident or '\\*', got <DELIM '#' at 2>")
    expect_error(get_error("[foo=#]"),
                 "Expected string or ident, got <DELIM '#' at 6>")
    expect_error(get_error(":nth-child()"),
                 "Expected at least one argument, got <DELIM ')' at 12>")
    expect_error(get_error("[href]a"),
                 "Expected selector, got <IDENT 'a' at 7>")
    expect_equal(get_error("[rel=stylesheet]"),
                 NULL)
    expect_error(get_error("[rel:stylesheet]"),
                 "Operator expected, got <DELIM ':' at 5>")
    expect_error(get_error("[rel=stylesheet k]"),
                 "Expected ']', got <IDENT 'k' at 17>")
    expect_error(get_error("[rel=stylesheet i i]"),
                 "Expected ']', got <IDENT 'i' at 19>")
    # A case-sensitivity flag requires an operator and value
    expect_error(get_error("[rel i]"),
                 "Operator expected, got <IDENT 'i' at 6>")
    expect_equal(get_error(":lang(fr)"),
                 NULL)
    expect_equal(get_error(":lang(en, fr)"),
                 NULL)
    expect_equal(get_error(":lang( en , fr )"),
                 NULL)
    # A second range without a preceding comma is rejected, not
    # silently treated as comma-separated (whitespace is not a
    # substitute for ',')
    expect_error(get_error(":lang(en fr)"),
                 "Expected ',' or '\\)', got <IDENT 'fr' at 10>")
    expect_error(get_error(":lang(en *)"),
                 "Expected ',' or '\\)', got <DELIM '\\*' at 10>")
    # EOF only auto-closes a construct (see below); a missing interior
    # part still errors, exactly as its closed form would
    expect_error(get_error("[foo="),
                 "Expected string or ident, got <EOF at 6>")
    expect_error(get_error("["),
                 "Expected ident or '\\*', got <EOF at 2>")
    expect_error(get_error(":lang("),
                 "Expected at least one argument, got <EOF at 7>")
    expect_error(get_error(":is(a,"),
                 "Expected selector, got <EOF at 7>")
    expect_error(get_error("foo!"),
                 "Unexpected character")
    # The non-standard != attribute operator is not supported
    expect_error(get_error("a[rel!=nofollow]"),
                 "Unexpected character")
    expect_error(get_error("a:not(b;)"),
                 "Unexpected character")

    # Mis-placed pseudo-elements
    expect_error(get_error("a:before:empty"),
                 "Got pseudo-element ::before not at the end of a selector")
    expect_error(get_error("li:before a"),
                 "Got pseudo-element ::before not at the end of a selector")
    expect_error(get_error(":not(:before)"),
                 "Got pseudo-element ::before inside :not\\(\\)")
    # A trailing comma is reported as the missing selector it is, not
    # as an unexpected ',' that was in fact expected
    expect_error(get_error(":not(a,)"),
                 "Expected selector after ',', got <DELIM '\\)' at 8>")
    expect_error(get_error(":is(a,)"),
                 "Expected selector after ',', got <DELIM '\\)' at 7>")
    expect_error(get_error(":is(a, )"),
                 "Expected selector after ',', got <DELIM '\\)' at 8>")
    expect_error(get_error(":has(a,)"),
                 "Expected selector after ',', got <DELIM '\\)' at 8>")
    expect_error(get_error(":is(:before)"),
                 "Got pseudo-element ::before inside :is\\(\\)")
    expect_error(get_error(":matches(:before)"),
                 "Got pseudo-element ::before inside :matches\\(\\)")
    # pseudo-elements are rejected anywhere in a complex argument
    expect_error(get_error(":is(a:before b)"),
                 "Got pseudo-element ::before inside :is\\(\\)")
    expect_error(get_error(":is(a b:before)"),
                 "Got pseudo-element ::before inside :is\\(\\)")
    # trailing combinators in arguments
    expect_error(get_error(":is(a >)"),
                 "Expected selector, got <DELIM '\\)' at 8>")
    # Only :is()/:where() take a forgiving selector list, so an empty
    # argument list stays an error for the other functional pseudo-classes
    expect_error(get_error(":not()"),
                 "Expected selector, got <DELIM '\\)' at 6>")
    expect_error(get_error(":has()"),
                 "Expected selector, got <DELIM '\\)' at 6>")
    expect_error(get_error("a:not( )"),
                 "Expected selector, got <DELIM '\\)' at 8>")
    # An empty forgiving list is empty, not a list holding one empty
    # selector: a lone ',' is still an error
    expect_error(get_error(":is(,)"),
                 "Expected selector, got <DELIM ',' at 5>")
})

test_that("constructs unclosed at EOF translate as their closed forms", {
    # css-syntax-3 auto-closes open blocks, functions, and strings at
    # EOF: the parse error is flagged, not fatal, and browsers accept
    # these selectors
    eof <- function(unclosed, closed) {
        for (translator in c("generic", "html", "xhtml")) {
            expect_equal(css_to_xpath(unclosed, translator = translator),
                         css_to_xpath(closed, translator = translator))
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
    # An empty forgiving selector list is valid, so an unclosed one
    # auto-closes to it rather than erroring on the missing argument
    eof(":is(", ":is()")
    eof("a:where(", "a:where()")
    eof(":not(a", ":not(a)")
    eof(":has(> a", ":has(> a)")
    # An ident ending in an escaped backslash, then an unclosed
    # attribute block: tokenizes as <IDENT 'di\'> <DELIM '['>
    # <IDENT 'v'> and auto-closes to an existence test
    eof("di\\\\[v", "di\\\\[v]")
    # A trailing backslash at EOF, inside an unclosed string, does
    # nothing (see "a trailing backslash at EOF decodes to U+FFFD" in
    # test-tokenizer.R) and so closes the same as without it
    eof('[foo="bar\\', '[foo="bar"]')
    # The unclosed string is auto-closed at parse time; the
    # pseudo-class is then rejected at translation time either way
    expect_error(css_to_xpath(':contains("foo'),
                 "The pseudo-class :contains\\(\\) is unknown")
})

test_that("a trailing backslash at EOF decodes to U+FFFD in a selector", {
    expect_equal(css_to_xpath("a\\", prefix = ""),
                 paste0("*[name() = 'a", "\uFFFD",
                        "' and namespace-uri() = '']"))
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
    # the caret points at the first '|', where the '||' the message
    # names starts, whether or not a prefix precedes it
    expect_equal(tryCatch(css_to_xpath("a||b"), error = identity)$pos, 2)
    expect_equal(tryCatch(css_to_xpath("||b"), error = identity)$pos, 1)

    # The unknown-pseudo-class error keeps the user's hyphenated
    # spelling (not the method-ised ':nth_col()')
    expect_error(css_to_xpath("e:nth-col(2)"),
                 "The pseudo-class :nth-col() is unknown",
                 fixed = TRUE)
    expect_error(css_to_xpath("e:nth-last-col(2)"),
                 "The pseudo-class :nth-last-col() is unknown",
                 fixed = TRUE)

    # Single-pipe namespace syntax is unaffected
    expect_equal(css_to_xpath("*|b", prefix = ""),
                 "*[local-name() = 'b']")
    expect_equal(css_to_xpath("|b", prefix = ""),
                 "b")
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

test_that("a misplaced pseudo-element is reported where it was written", {
    # The caret goes on the pseudo-element the message names, not on
    # whatever came after it and made it not-last
    pos <- function(css) tryCatch(css_to_xpath(css), error = identity)$pos
    expect_equal(pos("a:before:empty"), 2)
    expect_equal(pos("a::before:empty"), 2)
    expect_equal(pos("li:before a"), 3)
    expect_equal(pos(":not(:before)"), 6)
    expect_equal(pos(":is(a:before b)"), 6)
})

test_that("an invalid An+B argument is reported at the token that broke it", {
    pos <- function(css) tryCatch(css_to_xpath(css), error = identity)$pos
    # Everything before the token is a prefix some An+B expression
    # could still have grown from; the token itself is where the
    # argument left the grammar
    expect_equal(pos("e:nth-child(2n+1.5)"), 15)
    expect_equal(pos("e:nth-child(2 n)"), 15)
    expect_equal(pos("e:nth-child(odd x)"), 17)
    # An argument that is wrong from its first token, and one that is
    # merely unfinished, are both reported from the start of the
    # argument - no later token is to blame for either
    expect_equal(pos("e:nth-child(foo)"), 13)
    # '2x' is a single dimension token, name and all, so it is its
    # first token that the argument is wrong from
    expect_equal(pos("e:nth-child(2x)"), 13)
    expect_equal(pos("e:nth-child(2n+)"), 13)
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

test_that("a multi-line selector gets a position suffix instead of a gutter", {
    # A caret gutter assumes the selector is a single source line;
    # alignment would be wrong across a newline, so a multi-line
    # selector falls back to "... at position N" with no gutter block
    err <- tryCatch(parse("a\n>>b"), error = identity)
    expect_equal(err$pos, 4)
    expect_equal(conditionMessage(err),
                "Expected selector, got <DELIM '>' at 4> at position 4")
    expect_false(grepl("\n  \\|", conditionMessage(err), perl = TRUE))
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

    # a tokenize-level failure carries the class too, not just a
    # parser-level one (a trailing backslash no longer qualifies: see
    # "a trailing backslash decodes to U+FFFD" below)
    tok <- tryCatch(parse("html/body/a"), error = identity)
    expect_s3_class(tok, "selectr_parse_error")
})

test_that("a class selector must be identifier-shaped", {
    # '.5' tokenizes as a number and '.-5' as a '.' followed by one, so
    # neither reaches the ident rules; both get the same hint an ID does
    expect_error(css_to_xpath("div.5"),
                 "Invalid class selector '.5'; an identifier cannot start with a digit. Escape it: '.\\35 '", # nolint: line_length_linter.
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("div.-5"), "Escape it: '.-\\35 '",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath(".5"),
                 "Invalid class selector '.5'; ",
                 fixed = TRUE, class = "selectr_parse_error")
    # the caret points at the number the message quotes: the whole
    # '.5' token, or the '-5' that follows a '.' delimiter
    expect_equal(tryCatch(css_to_xpath("div .5"), error = identity)$pos, 5)
    expect_equal(tryCatch(css_to_xpath("div.-5"), error = identity)$pos, 5)

    # a stray number that is not a class name keeps the generic message
    expect_error(css_to_xpath("div 5"),
                 "Expected selector, got <NUMBER '5' at 5>", fixed = TRUE)
    expect_error(css_to_xpath("div.+5"),
                 "Expected ident, got <NUMBER '+5' at 5>", fixed = TRUE)

    # the escapes the messages suggest are the class names they spell
    expect_equal(css_to_xpath(".\\35 ", prefix = ""),
                 "*[contains(concat(' ', normalize-space(@class), ' '), ' 5 ')]")
    expect_equal(css_to_xpath(".-\\35 ", prefix = ""),
                 "*[contains(concat(' ', normalize-space(@class), ' '), ' -5 ')]") # nolint: line_length_linter.
})

test_that("functional pseudo-elements are rejected by name", {
    # The pseudo-element is at the end of the selector; it is the '('
    # that is unsupported, so it must not be reported as misplaced
    expect_error(css_to_xpath("::slotted(x)"),
                 "The functional pseudo-element ::slotted() is not supported",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("a::part(b)"),
                 "The functional pseudo-element ::part() is not supported",
                 fixed = TRUE, class = "selectr_parse_error")
    # the legacy single-colon spelling of a pseudo-element too
    expect_error(css_to_xpath("a:before(x)"),
                 "The functional pseudo-element ::before() is not supported",
                 fixed = TRUE, class = "selectr_parse_error")
    # the caret points at the name the message gives, not at the '('
    expect_equal(tryCatch(css_to_xpath("a::part(b)"), error = identity)$pos, 4)
    expect_equal(tryCatch(css_to_xpath("::slotted(x)"), error = identity)$pos,
                 3)
    expect_equal(tryCatch(css_to_xpath("a:before(x)"), error = identity)$pos, 3)

    # an argument-less pseudo-element still reaches the translator's
    # "not supported" message, and a misplaced one is still misplaced
    expect_error(css_to_xpath("a::before"), "Pseudo-elements are not supported",
                 fixed = TRUE, class = "selectr_translation_error")
    expect_error(css_to_xpath("a::before:empty"),
                 "Got pseudo-element ::before not at the end of a selector",
                 fixed = TRUE, class = "selectr_parse_error")
})

test_that("an unquoted numeric attribute value names the fix", {
    expect_error(css_to_xpath("[data-id=1]"),
                 "Attribute values must be quoted unless they are identifiers: write [data-id=\"1\"]", # nolint: line_length_linter.
                 fixed = TRUE, class = "selectr_parse_error")
    # the operator and any namespace prefix are echoed back as given
    expect_error(css_to_xpath("a[x|data-id^=1.5]"),
                 "write [x|data-id^=\"1.5\"]",
                 fixed = TRUE, class = "selectr_parse_error")
    # the caret points at the value
    expect_equal(tryCatch(css_to_xpath("[data-id=1]"), error = identity)$pos,
                 10)

    # a non-numeric value that is neither a string nor an ident keeps
    # the general message
    expect_error(css_to_xpath("[foo=#]"),
                 "Expected string or ident, got <DELIM '#' at 6>", fixed = TRUE)

    # the quoted form the message suggests is what it says it is
    expect_equal(css_to_xpath("[data-id=\"1\"]", prefix = ""),
                 "*[@data-id = '1']")
})

test_that("an ID selector must be identifier-shaped", {
    # Selectors requires the hash of an ID selector to be of type "id",
    # i.e. its name must start an identifier. Browsers throw a
    # SyntaxError for document.querySelector("#1").
    expect_error(css_to_xpath("#1"),
                 "Invalid ID selector '#1'; an identifier ",
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

test_that("a '-' that names nothing is rejected rather than read as a name", {
    # An element cannot be named '-', so reading one as a name never
    # selected anything; the selectors below used to translate to
    # expressions that matched no node, and now say what is wrong.
    expect_error(css_to_xpath("-"),
                 "Expected selector, got <DELIM '-' at 1>",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath(".-"),
                 "Expected ident, got <DELIM '-' at 2>",
                 fixed = TRUE, class = "selectr_parse_error")
    # a combinator written with spaces around it, but never finished
    expect_error(css_to_xpath("a - b"),
                 "Expected selector, got <DELIM '-' at 3>",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("a:not(-)"),
                 "Expected selector, got <DELIM '-' at 7>",
                 fixed = TRUE, class = "selectr_parse_error")

    # A CDC and a CDO are each rejected whole, at the position they
    # start; '-->' used to parse as the element '--' followed by a
    # child combinator, and '<!--' failed on its first character
    expect_error(css_to_xpath("-->a"),
                 "Expected selector, got <CDC '-->' at 1>",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_error(css_to_xpath("<!--a"),
                 "Expected selector, got <CDO '<!--' at 1>",
                 fixed = TRUE, class = "selectr_parse_error")
    expect_equal(tryCatch(css_to_xpath("div -->a"), error = identity)$pos, 5)

    # Names that do start an identifier are still names, escaped ones
    # included: '\-' is the element '-', not a delimiter
    expect_equal(css_to_xpath("-a", prefix = ""),
                 "*[name() = '-a' and namespace-uri() = '']")
    expect_equal(css_to_xpath("--a", prefix = ""),
                 "*[name() = '--a' and namespace-uri() = '']")
    expect_equal(css_to_xpath("\\-", prefix = ""),
                 "*[name() = '-' and namespace-uri() = '']")
    expect_equal(css_to_xpath(".-a", prefix = ""),
                 "*[contains(concat(' ', normalize-space(@class), ' '), ' -a ')]")
})
