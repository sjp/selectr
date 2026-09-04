test_that("parser generates correct series", {
    # An+B is validated in the parser, so an invalid series never
    # reaches parse_series() through parse(); tokenize the argument on
    # its own to exercise parse_series() directly. The tokens are the
    # ones the parser would collect, minus the trailing EOF.
    series <- function(css) {
        tokens <- Filter(function(token) token$type != "EOF", tokenize(css))
        parse_series(tokens)
    }

    expect_equal(series("1n+3"), c(1, 3))
    expect_equal(series("1n +3"), c(1, 3))
    expect_equal(series("1n + 3"), c(1, 3))
    expect_equal(series("1n+ 3"), c(1, 3))
    expect_equal(series("1n-3"), c(1, -3))
    expect_equal(series("1n -3"), c(1, -3))
    expect_equal(series("1n - 3"), c(1, -3))
    expect_equal(series("1n- 3"), c(1, -3))
    expect_equal(series("n-5"), c(1, -5))
    expect_equal(series("odd"), c(2, 1))
    expect_equal(series("even"), c(2, 0))
    expect_equal(series("3n"), c(3, 0))
    expect_equal(series("n"), c(1, 0))
    expect_equal(series("+n"), c(1, 0))
    expect_equal(series("-n"), c(-1, 0))
    expect_equal(series("5"), c(0, 5))
    expect_equal(series("foo"), NULL)
    expect_equal(series("n+"), NULL)
})

test_that("series are parsed case-insensitively", {
    xpath <- function(css) css_to_xpath(paste0("e:nth-child(", css, ")"))

    expect_equal(xpath("2N"), xpath("2n"))
    expect_equal(xpath("ODD"), xpath("odd"))
    expect_equal(xpath("EVEN"), xpath("even"))
    expect_equal(xpath("Odd"), xpath("odd"))
    expect_equal(xpath("eVen"), xpath("even"))
    expect_equal(xpath("N"), xpath("n"))
    expect_equal(xpath("N+1"), xpath("n+1"))
    expect_equal(xpath("-N+3"), xpath("-n+3"))
    expect_equal(xpath("2N+1"), xpath("2n+1"))
    expect_equal(css_to_xpath("e:nth-last-of-type(2N)"),
                 css_to_xpath("e:nth-last-of-type(2n)"))

    # Genuinely invalid input must still error
    expect_error(css_to_xpath("e:nth-child(2x)"))
    expect_error(css_to_xpath("e:nth-child(odds)"))
    expect_error(css_to_xpath("e:nth-child(m+1)"))
})

test_that("whitespace is only permitted around the sign before B", {
    # spec-legal placements keep working
    expect_equal(css_to_xpath("e:nth-child(2n + 1)"),
                 css_to_xpath("e:nth-child(2n+1)"))
    expect_equal(css_to_xpath("e:nth-child(2n +1)"),
                 css_to_xpath("e:nth-child(2n+1)"))
    expect_equal(css_to_xpath("e:nth-child(n+ 1)"),
                 css_to_xpath("e:nth-child(n+1)"))
    expect_equal(css_to_xpath("e:nth-child( 2n+1 )"),
                 css_to_xpath("e:nth-child(2n+1)"))
    # whitespace anywhere else is invalid (css-syntax-3 An+B grammar)
    expect_error(css_to_xpath("e:nth-child(3 7)"))
    expect_error(css_to_xpath("e:nth-child(2 n)"))
    expect_error(css_to_xpath("e:nth-child(2n 1)"))
    expect_error(css_to_xpath("e:nth-child(2n+1 3)"))
    expect_error(css_to_xpath("e:nth-child(2 n + 1)"))
    expect_error(css_to_xpath("e:nth-child(- n)"))
    expect_error(css_to_xpath("e:nth-child(+ 2n)"))
    expect_error(css_to_xpath("e:nth-child(o dd)"))
})

test_that("non-integer A and B values are rejected", {
    # An+B takes <integer> values only; these must not be truncated
    expect_error(css_to_xpath("e:nth-child(2.5)"))
    expect_error(css_to_xpath("e:nth-child(1.9)"))
    expect_error(css_to_xpath("e:nth-child(2e1)"))
    expect_error(css_to_xpath("e:nth-child(2.5n+1)"))
    expect_error(css_to_xpath("e:nth-child(2n+1.5)"))
    # signed integers and leading zeros remain valid
    expect_equal(css_to_xpath("e:nth-child(+05)"),
                 css_to_xpath("e:nth-child(5)"))
})

test_that("an invalid An+B argument is rejected at parse time", {
    err <- function(css) {
        tryCatch(css_to_xpath(css), error = function(e) conditionMessage(e))
    }

    # the message names the pseudo-class that was written, not nth-child
    expect_match(err("li:nth-last-of-type(foo)"),
                 "^Invalid An\\+B expression in :nth-last-of-type\\(\\): 'foo'")
    expect_match(err("li:nth-of-type(2n+)"),
                 "^Invalid An\\+B expression in :nth-of-type\\(\\): '2n\\+'")
    expect_match(err("li:nth-last-child(o dd)"),
                 "^Invalid An\\+B expression in :nth-last-child\\(\\): 'o dd'")
    # the name is canonicalised to lower case, as ":... is unknown" is
    expect_match(err("li:NTH-CHILD(foo)"),
                 "^Invalid An\\+B expression in :nth-child\\(\\): 'foo'")

    # 'of' is only part of the :nth-child()/:nth-last-child() grammar,
    # and the message says so instead of quoting the whole argument
    expect_match(err("li:nth-of-type(2 of li)"),
                 paste0("^Invalid An\\+B expression in :nth-of-type\\(\\): ",
                        "'of' is only allowed in :nth-child\\(\\) and ",
                        ":nth-last-child\\(\\)"))
    expect_match(err("li:nth-last-of-type(2 OF li)"),
                 "'of' is only allowed in :nth-child\\(\\)")

    # a quoted argument is named as such rather than reported by the
    # generic "not allowed in series" message the translator used to give
    expect_match(err(":nth-child('2')"),
                 paste0("^Invalid An\\+B expression in :nth-child\\(\\): ",
                        "a quoted string is not allowed"))

    # An empty argument list is still an argument-count error
    expect_match(err(":nth-child()"),
                 "^Expected at least one argument, got <DELIM '\\)' at 12>")
})

test_that("an A or B beyond the integer range is saturated", {
    series <- function(css) {
        tokens <- Filter(function(token) token$type != "EOF", tokenize(css))
        parse_series(tokens)
    }
    imax <- .Machine$integer.max

    # The An+B grammar has no upper bound, so a value R cannot hold as
    # an integer is clamped to .Machine$integer.max rather than
    # rejected: no document has that many siblings, so the clamped
    # series selects exactly what the written one would
    expect_equal(series("2147483648"), c(0, imax))
    expect_equal(series("99999999999"), c(0, imax))
    expect_equal(series("-99999999999"), c(0, -imax))
    expect_equal(series("99999999999n+1"), c(imax, 1))
    expect_equal(series("n+99999999999"), c(1, imax))
    expect_equal(series("-99999999999n-99999999999"), c(-imax, -imax))

    # and the selector translates, rather than erroring
    expect_equal(css_to_xpath(":nth-child(99999999999)"),
                 css_to_xpath(paste0(":nth-child(", imax, ")")))
    expect_equal(css_to_xpath(":nth-child(1000000000000)"),
                 css_to_xpath(paste0(":nth-child(", imax, ")")))
    expect_equal(css_to_xpath("a:nth-child(4294967296n)"),
                 css_to_xpath(paste0("a:nth-child(", imax, "n)")))

    # a huge B still counts down to B-1 without overflowing to NA, in
    # either direction
    expect_equal(css_to_xpath(":nth-child(99999999999)"),
                 paste0("descendant-or-self::*[count(preceding-sibling::*) = ",
                        imax - 1, "]"))
    expect_equal(css_to_xpath(":nth-child(-99999999999)"),
                 "descendant-or-self::*[0]")
    expect_equal(css_to_xpath(":nth-child(n-99999999999)"),
                 "descendant-or-self::*")
    expect_equal(css_to_xpath(":nth-child(-n+99999999999)"),
                 paste0("descendant-or-self::*[count(preceding-sibling::*) <= ",
                        imax - 1, "]"))

    # the saturated value is written out in full, not in E notation
    expect_false(grepl("e+", css_to_xpath(":nth-child(99999999999n+1)"),
                       fixed = TRUE))
})

test_that("An+B errors carry a source position", {
    err <- function(css) {
        tryCatch(css_to_xpath(css), error = function(e) conditionMessage(e))
    }

    # the caret sits under the start of the series ...
    expect_match(err("li:nth-child(foo)"),
                 "\n  \\|\n  \\| li:nth-child\\(foo\\)\n  \\|              \\^",
                 perl = TRUE)
    # ... under the offending string ...
    expect_match(err("li:nth-child('2')"),
                 "\n  \\|\n  \\| li:nth-child\\('2'\\)\n  \\|              \\^",
                 perl = TRUE)
    # ... and under the misplaced 'of' keyword
    expect_match(err("li:nth-of-type(2 of li)"),
                 "\n  \\| li:nth-of-type\\(2 of li\\)\n  \\|                  \\^",
                 perl = TRUE)

    # and, being parse errors now, they are structured conditions like
    # every other syntax error
    e <- tryCatch(css_to_xpath("li:nth-child(foo)"), error = identity)
    expect_s3_class(e, "selectr_parse_error")
    expect_equal(e$pos, 14)
    expect_equal(e$selector, "li:nth-child(foo)")
})
