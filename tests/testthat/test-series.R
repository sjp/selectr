context("series")

test_that("parser generates correct series", {
    series <- function(css) {
        selector <- parse(paste0(":nth-child(", css, ")"))[[1]]
        args <- selector$parsed_tree$arguments
        parse_series(args)
    }

    expect_that(series("1n+3"), equals(c(1, 3)))
    expect_that(series("1n +3"), equals(c(1, 3)))
    expect_that(series("1n + 3"), equals(c(1, 3)))
    expect_that(series("1n+ 3"), equals(c(1, 3)))
    expect_that(series("1n-3"), equals(c(1, -3)))
    expect_that(series("1n -3"), equals(c(1, -3)))
    expect_that(series("1n - 3"), equals(c(1, -3)))
    expect_that(series("1n- 3"), equals(c(1, -3)))
    expect_that(series("n-5"), equals(c(1, -5)))
    expect_that(series("odd"), equals(c(2, 1)))
    expect_that(series("even"), equals(c(2, 0)))
    expect_that(series("3n"), equals(c(3, 0)))
    expect_that(series("n"), equals(c(1, 0)))
    expect_that(series("+n"), equals(c(1, 0)))
    expect_that(series("-n"), equals(c(-1, 0)))
    expect_that(series("5"), equals(c(0, 5)))
    expect_that(series("foo"), equals(NULL))
    expect_that(series("n+"), equals(NULL))
})

test_that("series are parsed case-insensitively", {
    xpath <- function(css) css_to_xpath(paste0("e:nth-child(", css, ")"))

    expect_that(xpath("2N"), equals(xpath("2n")))
    expect_that(xpath("ODD"), equals(xpath("odd")))
    expect_that(xpath("EVEN"), equals(xpath("even")))
    expect_that(xpath("Odd"), equals(xpath("odd")))
    expect_that(xpath("eVen"), equals(xpath("even")))
    expect_that(xpath("N"), equals(xpath("n")))
    expect_that(xpath("N+1"), equals(xpath("n+1")))
    expect_that(xpath("-N+3"), equals(xpath("-n+3")))
    expect_that(xpath("2N+1"), equals(xpath("2n+1")))
    expect_that(css_to_xpath("e:nth-last-of-type(2N)"),
                equals(css_to_xpath("e:nth-last-of-type(2n)")))

    # Genuinely invalid input must still error
    expect_error(css_to_xpath("e:nth-child(2x)"))
    expect_error(css_to_xpath("e:nth-child(odds)"))
    expect_error(css_to_xpath("e:nth-child(m+1)"))
})

test_that("whitespace is only permitted around the sign before B", {
    # spec-legal placements keep working
    expect_that(css_to_xpath("e:nth-child(2n + 1)"),
                equals(css_to_xpath("e:nth-child(2n+1)")))
    expect_that(css_to_xpath("e:nth-child(2n +1)"),
                equals(css_to_xpath("e:nth-child(2n+1)")))
    expect_that(css_to_xpath("e:nth-child(n+ 1)"),
                equals(css_to_xpath("e:nth-child(n+1)")))
    expect_that(css_to_xpath("e:nth-child( 2n+1 )"),
                equals(css_to_xpath("e:nth-child(2n+1)")))
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
