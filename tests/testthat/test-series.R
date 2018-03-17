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
