context("tokenizer")

test_that("parser creates correct specificity", {
    spec <- function(css) {
        selectors <- parse(css)
        if (length(selectors) != 1)
            stop("More than one result attempting to be parsed.")
        selectors[[1]]$specificity()
    }

    expect_that(spec("*"), equals(rep(0, 3)))
    expect_that(spec(" foo"), equals(c(0, 0, 1)))
    expect_that(spec(":empty"), equals(c(0, 1, 0)))
    expect_that(spec(":before"), equals(c(0, 0, 1)))
    expect_that(spec("*:before"), equals(c(0, 0, 1)))
    expect_that(spec(":nth-child(2)"), equals(c(0, 1, 0)))
    expect_that(spec(".bar"), equals(c(0, 1, 0)))
    expect_that(spec("[baz]"), equals(c(0, 1, 0)))
    expect_that(spec('[baz="4"]'), equals(c(0, 1, 0)))
    expect_that(spec('[baz^="4"]'), equals(c(0, 1, 0)))
    expect_that(spec("#lipsum"), equals(c(1, 0, 0)))

    expect_that(spec(":not(*)"), equals(c(0, 0, 0)))
    expect_that(spec(":not(foo)"), equals(c(0, 0, 1)))
    expect_that(spec(":not(.foo)"), equals(c(0, 1, 0)))
    expect_that(spec(":not([foo])"), equals(c(0, 1, 0)))
    expect_that(spec(":not(:empty)"), equals(c(0, 1, 0)))
    expect_that(spec(":not(#foo)"), equals(c(1, 0, 0)))

    expect_that(spec("foo:empty"), equals(c(0, 1, 1)))
    expect_that(spec("foo:before"), equals(c(0, 0, 2)))
    expect_that(spec("foo::before"), equals(c(0, 0, 2)))
    expect_that(spec("foo:empty::before"), equals(c(0, 1, 2)))

    # combinations
    expect_that(spec("* foo"), equals(c(0, 0, 1)))
    expect_that(spec("foo :empty"), equals(c(0, 1, 1)))
    expect_that(spec(":empty :before"), equals(c(0, 1, 1)))
    expect_that(spec(".bar [baz]"), equals(c(0, 2, 0)))
    expect_that(spec('[baz] [baz="4"]'), equals(c(0, 2, 0)))
    expect_that(spec('[baz="4"] [baz^="4"]'), equals(c(0, 2, 0)))
    expect_that(spec('[baz^="4"] #lipsum'), equals(c(1, 1, 0)))
})
