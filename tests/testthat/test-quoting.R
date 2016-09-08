context("quoting")

test_that("quote characters are escaped", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_that(css('*[aval="\'"]'),
                equals('descendant-or-self::*[@aval = "\'"]'))
    expect_that(css('*[aval="\'\'\'"]'),
                equals("descendant-or-self::*[@aval = \"'''\"]"))
    expect_that(css('*[aval=\'"\']'),
                equals("descendant-or-self::*[@aval = '\"']"))
    expect_that(css('*[aval=\'"""\']'),
                equals("descendant-or-self::*[@aval = '\"\"\"']"))
    expect_that(css('*[aval=\'"\\\'"\']'),
                equals("descendant-or-self::*[@aval = concat('\"',\"'\",'\"')]"))
})
