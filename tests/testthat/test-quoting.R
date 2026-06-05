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

test_that("empty attribute values are quoted", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_that(xpath_literal(""), equals("''"))
    expect_that(css('*[aval=""]'),
                equals("descendant-or-self::*[@aval = '']"))
    expect_that(css('*[aval|=""]'),
                equals(paste0("descendant-or-self::*[@aval and ",
                              "(@aval = '' or starts-with(@aval, '-'))]")))
    # These operators can never match an empty value
    expect_that(css('*[aval~=""]'),
                equals("descendant-or-self::*[0]"))
    expect_that(css('*[aval^=""]'),
                equals("descendant-or-self::*[0]"))
    expect_that(css('*[aval$=""]'),
                equals("descendant-or-self::*[0]"))
    expect_that(css('*[aval*=""]'),
                equals("descendant-or-self::*[0]"))
})
