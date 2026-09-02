test_that("quote characters are escaped", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_equal(css('*[aval="\'"]'),
                 'descendant-or-self::*[@aval = "\'"]')
    expect_equal(css('*[aval="\'\'\'"]'),
                 "descendant-or-self::*[@aval = \"'''\"]")
    expect_equal(css('*[aval=\'"\']'),
                 "descendant-or-self::*[@aval = '\"']")
    expect_equal(css('*[aval=\'"""\']'),
                 "descendant-or-self::*[@aval = '\"\"\"']")
    expect_equal(css('*[aval=\'"\\\'"\']'),
                 "descendant-or-self::*[@aval = concat('\"',\"'\",'\"')]")
})

test_that("empty attribute values are quoted", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_equal(xpath_literal(""), "''")
    expect_equal(css('*[aval=""]'),
                 "descendant-or-self::*[@aval = '']")
    expect_equal(css('*[aval|=""]'),
                 paste0("descendant-or-self::*[@aval and ",
                        "(@aval = '' or starts-with(@aval, '-'))]"))
    # These operators can never match an empty value
    expect_equal(css('*[aval~=""]'),
                 "descendant-or-self::*[0]")
    expect_equal(css('*[aval^=""]'),
                 "descendant-or-self::*[0]")
    expect_equal(css('*[aval$=""]'),
                 "descendant-or-self::*[0]")
    expect_equal(css('*[aval*=""]'),
                 "descendant-or-self::*[0]")
})
