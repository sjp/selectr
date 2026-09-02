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
                 "descendant-or-self::*[@aval = '' or starts-with(@aval, '-')]")
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

test_that("attribute existence guards are safely omitted", {
    # xpath_attrib_{includes,dashmatch,prefixmatch,suffixmatch,
    # substringmatch}() no longer emit a redundant '@attr and ...'
    # existence guard (R/xpath.R): with no such attribute, each
    # underlying XPath test is already false. Confirm the guarded and
    # unguarded forms select exactly the same nodes, on a document with
    # an element missing the attribute, one with it empty, and one with
    # a near-miss value
    skip_if_not_installed("xml2")
    library(xml2)

    doc <- read_xml(paste0(
        "<r>",
        "<a/>",
        "<b foo=''/>",
        "<c foo='bar-x'/>",
        "<d foo='bar'/>",
        "<e foo=' bar baz '/>",
        "</r>"))

    match_names <- function(css) {
        xml_name(xml_find_all(doc, css_to_xpath(css)))
    }

    expect_equal(match_names("[foo^=bar]"), c("c", "d"))
    expect_equal(match_names("[foo*=bar]"), c("c", "d", "e"))
    expect_equal(match_names("[foo$=bar]"), "d")
    expect_equal(match_names("[foo~=bar]"), c("d", "e"))
    expect_equal(match_names("[foo|=bar]"), c("c", "d"))
    # The dash-match guard is redundant even for an empty value
    expect_equal(match_names("[foo|='']"), "b")

    # An 'and'-joined dash-match keeps the right precedence without its
    # own parentheses (is_or_group)
    doc2 <- read_xml("<r><m foo='bar' bar='1'/><n foo='bar'/></r>")
    match_names2 <- function(css) {
        xml_name(xml_find_all(doc2, css_to_xpath(css)))
    }
    expect_equal(match_names2("[foo|=bar][bar]"), "m")
    expect_equal(match_names2("[bar][foo|=bar]"), "m")
})
