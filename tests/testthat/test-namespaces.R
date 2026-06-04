context("namespaces")

test_that("namespace selectors translate faithfully", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # '*|e' matches 'e' in any namespace, including none
    expect_that(xpath("*|e"), equals("*[(local-name() = 'e')]"))
    # '|e' matches 'e' in no namespace, which is what an unprefixed
    # XPath name test already means
    expect_that(xpath("|e"), equals("e"))
    # '*|*' is equivalent to '*'
    expect_that(xpath("*|*"), equals("*"))
    # '|*' matches any element in no namespace
    expect_that(xpath("|*"), equals("*[(namespace-uri() = '')]"))
    # 'ns|e' defers prefix-to-URI binding to evaluation time
    expect_that(xpath("ns|e"), equals("ns:e"))

    # Attribute selectors
    expect_that(xpath("[*|a]"), equals("*[(@*[local-name() = 'a'])]"))
    expect_that(xpath("[*|a='v']"),
                equals("*[(@*[local-name() = 'a'] = 'v')]"))
    # Unprefixed attribute names have no namespace, so '[|a]' is
    # equivalent to '[a]'
    expect_that(xpath("[|a]"), equals("*[(@a)]"))
    expect_that(xpath("[|a='v']"), equals("*[(@a = 'v')]"))
    expect_that(xpath("[ns|a]"), equals("*[(@ns:a)]"))

    # Composability
    expect_that(xpath(":not(*|e)"), equals("*[(not((local-name() = 'e')))]"))
    expect_that(xpath("div > *|e"), equals("div/*[(local-name() = 'e')]"))
})

test_that("namespace selector specificity is correct", {
    spec <- function(css) parse(css)[[1]]$specificity()

    # Universal selectors and namespace components contribute nothing
    expect_that(spec("*|e"), equals(c(0, 0, 1)))
    expect_that(spec("|e"), equals(c(0, 0, 1)))
    expect_that(spec("*|*"), equals(c(0, 0, 0)))
    expect_that(spec("|*"), equals(c(0, 0, 0)))
})

test_that("malformed namespace selectors are rejected", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_error(css("e|"), "Expected ident or '\\*'")
    expect_error(css("a||b"), "Expected ident or '\\*'")
    expect_error(css("div .|x"), "Expected ident")
})

test_that("namespace selectors match correct elements", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r xmlns:svg="http://www.w3.org/2000/svg" a="x">',
        '<e>plain</e><svg:e svg:a="y">svg</svg:e></r>'))
    ns <- xml2::xml_ns(doc)
    matches <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_name(nodes, ns)
    }

    expect_that(matches("*|e"), equals(c("e", "svg:e")))
    expect_that(matches("|e"), equals("e"))
    expect_that(matches("|*"), equals(c("r", "e")))
    expect_that(matches("*|*"), equals(c("r", "e", "svg:e")))
    expect_that(matches("svg|e"), equals("svg:e"))
    expect_that(matches("[*|a]"), equals(c("r", "svg:e")))
    expect_that(matches("[|a]"), equals("r"))
})
