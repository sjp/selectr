context("namespaces")

test_that("namespace selectors translate faithfully", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # '*|e' matches 'e' in any namespace, including none
    expect_that(xpath("*|e"), equals("*[local-name() = 'e']"))
    # '|e' matches 'e' in no namespace, which is what an unprefixed
    # XPath name test already means
    expect_that(xpath("|e"), equals("e"))
    # '|e' with a name unusable as an XPath name test must still pin the
    # null namespace: a bare name() test is also unprefixed for an
    # element in a default namespace
    expect_that(xpath("|é"),
                equals("*[namespace-uri() = '' and local-name() = 'é']"))
    # '*|*' is equivalent to '*'
    expect_that(xpath("*|*"), equals("*"))
    # '|*' matches any element in no namespace
    expect_that(xpath("|*"), equals("*[namespace-uri() = '']"))
    # 'ns|e' defers prefix-to-URI binding to evaluation time
    expect_that(xpath("ns|e"), equals("ns:e"))

    # Attribute selectors
    expect_that(xpath("[*|a]"), equals("*[@*[local-name() = 'a']]"))
    expect_that(xpath("[*|a='v']"),
                equals("*[@*[local-name() = 'a'] = 'v']"))
    # Unprefixed attribute names have no namespace, so '[|a]' is
    # equivalent to '[a]'
    expect_that(xpath("[|a]"), equals("*[@a]"))
    expect_that(xpath("[|a='v']"), equals("*[@a = 'v']"))
    expect_that(xpath("[ns|a]"), equals("*[@ns:a]"))

    # Composability
    expect_that(xpath(":not(*|e)"), equals("*[not(local-name() = 'e')]"))
    expect_that(xpath("div > *|e"), equals("div/*[local-name() = 'e']"))

    # Inside pseudo-class arguments, prefixed names keep resolving
    # through the namespace map (a name test on the self axis or the
    # path step itself), rather than comparing against the document's
    # literal prefix with name()
    expect_that(xpath(":is(ns|e)"), equals("*[self::ns:e]"))
    expect_that(xpath(":not(ns|e)"), equals("*[not(self::ns:e)]"))
    expect_that(xpath(":has(ns|e)"), equals("*[.//ns:e]"))
    expect_that(xpath(":has(> ns|e)"), equals("*[child::ns:e]"))
    # Under '+' the position predicate [1] must precede the name test
    # ("the next sibling, if it is an ns:e"), so the name cannot stay
    # on the path step
    expect_that(xpath(":has(+ ns|e)"),
                equals("*[following-sibling::*[1][self::ns:e]]"))

    # 'ns|*' is a node test too ('*' is a valid local part), not an
    # unsafe name: stringifying it as name() = 'ns:*' could never
    # match, as name() never returns a literal '*'
    expect_that(xpath(":is(ns|*)"), equals("*[self::ns:*]"))
    expect_that(xpath(":not(ns|*)"), equals("*[not(self::ns:*)]"))
    expect_that(xpath(":has(ns|*)"), equals("*[.//ns:*]"))
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

test_that("namespaced pseudo-class arguments match by URI, not prefix", {
    skip_if_not_installed("xml2")

    # the document binds the SVG namespace to 's', the query to 'svg':
    # matching must go through the namespace map (URI), not compare
    # qualified names as strings
    doc <- xml2::read_xml(paste0(
        '<r xmlns:s="http://www.w3.org/2000/svg">',
        '<s:g id="g1"/><b id="b1"/></r>'))
    ns <- c(svg = "http://www.w3.org/2000/svg")
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("svg|g"), equals("g1"))
    expect_that(ids(":is(svg|g)"), equals("g1"))
    expect_that(ids(":not(svg|g)"), equals(c(NA, "b1"))) # r and b
    expect_that(ids(":is(svg|*)"), equals("g1"))
    expect_that(ids(":not(svg|*)"), equals(c(NA, "b1"))) # r and b
    expect_that(xml2::xml_name(xml2::xml_find_all(
                    doc, css_to_xpath(":has(svg|g)", prefix = "//"), ns)),
                equals("r"))
})

test_that("unprefixed pseudo-class argument names match default namespaces", {
    skip_if_not_installed("xml2")

    # An unprefixed name inside a pseudo-class argument compares
    # against name(), so it also matches an unprefixed element in a
    # default namespace — unlike a top-level bare name, whose XPath
    # name test matches in no namespace only
    doc <- xml2::read_xml(paste0(
        '<r id="root"><p id="plain"/>',
        '<x xmlns="http://d" id="wrapper"><p id="defaulted"/></x></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("p"), equals("plain"))
    expect_that(ids(":is(p)"), equals(c("plain", "defaulted")))
    expect_that(ids(":has(p)"), equals(c("root", "wrapper")))
})

test_that("'|e' with an unsafe name does not match a default namespace", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r><é id="plain"/>',
        '<w><é xmlns="http://default" id="defaulted"/></w></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("|é"), equals("plain"))
    expect_that(ids("|é:first-of-type"), equals("plain"))
})
