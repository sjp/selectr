test_that("querySelector returns a single node or NULL", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        if (is.null(x)) x else as.character(x)
    }
    expect_equal(p(querySelector(doc, "a")),
                 p(xml_find_first(doc, "//a")))
    expect_equal(p(querySelector(doc, "*", prefix = "")),
                 p(xml_find_first(doc, "*")))
    expect_equal(p(querySelector(doc, "d")), NULL)
    expect_equal(p(querySelector(doc, "c")), p(xml_find_first(doc, "//c")))
})

test_that("querySelectorAll returns expected nodes", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }
    expect_equal(p(querySelectorAll(doc, "a")),
                 p(xml_find_all(doc, "//a")))
    expect_equal(p(querySelectorAll(doc, "*", prefix = "")),
                 p(xml_find_all(doc, "*")))
    expect_equal(p(querySelectorAll(doc, "c")),
                 p(xml_find_all(doc, "//c")))
})

test_that("querySelectorAll returns empty list for no match", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }
    expect_equal(p(querySelectorAll(doc, "d")),
                 p(xml_find_all(doc, "//d")))
})

test_that("querySelector handles namespaces", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>')
    p <- function(x) {
        if (is.null(x)) x else as.character(x)
    }

    expect_equal(querySelector(doc, "circle"), NULL)
    expect_equal(querySelector(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg")),
                 NULL)
    expect_equal(p(querySelector(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))[[1]]))
    # a named list is also accepted, consistent with the 'XML' methods
    expect_equal(p(querySelector(doc, "svg|circle", ns = list(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))[[1]]))

    # now with querySelectorNS
    expect_equal(querySelectorNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg")), NULL)
    expect_equal(p(querySelectorNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))[[1]]))
})

test_that("querySelectorAll handles namespaces", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }

    expect_equal(p(querySelectorAll(doc, "circle")),
                 p(xml_find_all(doc, "//circle")))
    expect_equal(p(querySelectorAll(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//circle", ns = c(svg = "http://www.w3.org/2000/svg"))))
    expect_equal(p(querySelectorAll(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))))
    # a named list is also accepted, consistent with the 'XML' methods
    expect_equal(p(querySelectorAll(doc, "svg|circle", ns = list(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))))

    # now with querySelectorAllNS
    expect_equal(p(querySelectorAllNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//circle", ns = c(svg = "http://www.w3.org/2000/svg"))))
    expect_equal(p(querySelectorAllNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                 p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))))
})

test_that("querySelectorAll honours attribute case-sensitivity flags", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<r><a rel="NoFollow"/><a rel="nofollow"/><a rel="other"/></r>')
    rels <- function(css) {
        unlist(lapply(querySelectorAll(doc, css), xml_attr, "rel"))
    }

    expect_equal(rels('a[rel="nofollow"]'), "nofollow")
    expect_equal(rels('a[rel="nofollow" i]'),
                 c("NoFollow", "nofollow"))
    expect_equal(rels('a[rel="NOFOLLOW" i]'),
                 c("NoFollow", "nofollow"))
    expect_equal(rels('a[rel="nofollow" s]'), "nofollow")
    expect_equal(rels('a[rel^="NO" i]'), c("NoFollow", "nofollow"))
    expect_equal(rels('a[rel$="LOW" i]'), c("NoFollow", "nofollow"))
    expect_equal(rels('a[rel*="FOLL" i]'), c("NoFollow", "nofollow"))
})

test_that("the namespaced queries are scoped to the node given", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml(paste0('<root xmlns:s="urn:s"><s:a id="outer"/>',
                           '<wrap><s:a id="inner"/></wrap></root>'))
    ns <- c(s = "urn:s")
    wrap <- xml_find_first(doc, "//wrap")

    # the namespace filter must not escape the queried node: the
    # 'outer' element is not inside <wrap>
    expect_equal(xml_attr(querySelectorAllNS(wrap, "s|a", ns), "id"),
                 "inner")
    expect_equal(xml_attr(querySelectorNS(wrap, "s|a", ns), "id"),
                 "inner")
    # which is what the plain query with a namespace already does
    expect_equal(xml_attr(querySelectorAll(wrap, "s|a", ns = ns), "id"),
                 "inner")

    # a node set is scoped the same way, node by node
    expect_equal(xml_attr(querySelectorAllNS(xml_find_all(doc, "//wrap"),
                                             "s|a", ns), "id"),
                 "inner")

    # from the document both are still found: the 'descendant-or-self'
    # axis from the document node includes the root
    expect_equal(xml_attr(querySelectorAllNS(doc, "s|a", ns), "id"),
                 c("outer", "inner"))
})

test_that("querySelector methods handle invalid arguments", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')

    selector_error <- "A valid selector (single character string) must be provided."
    expect_error(querySelector(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorAll(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorNS(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc), selector_error, fixed = TRUE)

    expect_error(querySelector(doc, c("a", "b")), selector_error, fixed = TRUE)
    expect_error(querySelectorAll(doc, c("a", "b")), selector_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, c("a", "b"), c(svg = "http://www.w3.org/2000/svg")), selector_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, c("a", "b"), c(svg = "http://www.w3.org/2000/svg")), selector_error, fixed = TRUE)
    expect_error(querySelector(doc, 1), selector_error, fixed = TRUE)
    expect_error(querySelector(doc, character(0)), selector_error, fixed = TRUE)
    expect_error(querySelector(doc, NA_character_), selector_error, fixed = TRUE)

    # invalid namespace objects are rejected, consistent with the 'XML' methods
    ns_object_error <- "A namespace object must be either a named list or a named character vector."
    expect_error(querySelector(doc, "a", ns = 1), ns_object_error, fixed = TRUE)
    expect_error(querySelectorAll(doc, "a", ns = 1), ns_object_error, fixed = TRUE)
    expect_error(querySelector(doc, "a", ns = list("x")), "The namespace object must be a named list or character vector", fixed = TRUE)
    expect_error(querySelectorAll(doc, "a", ns = c("x")), "The namespace object must be a named list or character vector", fixed = TRUE)

    namespace_error <- "A namespace must be provided"
    expect_error(querySelectorNS(doc, "a"), namespace_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", NULL), namespace_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", character(0)), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a"), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", NULL), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", character(0)), namespace_error, fixed = TRUE)
})

test_that("querySelector returns the first node querySelectorAll finds", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml(paste0('<r xmlns:s="http://www.w3.org/2000/svg">',
                           '<b id="b1"/><a id="a1"/>',
                           '<w id="w1"><b id="b2"/><s:a id="s1"/></w>',
                           '<w id="w2"><a id="a2"/><s:a id="s2"/></w>',
                           '</r>'))
    ns <- c(s = "http://www.w3.org/2000/svg")
    id <- function(node) if (is.null(node)) NULL else xml_attr(node, "id")
    first <- function(nodes) if (length(nodes)) nodes[[1]] else NULL

    nodes <- xml_find_all(doc, "//w")
    # A grouped selector is a union, whose first node is the first in
    # document order rather than the first branch's first match.
    selectors <- c("a", "b", "a, b", "b, a", "w > a", ":scope > a", "z")
    for (selector in selectors) {
        for (obj in list(doc, nodes)) {
            expect_identical(querySelector(obj, selector),
                             first(querySelectorAll(obj, selector)))
        }
    }
    for (selector in c(selectors, "s|a", "s|a, b")) {
        for (obj in list(doc, nodes)) {
            expect_identical(querySelectorNS(obj, selector, ns),
                             first(querySelectorAllNS(obj, selector, ns)))
        }
    }

    expect_equal(id(querySelector(doc, "a, b")), "b1")
    expect_equal(id(querySelector(nodes, "a")), "a2")
    expect_equal(id(querySelectorNS(doc, "s|a", ns)), "s1")
    expect_null(querySelector(xml_find_all(doc, "//nope"), "a"))
})
