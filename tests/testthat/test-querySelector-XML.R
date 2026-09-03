test_that("querySelector returns a single node or NULL", {
    skip_if_not_installed("XML")
    doc <- XML::xmlRoot(XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        if (is.null(x))
            return(x)
        XML::saveXML(x, file = NULL)
    }
    expect_equal(p(querySelector(doc, "a")),
                 p(XML::getNodeSet(doc, "//a")[[1]]))
    expect_equal(p(querySelector(doc, "*", prefix = "")),
                 p(XML::getNodeSet(doc, "*")[[1]]))
    expect_equal(p(querySelector(doc, "d")), NULL)
    expect_equal(p(querySelector(doc, "c")), p(XML::getNodeSet(doc, "//c")[[1]]))

    # do the same again but on the xml doc itself
    doc <- XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    expect_equal(p(querySelector(doc, "a")),
                 p(XML::getNodeSet(XML::xmlRoot(doc), "//a")[[1]]))
    expect_equal(p(querySelector(doc, "*", prefix = "")),
                 p(XML::getNodeSet(XML::xmlRoot(doc), "*")[[1]]))
    expect_equal(p(querySelector(doc, "d")), NULL)
    expect_equal(p(querySelector(doc, "c")), p(XML::getNodeSet(XML::xmlRoot(doc), "//c")[[1]]))
})

test_that("querySelectorAll returns expected nodes", {
    skip_if_not_installed("XML")
    doc <- XML::xmlRoot(XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        lapply(x, function(node) XML::saveXML(node, file = NULL))
    }
    expect_equal(p(querySelectorAll(doc, "a")),
                 p(XML::getNodeSet(doc, "//a")))
    expect_equal(p(querySelectorAll(doc, "*", prefix = "")),
                 p(XML::getNodeSet(doc, "*")))
    expect_equal(p(querySelectorAll(doc, "c")),
                 p(XML::getNodeSet(doc, "//c")))

    # do the same again but on the xml doc itself
    doc <- XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    expect_equal(p(querySelectorAll(doc, "a")),
                 p(XML::getNodeSet(XML::xmlRoot(doc), "//a")))
    expect_equal(p(querySelectorAll(doc, "*", prefix = "")),
                 p(XML::getNodeSet(XML::xmlRoot(doc), "*")))
    expect_equal(p(querySelectorAll(doc, "c")),
                 p(XML::getNodeSet(XML::xmlRoot(doc), "//c")))
})

test_that("querySelectorAll returns empty list for no match", {
    skip_if_not_installed("XML")
    doc <- XML::xmlRoot(XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        lapply(x, function(node) XML::saveXML(node, file = NULL))
    }
    expect_equal(p(querySelectorAll(doc, "d")),
                 p(XML::getNodeSet(doc, "//d")))
})

test_that("querySelector handles namespaces", {
    skip_if_not_installed("XML")
    doc <- XML::xmlRoot(XML::xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>'))
    p <- function(x) {
        if (is.null(x)) x else XML::saveXML(x, file = NULL)
    }

    expect_equal(querySelector(doc, "circle"), NULL)
    expect_equal(querySelector(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg")),
                 NULL)
    expect_equal(p(querySelector(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(XML::getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))[[1]]))

    # now with querySelectorNS; the unprefixed query cannot match the
    # document's default namespace, which is exactly the behaviour
    # under test
    expect_equal(querySelectorNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg")),
                 NULL)
    expect_equal(p(querySelectorNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                 p(XML::getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))[[1]]))
})

test_that("querySelectorAll handles namespaces", {
    skip_if_not_installed("XML")
    doc <- XML::xmlRoot(XML::xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>'))
    p <- function(x) {
        lapply(x, function(node) XML::saveXML(node, file = NULL))
    }

    expect_equal(p(querySelectorAll(doc, "circle")),
                 p(XML::getNodeSet(doc, "//circle")))
    expect_equal(p(querySelectorAll(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(XML::getNodeSet(doc, "//circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))))
    expect_equal(p(querySelectorAll(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                 p(XML::getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))))

    # now with querySelectorAllNS; the unprefixed query cannot match
    # the document's default namespace, which is exactly the behaviour
    # under test
    expect_equal(p(querySelectorAllNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg"))),
                 suppressWarnings(p(XML::getNodeSet(doc, "//circle", namespaces = c(svg = "http://www.w3.org/2000/svg")))))
    expect_equal(p(querySelectorAllNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                 p(XML::getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))))
})

test_that("the namespaced queries are scoped to the node given", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse(paste0('<root xmlns:s="urn:s"><s:a id="outer"/>',
                           '<wrap><s:a id="inner"/></wrap></root>'))
    ns <- c(s = "urn:s")
    ids <- function(x) as.character(sapply(x, XML::xmlGetAttr, "id"))
    wrap <- XML::getNodeSet(doc, "//wrap")[[1]]

    # the namespace filter must not escape the queried node: the
    # 'outer' element is not inside <wrap>
    expect_equal(ids(querySelectorAllNS(wrap, "s|a", ns)), "inner")
    expect_equal(XML::xmlGetAttr(querySelectorNS(wrap, "s|a", ns), "id"),
                 "inner")
    # which is what the plain query with a namespace already does
    expect_equal(ids(querySelectorAll(wrap, "s|a", ns = ns)), "inner")

    # a node set is scoped the same way, node by node
    expect_equal(ids(querySelectorAllNS(XML::getNodeSet(doc, "//wrap"), "s|a", ns)),
                 "inner")

    # from the document (or its root) both are still found: the
    # 'descendant-or-self' axis from the document node includes the root
    expect_equal(ids(querySelectorAllNS(doc, "s|a", ns)),
                 c("outer", "inner"))
    expect_equal(ids(querySelectorAllNS(XML::xmlRoot(doc), "s|a", ns)),
                 c("outer", "inner"))
})

test_that("querySelector methods handle invalid arguments", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')

    selector_error <- "A valid selector (single character string) must be provided."
    expect_error(querySelector(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorAll(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorNS(doc), selector_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc), selector_error, fixed = TRUE)

    expect_error(querySelector(doc, c("a", "b")), selector_error, fixed = TRUE)
    expect_error(querySelectorAll(doc, c("a", "b")), selector_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, c("a", "b"), c(svg = "http://www.w3.org/2000/svg")), selector_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, c("a", "b"), c(svg = "http://www.w3.org/2000/svg")), selector_error, fixed = TRUE)
    expect_error(querySelector(doc, 1), "A valid selector (single character string) must be provided.", fixed = TRUE)
    expect_error(querySelector(doc, character(0)), selector_error, fixed = TRUE)
    expect_error(querySelector(doc, NA_character_), selector_error, fixed = TRUE)

    namespace_error <- "A namespace must be provided"
    expect_error(querySelectorNS(doc, "a"), namespace_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", NULL), namespace_error, fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", character(0)), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a"), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", NULL), namespace_error, fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", character(0)), namespace_error, fixed = TRUE)
})

test_that("querySelector returns the first node querySelectorAll finds", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse(paste0('<r xmlns:s="http://www.w3.org/2000/svg">',
                           '<b id="b1"/><a id="a1"/>',
                           '<w id="w1"><b id="b2"/><s:a id="s1"/></w>',
                           '<w id="w2"><a id="a2"/><s:a id="s2"/></w>',
                           '</r>'))
    ns <- c(s = "http://www.w3.org/2000/svg")
    id <- function(node) if (is.null(node)) NULL else XML::xmlGetAttr(node, "id")
    first <- function(nodes) if (length(nodes)) nodes[[1]] else NULL

    root <- XML::xmlRoot(doc)
    nodes <- XML::getNodeSet(root, "//w")
    # A grouped selector is a union, whose first node is the first in
    # document order rather than the first branch's first match.
    selectors <- c("a", "b", "a, b", "b, a", "w > a", ":scope > a", "z")
    for (selector in selectors) {
        for (obj in list(doc, root, nodes)) {
            expect_identical(querySelector(obj, selector),
                             first(querySelectorAll(obj, selector)))
        }
    }
    for (selector in c(selectors, "s|a", "s|a, b")) {
        for (obj in list(doc, root, nodes)) {
            expect_identical(querySelectorNS(obj, selector, ns),
                             first(querySelectorAllNS(obj, selector, ns)))
        }
    }

    expect_equal(id(querySelector(root, "a, b")), "b1")
    expect_equal(id(querySelector(nodes, "a")), "a2")
    expect_equal(id(querySelectorNS(root, "s|a", ns)), "s1")
    expect_null(querySelector(structure(list(), class = "XMLNodeSet"), "a"))
})
