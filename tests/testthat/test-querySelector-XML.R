context("querySelector-XML")

test_that("querySelector returns a single node or NULL", {
    library(XML)
    doc <- xmlRoot(xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        if (is.null(x))
            return(x)
        saveXML(x, file = NULL)
    }
    expect_that(p(querySelector(doc, "a")),
                equals(p(getNodeSet(doc, "//a")[[1]])))
    expect_that(p(querySelector(doc, "*", prefix = "")),
                equals(p(getNodeSet(doc, "*")[[1]])))
    expect_that(p(querySelector(doc, "d")), equals(NULL))
    expect_that(p(querySelector(doc, "c")), equals(p(getNodeSet(doc, "//c")[[1]])))

    # do the same again but on the xml doc itself
    doc <- xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    expect_that(p(querySelector(doc, "a")),
                equals(p(getNodeSet(xmlRoot(doc), "//a")[[1]])))
    expect_that(p(querySelector(doc, "*", prefix = "")),
                equals(p(getNodeSet(xmlRoot(doc), "*")[[1]])))
    expect_that(p(querySelector(doc, "d")), equals(NULL))
    expect_that(p(querySelector(doc, "c")), equals(p(getNodeSet(xmlRoot(doc), "//c")[[1]])))
})

test_that("querySelectorAll returns expected nodes", {
    library(XML)
    doc <- xmlRoot(xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        lapply(x, function(node) saveXML(node, file = NULL))
    }
    expect_that(p(querySelectorAll(doc, "a")),
                equals(p(getNodeSet(doc, "//a"))))
    expect_that(p(querySelectorAll(doc, "*", prefix = "")),
                equals(p(getNodeSet(doc, "*"))))
    expect_that(p(querySelectorAll(doc, "c")),
                equals(p(getNodeSet(doc, "//c"))))

    # do the same again but on the xml doc itself
    doc <- xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    expect_that(p(querySelectorAll(doc, "a")),
                equals(p(getNodeSet(xmlRoot(doc), "//a"))))
    expect_that(p(querySelectorAll(doc, "*", prefix = "")),
                equals(p(getNodeSet(xmlRoot(doc), "*"))))
    expect_that(p(querySelectorAll(doc, "c")),
                equals(p(getNodeSet(xmlRoot(doc), "//c"))))
})

test_that("querySelectorAll returns empty list for no match", {
    library(XML)
    doc <- xmlRoot(xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>'))
    p <- function(x) {
        lapply(x, function(node) saveXML(node, file = NULL))
    }
    expect_that(p(querySelectorAll(doc, "d")),
                equals(p(getNodeSet(doc, "//d"))))
})

test_that("querySelector handles namespaces", {
    library(XML)
    doc <- xmlRoot(xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>'))
    p <- function(x) {
        if (is.null(x)) x else saveXML(x, file = NULL)
    }

    expect_that(querySelector(doc, "circle"), equals(NULL))
    expect_that(querySelector(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg")),
                equals(NULL))
    expect_that(p(querySelector(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))[[1]])))

    # now with querySelectorNS
    expect_that(querySelectorNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg")), equals(NULL))
    expect_that(p(querySelectorNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg"))[[1]])))
})

test_that("querySelectorAll handles namespaces", {
    library(XML)
    doc <- xmlRoot(xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>'))
    p <- function(x) {
        lapply(x, function(node) saveXML(node, file = NULL))
    }

    expect_that(p(querySelectorAll(doc, "circle")),
                equals(p(getNodeSet(doc, "//circle"))))
    expect_that(p(querySelectorAll(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//circle", namespaces = c(svg = "http://www.w3.org/2000/svg")))))
    expect_that(p(querySelectorAll(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg")))))

    # now with querySelectorAllNS
    expect_that(p(querySelectorAllNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//circle", namespaces = c(svg = "http://www.w3.org/2000/svg")))))
    expect_that(p(querySelectorAllNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(getNodeSet(doc, "//svg:circle", namespaces = c(svg = "http://www.w3.org/2000/svg")))))
})

test_that("querySelector methods handle invalid arguments", {
    library(XML)
    doc <- xmlParse('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')

    expect_error(querySelector(doc), "A valid selector (character vector) must be provided.", fixed = TRUE)
    expect_error(querySelectorAll(doc), "A valid selector (character vector) must be provided.", fixed = TRUE)
    expect_error(querySelectorNS(doc), "A valid selector (character vector) must be provided.", fixed = TRUE)
    expect_error(querySelectorAllNS(doc), "A valid selector (character vector) must be provided.", fixed = TRUE)

    expect_error(querySelectorNS(doc, "a"), "A namespace must be provided.", fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", NULL), "A namespace must be provided.", fixed = TRUE)
    expect_error(querySelectorNS(doc, "a", character(0)), "A namespace must be provided.", fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a"), "A namespace must be provided.", fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", NULL), "A namespace must be provided.", fixed = TRUE)
    expect_error(querySelectorAllNS(doc, "a", character(0)), "A namespace must be provided.", fixed = TRUE)
})
