context("querySelector-xml2")

test_that("querySelector returns a single node or NULL", {
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        if (is.null(x)) x else as.character(x)
    }
    expect_that(p(querySelector(doc, "a")),
                equals(p(xml_find_first(doc, "//a"))))
    expect_that(p(querySelector(doc, "*", prefix = "")),
                equals(p(xml_find_first(doc, "*"))))
    expect_that(p(querySelector(doc, "d")), equals(NULL))
    expect_that(p(querySelector(doc, "c")), equals(p(xml_find_first(doc, "//c"))))
})

test_that("querySelectorAll returns expected nodes", {
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }
    expect_that(p(querySelectorAll(doc, "a")),
                equals(p(xml_find_all(doc, "//a"))))
    expect_that(p(querySelectorAll(doc, "*", prefix = "")),
                equals(p(xml_find_all(doc, "*"))))
    expect_that(p(querySelectorAll(doc, "c")),
                equals(p(xml_find_all(doc, "//c"))))
})

test_that("querySelectorAll returns empty list for no match", {
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }
    expect_that(p(querySelectorAll(doc, "d")),
                equals(p(xml_find_all(doc, "//d"))))
})

test_that("querySelector handles namespaces", {
    library(xml2)
    doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>')
    p <- function(x) {
        if (is.null(x)) x else as.character(x)
    }

    expect_that(querySelector(doc, "circle"), equals(NULL))
    expect_that(querySelector(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg")),
                equals(NULL))
    expect_that(p(querySelector(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))[[1]])))

    # now with querySelectorNS
    expect_that(querySelectorNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg")), equals(NULL))
    expect_that(p(querySelectorNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg"))[[1]])))
})

test_that("querySelectorAll handles namespaces", {
    library(xml2)
    doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><circle cx="10" cy="10" r="10"/><circle cx="20" cy="20" r="20"/><circle cx="30" cy="30" r="30"/></svg>')
    p <- function(x) {
        lapply(x, function(node) as.character(node))
    }

    expect_that(p(querySelectorAll(doc, "circle")),
                equals(p(xml_find_all(doc, "//circle"))))
    expect_that(p(querySelectorAll(doc, "circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//circle", ns = c(svg = "http://www.w3.org/2000/svg")))))
    expect_that(p(querySelectorAll(doc, "svg|circle", ns = c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg")))))

    # now with querySelectorAllNS
    expect_that(p(querySelectorAllNS(doc, "circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//circle", ns = c(svg = "http://www.w3.org/2000/svg")))))
    expect_that(p(querySelectorAllNS(doc, "svg|circle", c(svg = "http://www.w3.org/2000/svg"))),
                equals(p(xml_find_all(doc, "//svg:circle", ns = c(svg = "http://www.w3.org/2000/svg")))))
})

test_that("querySelector methods handle invalid arguments", {
    library(xml2)
    doc <- read_xml('<a><b id="#test"/><c class="ex"/><c class="xmp"/></a>')

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
