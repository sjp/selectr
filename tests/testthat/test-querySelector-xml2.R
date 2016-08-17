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
    library(XML)
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
