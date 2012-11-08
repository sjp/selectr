context("querySelector")

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
