context("methods")

test_that("method registration occurs correctly", {
    library(XML)
    xdoc <- xmlParse("<svg><circle /></svg>")

    library(xml2)
    x2doc <- read_xml("<svg><circle /></svg>")

    results <- querySelector(xdoc, "circle")
    results <- querySelector(x2doc, "circle")
})
