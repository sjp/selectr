context("methods")

test_that("method registration occurs correctly", {
    library(XML)
    xdoc <- xmlParse("<svg><circle /></svg>")

    library(xml2)
    x2doc <- read_xml("<svg><circle /></svg>")

    # querySelector() must dispatch to the methods registered for each
    # package's document class and find the element
    res_xml <- querySelector(xdoc, "circle")
    expect_true(inherits(res_xml, "XMLInternalNode"))
    expect_that(xmlName(res_xml), equals("circle"))

    res_xml2 <- querySelector(x2doc, "circle")
    expect_true(inherits(res_xml2, "xml_node"))
    expect_that(xml_name(res_xml2), equals("circle"))
})
