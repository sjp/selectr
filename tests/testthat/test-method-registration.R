test_that("method registration occurs correctly", {
    skip_if_not_installed("XML")
    skip_if_not_installed("xml2")
    library(XML)
    xdoc <- xmlParse("<svg><circle /></svg>")

    library(xml2)
    x2doc <- read_xml("<svg><circle /></svg>")

    # querySelector() must dispatch to the methods registered for each
    # package's document class and find the element
    res_xml <- querySelector(xdoc, "circle")
    expect_true(inherits(res_xml, "XMLInternalNode"))
    expect_equal(xmlName(res_xml), "circle")

    res_xml2 <- querySelector(x2doc, "circle")
    expect_true(inherits(res_xml2, "xml_node"))
    expect_equal(xml_name(res_xml2), "circle")
})
