test_that("method registration occurs correctly", {
    skip_if_not_installed("XML")
    skip_if_not_installed("xml2")
    xdoc <- XML::xmlParse("<svg><circle /></svg>")

    x2doc <- xml2::read_xml("<svg><circle /></svg>")

    # querySelector() must dispatch to the methods registered for each
    # package's document class and find the element
    res_xml <- querySelector(xdoc, "circle")
    expect_true(inherits(res_xml, "XMLInternalNode"))
    expect_equal(XML::xmlName(res_xml), "circle")

    res_xml2 <- querySelector(x2doc, "circle")
    expect_true(inherits(res_xml2, "xml_node"))
    expect_equal(xml2::xml_name(res_xml2), "circle")
})
