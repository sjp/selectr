test_that("querySelector methods present an error on non-XML/xml2 objects", {
    expect_error(querySelector(list()), class = "selectr_argument_error")
    expect_error(querySelectorAll(list()), class = "selectr_argument_error")
    expect_error(querySelectorNS(list()), class = "selectr_argument_error")
    expect_error(querySelectorAllNS(list()), class = "selectr_argument_error")
})

test_that("an R-level XML tree is reported as one, not as a foreign object", {
    skip_if_not_installed("XML")
    # XML::xmlTreeParse() and XML::htmlTreeParse() build a tree of R lists rather
    # than the internal document XPath needs, so the default method's
    # "not an 'XML' ... document" would describe the wrong problem
    tree <- XML::xmlTreeParse("<a><b/></a>", asText = TRUE)
    html <- XML::htmlTreeParse("<p>text", asText = TRUE)
    for (doc in list(tree, XML::xmlRoot(tree), html)) {
        expect_error(querySelector(doc, "b"),
                     "is an R-level 'XML' tree",
                     fixed = TRUE, class = "selectr_argument_error")
        expect_error(querySelectorAll(doc, "b"),
                     "Re-parse the document with XML::xmlParse()",
                     fixed = TRUE, class = "selectr_argument_error")
        expect_error(querySelectorNS(doc, "b", c(x = "y")),
                     "querySelectorNS() is an R-level",
                     fixed = TRUE, class = "selectr_argument_error")
        expect_error(querySelectorAllNS(doc, "b", c(x = "y")),
                     "querySelectorAllNS() is an R-level",
                     fixed = TRUE, class = "selectr_argument_error")
    }

    # the internal form of the same document is unaffected
    expect_equal(XML::xmlName(querySelector(
        XML::xmlParse("<a><b/></a>", asText = TRUE), "b")), "b")
})
