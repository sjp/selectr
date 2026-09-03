htmlText <- paste0('<html><head><title>t</title></head><body>',
                   '<DIV class="wrap" lang="en">',
                   '<input type="checkbox" checked>',
                   '<input type="text" disabled>',
                   '<a href="u">l</a>',
                   '</DIV></body></html>')

test_that("XML HTML documents use the html translator by default", {
    skip_if_not_installed("XML")
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)

    # Pseudo-classes that only the html translator implements
    expect_equal(length(querySelectorAll(doc, "input:checked")), 1)
    expect_equal(length(querySelectorAll(doc, ":disabled")), 1)
    expect_equal(length(querySelectorAll(doc, ":link")), 1)
    expect_equal(length(querySelectorAll(doc, ":lang(en)")), 4)

    # Element and attribute names are matched case-insensitively
    expect_equal(length(querySelectorAll(doc, "DIV")), 1)
    expect_equal(length(querySelectorAll(doc, "[HREF]")), 1)

    expect_false(is.null(querySelector(doc, "input:checked")))
})

test_that("an explicit translator overrides the html default", {
    skip_if_not_installed("XML")
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)

    expect_equal(length(querySelectorAll(doc, "input:checked",
                                         translator = "generic")),
                 0)
    expect_equal(querySelector(doc, "input:checked", translator = "generic"),
                 NULL)
    # css_to_xpath()'s arguments are matched partially, so an
    # abbreviated argument counts as explicit too
    expect_equal(length(querySelectorAll(doc, "input:checked",
                                         trans = "generic")),
                 0)
})

test_that("XML documents and nodes keep the generic translator", {
    skip_if_not_installed("XML")
    library(XML)
    doc <- xmlParse('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_equal(length(querySelectorAll(doc, "B")), 1)
    expect_equal(length(querySelectorAll(doc, "b")), 0)
    expect_equal(length(querySelectorAll(doc, "input:checked")), 0)

    node <- querySelector(doc, "a")
    expect_equal(length(querySelectorAll(node, "B")), 1)

    # A node built outside any document, and an empty node set, have
    # no document to inspect, and must not error
    expect_equal(length(querySelectorAll(newXMLNode("a"), "B")), 0)
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "zz"), "B")),
                 0)
})

test_that("XML nodes and node sets of an HTML document are detected", {
    skip_if_not_installed("XML")
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)
    node <- querySelector(doc, "div")
    nodeset <- querySelectorAll(doc, "div")

    expect_equal(length(querySelectorAll(node, "input:checked")), 1)
    expect_equal(length(querySelectorAll(node, "INPUT")), 2)
    expect_false(is.null(querySelector(node, "input:checked")))
    expect_equal(length(querySelectorAll(nodeset, "input:checked")), 1)
    expect_false(is.null(querySelector(nodeset, "input:checked")))

    # An explicit translator still wins for both
    expect_equal(length(querySelectorAll(node, "input:checked",
                                         translator = "generic")),
                 0)
    expect_equal(length(querySelectorAll(nodeset, "input:checked",
                                         translator = "generic")),
                 0)
})

test_that("namespaced queries on an XML HTML document use the html translator", {
    skip_if_not_installed("XML")
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)
    ns <- c(x = "http://www.w3.org/1999/xhtml")
    # htmlParse() does not put the document in a namespace, so these
    # match nothing; the point is that the translator still applies
    expect_equal(length(querySelectorAllNS(doc, "x|input:checked", ns)),
                 0)
    expect_equal(querySelectorNS(doc, "x|input:checked", ns), NULL)
})

test_that("xml2 HTML documents use the html translator by default", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_html(htmlText)

    expect_equal(length(querySelectorAll(doc, "input:checked")), 1)
    expect_equal(length(querySelectorAll(doc, ":disabled")), 1)
    expect_equal(length(querySelectorAll(doc, ":link")), 1)
    expect_equal(length(querySelectorAll(doc, "DIV")), 1)
    expect_equal(length(querySelectorAll(doc, "[HREF]")), 1)
    expect_false(is.null(querySelector(doc, "input:checked")))

    expect_equal(length(querySelectorAll(doc, "input:checked",
                                         translator = "generic")),
                 0)
})

test_that("xml2 nodes and node sets of an HTML document are detected", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_html(htmlText)
    node <- querySelector(doc, "div")
    nodeset <- querySelectorAll(doc, "div")

    expect_equal(length(querySelectorAll(node, "input:checked")), 1)
    expect_false(is.null(querySelector(node, "input:checked")))
    expect_equal(length(querySelectorAll(nodeset, "input:checked")), 1)
    expect_false(is.null(querySelector(nodeset, "input:checked")))
})

test_that("xml2 XML documents keep the generic translator", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_equal(length(querySelectorAll(doc, "B")), 1)
    expect_equal(length(querySelectorAll(doc, "b")), 0)
    expect_equal(length(querySelectorAll(doc, "input:checked")), 0)

    node <- querySelector(doc, "a")
    expect_equal(length(querySelectorAll(node, "B")), 1)

    # An empty node set and a missing node have no document to
    # inspect, and must not error
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "zz"), "B")),
                 0)
    expect_equal(length(querySelectorAll(xml_find_first(doc, "//zz"), "B")),
                 0)
})

test_that("css_to_xpath() still defaults to the generic translator", {
    expect_equal(css_to_xpath("input:checked"),
                 css_to_xpath("input:checked", translator = "generic"))
})

test_that("the xhtml translator reads xml:lang as well as lang", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml(paste0(
        '<html xmlns="http://www.w3.org/1999/xhtml"><body>',
        '<p id="xmlonly" xml:lang="en">a</p>',
        '<p id="langonly" lang="en">b</p>',
        '<p id="both" xml:lang="de" lang="en">c</p>',
        '<p id="region" xml:lang="EN-gb">d</p>',
        '<div xml:lang="en"><p id="inherited">e</p>',
        '<p id="reset" xml:lang="">f</p></div>',
        '<p id="none">g</p>',
        '</body></html>'))

    pid <- function(selector)
        xml_attr(querySelectorAll(doc, selector, translator = "xhtml"), "id")

    # Both attributes are consulted, and an element with neither, or
    # with an empty value, has an unknown language
    expect_equal(pid("*|p:lang(en)"),
                 c("xmlonly", "langonly", "region", "inherited"))
    expect_equal(pid("*|p:lang(*)"),
                 c("xmlonly", "langonly", "both", "region",
                   "inherited"))

    # xml:lang wins where both are present
    expect_equal(pid("*|p:lang(de)"), "both")

    # The shared language string also feeds the subtag, prefix and
    # extended-filtering branches, and is matched case-insensitively
    expect_equal(pid("*|p:lang(en-GB)"), "region")
    expect_equal(pid("*|p:lang(en-*)"),
                 c("xmlonly", "langonly", "region", "inherited"))
    expect_equal(pid("*|p:lang(*-gb)"), "region")
})

test_that("the html translator stays lang-only", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_html(paste0('<html><body><p id="x" xml:lang="en">a</p>',
                            '<p id="y" lang="en">b</p></body></html>'))
    expect_equal(xml_attr(querySelectorAll(doc, "p:lang(en)"), "id"),
                 "y")
})
