context("html-translator")

htmlText <- paste0('<html><head><title>t</title></head><body>',
                   '<DIV class="wrap" lang="en">',
                   '<input type="checkbox" checked>',
                   '<input type="text" disabled>',
                   '<a href="u">l</a>',
                   '</DIV></body></html>')

test_that("XML HTML documents use the html translator by default", {
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)

    # Pseudo-classes that only the html translator implements
    expect_that(length(querySelectorAll(doc, "input:checked")), equals(1))
    expect_that(length(querySelectorAll(doc, ":disabled")), equals(1))
    expect_that(length(querySelectorAll(doc, ":link")), equals(1))
    expect_that(length(querySelectorAll(doc, ":lang(en)")), equals(4))

    # Element and attribute names are matched case-insensitively
    expect_that(length(querySelectorAll(doc, "DIV")), equals(1))
    expect_that(length(querySelectorAll(doc, "[HREF]")), equals(1))

    expect_false(is.null(querySelector(doc, "input:checked")))
})

test_that("an explicit translator overrides the html default", {
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)

    expect_that(length(querySelectorAll(doc, "input:checked",
                                        translator = "generic")),
                equals(0))
    expect_that(querySelector(doc, "input:checked", translator = "generic"),
                equals(NULL))
    # css_to_xpath()'s arguments are matched partially, so an
    # abbreviated argument counts as explicit too
    expect_that(length(querySelectorAll(doc, "input:checked",
                                        trans = "generic")),
                equals(0))
})

test_that("XML documents and nodes keep the generic translator", {
    library(XML)
    doc <- xmlParse('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_that(length(querySelectorAll(doc, "B")), equals(1))
    expect_that(length(querySelectorAll(doc, "b")), equals(0))
    expect_that(length(querySelectorAll(doc, "input:checked")), equals(0))

    # An XML node carries no record of the document it came from, so a
    # query starting from a node of an HTML document is generic
    hdoc <- htmlParse(htmlText, asText = TRUE)
    expect_that(length(querySelectorAll(xmlRoot(hdoc), "input:checked")),
                equals(0))
    expect_that(length(querySelectorAll(xmlRoot(hdoc), "input:checked",
                                        translator = "html")),
                equals(1))
})

test_that("namespaced queries on an XML HTML document use the html translator", {
    library(XML)
    doc <- htmlParse(htmlText, asText = TRUE)
    ns <- c(x = "http://www.w3.org/1999/xhtml")
    # htmlParse() does not put the document in a namespace, so these
    # match nothing; the point is that the translator still applies
    expect_that(length(querySelectorAllNS(doc, "x|input:checked", ns)),
                equals(0))
    expect_that(querySelectorNS(doc, "x|input:checked", ns), equals(NULL))
})

test_that("xml2 HTML documents use the html translator by default", {
    library(xml2)
    doc <- read_html(htmlText)

    expect_that(length(querySelectorAll(doc, "input:checked")), equals(1))
    expect_that(length(querySelectorAll(doc, ":disabled")), equals(1))
    expect_that(length(querySelectorAll(doc, ":link")), equals(1))
    expect_that(length(querySelectorAll(doc, "DIV")), equals(1))
    expect_that(length(querySelectorAll(doc, "[HREF]")), equals(1))
    expect_false(is.null(querySelector(doc, "input:checked")))

    expect_that(length(querySelectorAll(doc, "input:checked",
                                        translator = "generic")),
                equals(0))
})

test_that("xml2 nodes and node sets of an HTML document are detected", {
    library(xml2)
    doc <- read_html(htmlText)
    node <- querySelector(doc, "div")
    nodeset <- querySelectorAll(doc, "div")

    expect_that(length(querySelectorAll(node, "input:checked")), equals(1))
    expect_false(is.null(querySelector(node, "input:checked")))
    expect_that(length(querySelectorAll(nodeset, "input:checked")), equals(1))
    expect_false(is.null(querySelector(nodeset, "input:checked")))
})

test_that("xml2 XML documents keep the generic translator", {
    library(xml2)
    doc <- read_xml('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_that(length(querySelectorAll(doc, "B")), equals(1))
    expect_that(length(querySelectorAll(doc, "b")), equals(0))
    expect_that(length(querySelectorAll(doc, "input:checked")), equals(0))

    node <- querySelector(doc, "a")
    expect_that(length(querySelectorAll(node, "B")), equals(1))

    # An empty node set and a missing node have no document to
    # inspect, and must not error
    expect_that(length(querySelectorAll(querySelectorAll(doc, "zz"), "B")),
                equals(0))
    expect_that(length(querySelectorAll(xml_find_first(doc, "//zz"), "B")),
                equals(0))
})

test_that("css_to_xpath() still defaults to the generic translator", {
    expect_that(css_to_xpath("input:checked"),
                equals(css_to_xpath("input:checked", translator = "generic")))
})

test_that("the xhtml translator reads xml:lang as well as lang", {
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
    expect_that(pid("*|p:lang(en)"),
                equals(c("xmlonly", "langonly", "region", "inherited")))
    expect_that(pid("*|p:lang(*)"),
                equals(c("xmlonly", "langonly", "both", "region",
                         "inherited")))

    # xml:lang wins where both are present
    expect_that(pid("*|p:lang(de)"), equals("both"))

    # The shared language string also feeds the subtag, prefix and
    # extended-filtering branches, and is matched case-insensitively
    expect_that(pid("*|p:lang(en-GB)"), equals("region"))
    expect_that(pid("*|p:lang(en-*)"),
                equals(c("xmlonly", "langonly", "region", "inherited")))
    expect_that(pid("*|p:lang(*-gb)"), equals("region"))
})

test_that("the html translator stays lang-only", {
    library(xml2)
    doc <- read_html(paste0('<html><body><p id="x" xml:lang="en">a</p>',
                            '<p id="y" lang="en">b</p></body></html>'))
    expect_that(xml_attr(querySelectorAll(doc, "p:lang(en)"), "id"),
                equals("y"))
})
