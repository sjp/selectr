context("querySelector-chaining")

chainDoc <- '<html><body>
  <table id="t1"><tr><td class="a">1</td><td>2</td></tr><tr><td>3</td></tr></table>
  <table id="t2"><tr><td class="a">4</td></tr></table>
  <p><td>outside</td></p>
</body></html>'

test_that("xml2 nodesets can be queried", {
    library(xml2)
    doc <- read_xml(chainDoc)
    p <- function(x) lapply(x, as.character)

    tables <- querySelectorAll(doc, "table")
    expect_that(length(tables), equals(2))

    cells <- querySelectorAll(tables, "td")
    expect_that(p(cells), equals(p(xml_find_all(doc, "//table//td"))))

    # a node matched from more than one node in the set appears once
    expect_that(length(querySelectorAll(querySelectorAll(doc, "table, tr"), "td")),
                equals(4))

    # querySelector() gives back the first match across the whole set
    expect_that(as.character(querySelector(tables, "td")),
                equals(as.character(xml_find_first(doc, "//table//td"))))
    expect_that(querySelector(tables, "div"), equals(NULL))
})

test_that(":scope on an xml2 nodeset is applied per node", {
    library(xml2)
    doc <- read_xml('<a><x><b id="1"/><c><b id="2"/></c></x><y><b id="3"/></y></a>')
    ids <- function(x) xml_attr(x, "id")

    kids <- querySelectorAll(doc, "x, y")
    expect_that(ids(querySelectorAll(kids, ":scope > b")), equals(c("1", "3")))
    expect_that(ids(querySelectorAll(kids, "b")), equals(c("1", "2", "3")))
})

test_that("querying an empty xml2 nodeset gives an empty nodeset", {
    library(xml2)
    doc <- read_xml(chainDoc)
    empty <- querySelectorAll(doc, "nosuchelement")
    expect_that(length(empty), equals(0))

    res <- querySelectorAll(empty, "td")
    expect_true(inherits(res, "xml_nodeset"))
    expect_that(length(res), equals(0))
    expect_that(querySelector(empty, "td"), equals(NULL))
})

test_that("querying an xml_missing gives an empty result", {
    library(xml2)
    doc <- read_xml(chainDoc)
    missing <- xml_find_first(doc, "//nosuchelement")
    expect_true(inherits(missing, "xml_missing"))

    res <- querySelectorAll(missing, "td")
    expect_true(inherits(res, "xml_nodeset"))
    expect_that(length(res), equals(0))
    expect_that(querySelector(missing, "td"), equals(NULL))

    # the namespaced variants are equally quiet
    svg <- c(svg = "http://www.w3.org/2000/svg")
    expect_that(length(querySelectorAllNS(missing, "svg|circle", svg)),
                equals(0))
    expect_that(querySelectorNS(missing, "svg|circle", svg), equals(NULL))
})

test_that("xml2 nodeset and missing methods validate their arguments", {
    library(xml2)
    doc <- read_xml(chainDoc)
    tables <- querySelectorAll(doc, "table")
    missing <- xml_find_first(doc, "//nosuchelement")

    expect_error(querySelectorAll(tables, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorAll(missing, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorNS(tables, "td"), "A namespace must be provided.")
    expect_error(querySelectorAllNS(missing, "td"), "A namespace must be provided.")
})

test_that("namespaced queries work on xml2 nodesets", {
    library(xml2)
    svg <- c(svg = "http://www.w3.org/2000/svg")
    doc <- read_xml('<svg xmlns="http://www.w3.org/2000/svg"><g><circle id="1"/></g><g><circle id="2"/></g></svg>')
    gs <- querySelectorAllNS(doc, "svg|g", svg)
    expect_that(length(gs), equals(2))

    circles <- querySelectorAll(gs, "svg|circle", ns = svg)
    expect_that(xml_attr(circles, "id"), equals(c("1", "2")))
    expect_that(xml_attr(querySelector(gs, "svg|circle", ns = svg), "id"),
                equals("1"))

    expect_that(xml_attr(querySelectorAllNS(gs, "svg|circle", svg), "id"),
                equals(c("1", "2")))
    expect_that(xml_attr(querySelectorNS(gs, "svg|circle", svg), "id"),
                equals("1"))
})

test_that("XML nodesets can be queried", {
    library(XML)
    doc <- xmlParse(chainDoc)
    p <- function(x) sapply(x, function(node) xmlValue(node))

    tables <- querySelectorAll(doc, "table")
    expect_true(inherits(tables, "XMLNodeSet"))
    expect_that(length(tables), equals(2))

    cells <- querySelectorAll(tables, "td")
    expect_true(inherits(cells, "XMLNodeSet"))
    expect_that(p(cells), equals(p(getNodeSet(doc, "//table//td"))))

    # a node matched from more than one node in the set appears once
    expect_that(length(querySelectorAll(querySelectorAll(doc, "table, tr"), "td")),
                equals(4))

    expect_that(xmlValue(querySelector(tables, "td")), equals("1"))
    expect_that(querySelector(tables, "div"), equals(NULL))
})

test_that(":scope on an XML nodeset is applied per node", {
    library(XML)
    doc <- xmlParse('<a><x><b id="1"/><c><b id="2"/></c></x><y><b id="3"/></y></a>')
    ids <- function(x) as.character(sapply(x, function(node) xmlGetAttr(node, "id")))

    kids <- querySelectorAll(doc, "x, y")
    expect_that(ids(querySelectorAll(kids, ":scope > b")), equals(c("1", "3")))
    expect_that(ids(querySelectorAll(kids, "b")), equals(c("1", "2", "3")))
})

test_that("querying an empty XML nodeset gives an empty nodeset", {
    library(XML)
    doc <- xmlParse(chainDoc)
    empty <- querySelectorAll(doc, "nosuchelement")
    expect_that(length(empty), equals(0))

    res <- querySelectorAll(empty, "td")
    expect_true(inherits(res, "XMLNodeSet"))
    expect_that(length(res), equals(0))
    expect_that(querySelector(empty, "td"), equals(NULL))
})

test_that("namespaced queries work on XML nodesets", {
    library(XML)
    svg <- c(svg = "http://www.w3.org/2000/svg")
    doc <- xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><g><circle id="1"/></g><g><circle id="2"/></g></svg>')
    ids <- function(x) as.character(sapply(x, function(node) xmlGetAttr(node, "id")))

    gs <- querySelectorAllNS(doc, "svg|g", svg)
    expect_that(length(gs), equals(2))

    expect_that(ids(querySelectorAll(gs, "svg|circle", ns = svg)),
                equals(c("1", "2")))
    expect_that(xmlGetAttr(querySelector(gs, "svg|circle", ns = svg), "id"),
                equals("1"))
    expect_that(ids(querySelectorAllNS(gs, "svg|circle", svg)),
                equals(c("1", "2")))
    expect_that(xmlGetAttr(querySelectorNS(gs, "svg|circle", svg), "id"),
                equals("1"))
})

test_that("XML nodeset methods validate their arguments", {
    library(XML)
    doc <- xmlParse(chainDoc)
    tables <- querySelectorAll(doc, "table")

    expect_error(querySelectorAll(tables, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorNS(tables, "td"), "A namespace must be provided.")
    expect_error(querySelectorAllNS(tables, "td"), "A namespace must be provided.")
})
