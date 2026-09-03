chainDoc <- '<html><body>
  <table id="t1"><tr><td class="a">1</td><td>2</td></tr><tr><td>3</td></tr></table>
  <table id="t2"><tr><td class="a">4</td></tr></table>
  <p><td>outside</td></p>
</body></html>'

test_that("xml2 nodesets can be queried", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(chainDoc)
    p <- function(x) lapply(x, as.character)

    tables <- querySelectorAll(doc, "table")
    expect_equal(length(tables), 2)

    cells <- querySelectorAll(tables, "td")
    expect_equal(p(cells), p(xml2::xml_find_all(doc, "//table//td")))

    # a node matched from more than one node in the set appears once
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "table, tr"), "td")),
                 4)

    # querySelector() gives back the first match across the whole set
    expect_equal(as.character(querySelector(tables, "td")),
                 as.character(xml2::xml_find_first(doc, "//table//td")))
    expect_equal(querySelector(tables, "div"), NULL)
})

test_that(":scope on an xml2 nodeset is applied per node", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml('<a><x><b id="1"/><c><b id="2"/></c></x><y><b id="3"/></y></a>')
    ids <- function(x) xml2::xml_attr(x, "id")

    kids <- querySelectorAll(doc, "x, y")
    expect_equal(ids(querySelectorAll(kids, ":scope > b")), c("1", "3"))
    expect_equal(ids(querySelectorAll(kids, "b")), c("1", "2", "3"))
})

test_that("querying an empty xml2 nodeset gives an empty nodeset", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(chainDoc)
    empty <- querySelectorAll(doc, "nosuchelement")
    expect_equal(length(empty), 0)

    res <- querySelectorAll(empty, "td")
    expect_true(inherits(res, "xml_nodeset"))
    expect_equal(length(res), 0)
    expect_equal(querySelector(empty, "td"), NULL)
})

test_that("querying an xml_missing gives an empty result", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(chainDoc)
    missing <- xml2::xml_find_first(doc, "//nosuchelement")
    expect_true(inherits(missing, "xml_missing"))

    res <- querySelectorAll(missing, "td")
    expect_true(inherits(res, "xml_nodeset"))
    expect_equal(length(res), 0)
    expect_equal(querySelector(missing, "td"), NULL)

    # the namespaced variants are equally quiet
    svg <- c(svg = "http://www.w3.org/2000/svg")
    expect_equal(length(querySelectorAllNS(missing, "svg|circle", svg)),
                 0)
    expect_equal(querySelectorNS(missing, "svg|circle", svg), NULL)
})

test_that("xml2 nodeset and missing methods validate their arguments", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(chainDoc)
    tables <- querySelectorAll(doc, "table")
    missing <- xml2::xml_find_first(doc, "//nosuchelement")

    expect_error(querySelectorAll(tables, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorAll(missing, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorNS(tables, "td"), "A namespace must be provided.")
    expect_error(querySelectorAllNS(missing, "td"), "A namespace must be provided.")
})

test_that("namespaced queries work on xml2 nodesets", {
    skip_if_not_installed("xml2")
    svg <- c(svg = "http://www.w3.org/2000/svg")
    doc <- xml2::read_xml('<svg xmlns="http://www.w3.org/2000/svg"><g><circle id="1"/></g><g><circle id="2"/></g></svg>')
    gs <- querySelectorAllNS(doc, "svg|g", svg)
    expect_equal(length(gs), 2)

    circles <- querySelectorAll(gs, "svg|circle", ns = svg)
    expect_equal(xml2::xml_attr(circles, "id"), c("1", "2"))
    expect_equal(xml2::xml_attr(querySelector(gs, "svg|circle", ns = svg), "id"),
                 "1")

    expect_equal(xml2::xml_attr(querySelectorAllNS(gs, "svg|circle", svg), "id"),
                 c("1", "2"))
    expect_equal(xml2::xml_attr(querySelectorNS(gs, "svg|circle", svg), "id"),
                 "1")
})

test_that("XML nodesets can be queried", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse(chainDoc)
    p <- function(x) sapply(x, function(node) XML::xmlValue(node))

    tables <- querySelectorAll(doc, "table")
    expect_true(inherits(tables, "XMLNodeSet"))
    expect_equal(length(tables), 2)

    cells <- querySelectorAll(tables, "td")
    expect_true(inherits(cells, "XMLNodeSet"))
    expect_equal(p(cells), p(XML::getNodeSet(doc, "//table//td")))

    # a node matched from more than one node in the set appears once
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "table, tr"), "td")),
                 4)

    expect_equal(XML::xmlValue(querySelector(tables, "td")), "1")
    expect_equal(querySelector(tables, "div"), NULL)
})

test_that(":scope on an XML nodeset is applied per node", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse('<a><x><b id="1"/><c><b id="2"/></c></x><y><b id="3"/></y></a>')
    ids <- function(x) as.character(sapply(x, function(node) XML::xmlGetAttr(node, "id")))

    kids <- querySelectorAll(doc, "x, y")
    expect_equal(ids(querySelectorAll(kids, ":scope > b")), c("1", "3"))
    expect_equal(ids(querySelectorAll(kids, "b")), c("1", "2", "3"))
})

test_that("querying an empty XML nodeset gives an empty nodeset", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse(chainDoc)
    empty <- querySelectorAll(doc, "nosuchelement")
    expect_equal(length(empty), 0)

    res <- querySelectorAll(empty, "td")
    expect_true(inherits(res, "XMLNodeSet"))
    expect_equal(length(res), 0)
    expect_equal(querySelector(empty, "td"), NULL)
})

test_that("namespaced queries work on XML nodesets", {
    skip_if_not_installed("XML")
    svg <- c(svg = "http://www.w3.org/2000/svg")
    doc <- XML::xmlParse('<svg xmlns="http://www.w3.org/2000/svg"><g><circle id="1"/></g><g><circle id="2"/></g></svg>')
    ids <- function(x) as.character(sapply(x, function(node) XML::xmlGetAttr(node, "id")))

    gs <- querySelectorAllNS(doc, "svg|g", svg)
    expect_equal(length(gs), 2)

    expect_equal(ids(querySelectorAll(gs, "svg|circle", ns = svg)),
                 c("1", "2"))
    expect_equal(XML::xmlGetAttr(querySelector(gs, "svg|circle", ns = svg), "id"),
                 "1")
    expect_equal(ids(querySelectorAllNS(gs, "svg|circle", svg)),
                 c("1", "2"))
    expect_equal(XML::xmlGetAttr(querySelectorNS(gs, "svg|circle", svg), "id"),
                 "1")
})

test_that("XML nodeset methods validate their arguments", {
    skip_if_not_installed("XML")
    doc <- XML::xmlParse(chainDoc)
    tables <- querySelectorAll(doc, "table")

    expect_error(querySelectorAll(tables, c("td", "tr")),
                 "A valid selector .*must be provided")
    expect_error(querySelectorNS(tables, "td"), "A namespace must be provided.")
    expect_error(querySelectorAllNS(tables, "td"), "A namespace must be provided.")
})
