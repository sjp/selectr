context("namespaces")

test_that("namespace selectors translate faithfully", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # '*|e' matches 'e' in any namespace, including none
    expect_that(xpath("*|e"), equals("*[local-name() = 'e']"))
    # '|e' matches 'e' in no namespace, which is what an unprefixed
    # XPath name test already means
    expect_that(xpath("|e"), equals("e"))
    # '|e' with a name unusable as an XPath name test must still pin the
    # null namespace: a bare name() test is also unprefixed for an
    # element in a default namespace
    expect_that(xpath("|é"),
                equals("*[name() = 'é' and namespace-uri() = '']"))
    # '*|*' is equivalent to '*'
    expect_that(xpath("*|*"), equals("*"))
    # '|*' matches any element in no namespace
    expect_that(xpath("|*"), equals("*[namespace-uri() = '']"))
    # 'ns|e' defers prefix-to-URI binding to evaluation time
    expect_that(xpath("ns|e"), equals("ns:e"))

    # Attribute selectors
    expect_that(xpath("[*|a]"), equals("*[@*[local-name() = 'a']]"))
    expect_that(xpath("[*|a='v']"),
                equals("*[@*[local-name() = 'a'] = 'v']"))
    # Unprefixed attribute names have no namespace, so '[|a]' is
    # equivalent to '[a]'
    expect_that(xpath("[|a]"), equals("*[@a]"))
    expect_that(xpath("[|a='v']"), equals("*[@a = 'v']"))
    expect_that(xpath("[ns|a]"), equals("*[@ns:a]"))
    # An unsafe prefix (not an NCName) forces a whole-name comparison
    # on the attribute axis, mirroring the element path; a QName like
    # '@1ns:href' is not valid XPath and would fail to compile
    expect_that(xpath("[\\31 ns|href]"),
                equals("*[attribute::*[name() = '1ns:href']]"))
    expect_that(xpath("[\\31 ns|href='v']"),
                equals("*[attribute::*[name() = '1ns:href'] = 'v']"))
    # An unsafe local name already took this path; guard it still does
    expect_that(xpath("[ns|\\31 href]"),
                equals("*[attribute::*[name() = 'ns:1href']]"))

    # Composability
    expect_that(xpath(":not(*|e)"), equals("*[not(local-name() = 'e')]"))
    expect_that(xpath("div > *|e"), equals("div/*[local-name() = 'e']"))

    # On the right of '+' the name becomes a node test on the self
    # axis, which like the bare name test in a path step matches an
    # unprefixed name in the null namespace only; '|e' and 'e'
    # coincide there too
    expect_that(xpath("e + f"),
                equals("e/following-sibling::*[1][self::f]"))
    expect_that(xpath("e + |f"),
                equals("e/following-sibling::*[1][self::f]"))
    expect_that(xpath("e + ns|f"),
                equals("e/following-sibling::*[1][self::ns:f]"))
    expect_that(xpath("e + *|f"),
                equals("e/following-sibling::*[1][local-name() = 'f']"))

    # Inside pseudo-class arguments, prefixed names keep resolving
    # through the namespace map (a name test on the self axis or the
    # path step itself), rather than comparing against the document's
    # literal prefix with name()
    expect_that(xpath(":is(ns|e)"), equals("*[self::ns:e]"))
    expect_that(xpath(":not(ns|e)"), equals("*[not(self::ns:e)]"))
    expect_that(xpath(":has(ns|e)"), equals("*[.//ns:e]"))
    expect_that(xpath(":has(> ns|e)"), equals("*[child::ns:e]"))
    # Under '+' the position predicate [1] must precede the name test
    # ("the next sibling, if it is an ns:e"), so the name cannot stay
    # on the path step
    expect_that(xpath(":has(+ ns|e)"),
                equals("*[following-sibling::*[1][self::ns:e]]"))

    # 'ns|*' is a node test too ('*' is a valid local part), not an
    # unsafe name: stringifying it as name() = 'ns:*' could never
    # match, as name() never returns a literal '*'
    expect_that(xpath(":is(ns|*)"), equals("*[self::ns:*]"))
    expect_that(xpath(":not(ns|*)"), equals("*[not(self::ns:*)]"))
    expect_that(xpath(":has(ns|*)"), equals("*[.//ns:*]"))
})

test_that("namespace selector specificity is correct", {
    spec <- function(css) parse(css)[[1]]$specificity()

    # Universal selectors and namespace components contribute nothing
    expect_that(spec("*|e"), equals(c(0, 0, 1)))
    expect_that(spec("|e"), equals(c(0, 0, 1)))
    expect_that(spec("*|*"), equals(c(0, 0, 0)))
    expect_that(spec("|*"), equals(c(0, 0, 0)))
})

test_that("malformed namespace selectors are rejected", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_error(css("e|"), "Expected ident or '\\*'")
    # 'a||b' is not a malformed namespace selector but the Selectors 4
    # column combinator, so it is rejected by name
    expect_error(css("a||b"), "The column combinator '\\|\\|' is not supported")
    expect_error(css("div .|x"), "Expected ident")
})

test_that("namespace selectors match correct elements", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r xmlns:svg="http://www.w3.org/2000/svg" a="x">',
        '<e>plain</e><svg:e svg:a="y">svg</svg:e></r>'))
    ns <- xml2::xml_ns(doc)
    matches <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_name(nodes, ns)
    }

    expect_that(matches("*|e"), equals(c("e", "svg:e")))
    expect_that(matches("|e"), equals("e"))
    expect_that(matches("|*"), equals(c("r", "e")))
    expect_that(matches("*|*"), equals(c("r", "e", "svg:e")))
    expect_that(matches("svg|e"), equals("svg:e"))
    expect_that(matches("[*|a]"), equals(c("r", "svg:e")))
    expect_that(matches("[|a]"), equals("r"))
})

test_that("namespaced pseudo-class arguments match by URI, not prefix", {
    skip_if_not_installed("xml2")

    # the document binds the SVG namespace to 's', the query to 'svg':
    # matching must go through the namespace map (URI), not compare
    # qualified names as strings
    doc <- xml2::read_xml(paste0(
        '<r xmlns:s="http://www.w3.org/2000/svg">',
        '<s:g id="g1"/><b id="b1"/></r>'))
    ns <- c(svg = "http://www.w3.org/2000/svg")
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("svg|g"), equals("g1"))
    expect_that(ids(":is(svg|g)"), equals("g1"))
    expect_that(ids(":not(svg|g)"), equals(c(NA, "b1"))) # r and b
    expect_that(ids(":is(svg|*)"), equals("g1"))
    expect_that(ids(":not(svg|*)"), equals(c(NA, "b1"))) # r and b
    expect_that(xml2::xml_name(xml2::xml_find_all(
                    doc, css_to_xpath(":has(svg|g)", prefix = "//"), ns)),
                equals("r"))
})

test_that("unprefixed names match no namespace wherever they appear", {
    skip_if_not_installed("xml2")

    # An unprefixed name is an XPath name test wherever it sits in the
    # selector, so it matches in no namespace only: an element in a
    # *default* namespace needs an explicit prefix, inside a functional
    # pseudo-class argument exactly as at the top level
    doc <- xml2::read_xml(paste0(
        '<r id="root"><p id="plain"/><span id="plain-sib"/>',
        '<x xmlns="http://d" id="wrapper">',
        '<p id="defaulted"/><span id="defaulted-sib"/></x></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"),
                                    c(d = "http://d"))
        xml2::xml_attr(nodes, "id")
    }

    # top level and pseudo-class argument agree
    expect_that(ids("p"), equals("plain"))
    expect_that(ids(":is(p)"), equals("plain"))
    expect_that(ids(":where(p)"), equals("plain"))
    expect_that(ids("*:not(p)"),
                equals(c("root", "plain-sib", "wrapper",
                         "defaulted", "defaulted-sib")))

    # so do the combinators, on either side and in either direction
    expect_that(ids("r > p"), equals("plain"))
    expect_that(ids(":is(r > p)"), equals("plain"))
    expect_that(ids("p + span"), equals("plain-sib"))
    expect_that(ids(":is(p + span)"), equals("plain-sib"))
    expect_that(ids("p ~ span"), equals("plain-sib"))
    expect_that(ids(":is(p ~ span)"), equals("plain-sib"))
    expect_that(ids(":has(p)"), equals("root"))
    expect_that(ids(":has(> p)"), equals("root"))
    expect_that(ids(":has(+ span)"), equals("plain"))

    # a prefix bound to the default namespace reaches the rest,
    # again in both positions
    expect_that(ids("d|p"), equals("defaulted"))
    expect_that(ids(":is(d|p)"), equals("defaulted"))
    expect_that(ids(":has(d|p)"), equals(c("root", "wrapper")))
})

test_that("'+' matches type selectors by namespace like '~'", {
    skip_if_not_installed("xml2")

    # The element following the first p is in a default namespace; a
    # bare type selector must not match it under '+', just as it does
    # not under '~'
    doc <- xml2::read_xml(paste0(
        '<r><w><p/><span xmlns="http://d" id="defaulted"/></w>',
        '<v><p/><span id="plain"/></v></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("p + span"), equals("plain"))
    expect_that(ids("p ~ span"), equals("plain"))
    # '|span' pins the null namespace explicitly: same elements
    expect_that(ids("p + |span"), equals("plain"))
    # the universal selector matches either way
    expect_that(ids("p + *"), equals(c("defaulted", "plain")))
})

test_that("'|e' with an unsafe name does not match a default namespace", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r><é id="plain"/>',
        '<w><é xmlns="http://default" id="defaulted"/></w></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_that(ids("|é"), equals("plain"))
    expect_that(ids("|é:first-of-type"), equals("plain"))
})

test_that("a zero-length 'ns' means no namespace map", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    xmldoc <- '<r><b id="one"/><w><b id="two"/></w></r>'
    ids <- function(doc, ns)
        as.character(unlist(lapply(querySelectorAll(doc, "b", ns = ns),
                                   function(node) {
            if (inherits(node, "xml_node"))
                xml2::xml_attr(node, "id")
            else
                XML::xmlGetAttr(node, "id")
        })))

    # xml2 defaults to the document's own namespace map, built by
    # walking the whole document; character(0) says there is nothing to
    # look up, and matches the same nodes in an un-namespaced document
    doc <- xml2::read_xml(xmldoc)
    expect_that(ids(doc, character(0)), equals(c("one", "two")))
    expect_that(ids(doc, list()), equals(c("one", "two")))
    expect_that(ids(doc, NULL), equals(c("one", "two")))
    # and from a node or a node set, not just the document
    root <- xml2::xml_root(doc)
    expect_that(ids(root, character(0)), equals(c("one", "two")))
    expect_that(ids(querySelectorAll(doc, "r, w"), character(0)),
                equals(c("one", "two")))
    expect_that(xml2::xml_attr(querySelector(doc, "b", ns = character(0)), "id"),
                equals("one"))

    # the XML package takes it the same way
    xdoc <- XML::xmlParse(xmldoc)
    expect_that(ids(xdoc, character(0)), equals(c("one", "two")))
    expect_that(ids(XML::getNodeSet(xdoc, "//w"), character(0)), equals("two"))

    # the namespaced functions still require a namespace to filter to
    expect_error(querySelectorAllNS(doc, "b", character(0)),
                 "A namespace must be provided.")
    expect_error(querySelectorNS(doc, "b", list()),
                 "A namespace must be provided.")
})
