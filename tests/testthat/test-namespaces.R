test_that("namespace selectors translate faithfully", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # '*|e' matches 'e' in any namespace, including none
    expect_equal(xpath("*|e"), "*[local-name() = 'e']")
    # '|e' matches 'e' in no namespace, which is what an unprefixed
    # XPath name test already means
    expect_equal(xpath("|e"), "e")
    # '|e' with a name unusable as an XPath name test must still pin the
    # null namespace: a bare name() test is also unprefixed for an
    # element in a default namespace.  'e' means the same thing, so it
    # gets the same pin
    expect_equal(xpath("|é"),
                 "*[name() = 'é' and namespace-uri() = '']")
    expect_equal(xpath("é"),
                 "*[name() = 'é' and namespace-uri() = '']")
    # A prefixed name unusable as an XPath name test keeps the prefix in
    # the node test, where the evaluator's namespace map still resolves
    # it, and compares the local part alone
    expect_equal(xpath("ns|é"), "ns:*[local-name() = 'é']")
    # The node test left on the path step is the namespaced wildcard,
    # but the compound still names a type, so the of-type pseudo-classes
    # count siblings by prefix and local name rather than refusing it as
    # they do for 'ns|*' itself
    expect_equal(xpath("ns|é:first-of-type"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "count(preceding-sibling::",
                        "ns:*[local-name() = 'é']) = 0]"))
    expect_equal(xpath("ns|é:nth-of-type(2)"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "count(preceding-sibling::",
                        "ns:*[local-name() = 'é']) = 1]"))
    expect_equal(xpath("svg|di\\[v:last-of-type"),
                 paste0("svg:*[local-name() = 'di[v' and ",
                        "count(following-sibling::",
                        "svg:*[local-name() = 'di[v']) = 0]"))
    expect_equal(xpath("ns|é:only-of-type"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "count(preceding-sibling::",
                        "ns:*[local-name() = 'é']) = 0 and ",
                        "count(following-sibling::",
                        "ns:*[local-name() = 'é']) = 0]"))
    # 'ns|*' names no type and is still refused
    expect_error(xpath("ns|*:first-of-type"),
                 "\\*:first-of-type is not implemented")
    # '*|*' is equivalent to '*'
    expect_equal(xpath("*|*"), "*")
    # '|*' matches any element in no namespace
    expect_equal(xpath("|*"), "*[namespace-uri() = '']")
    # 'ns|e' defers prefix-to-URI binding to evaluation time
    expect_equal(xpath("ns|e"), "ns:e")

    # Attribute selectors
    expect_equal(xpath("[*|a]"), "*[@*[local-name() = 'a']]")
    expect_equal(xpath("[*|a='v']"),
                 "*[@*[local-name() = 'a'] = 'v']")
    # Unprefixed attribute names have no namespace, so '[|a]' is
    # equivalent to '[a]'
    expect_equal(xpath("[|a]"), "*[@a]")
    expect_equal(xpath("[|a='v']"), "*[@a = 'v']")
    expect_equal(xpath("[ns|a]"), "*[@ns:a]")
    # An attribute name unusable as an XPath name test is compared with
    # name(), which needs no namespace pin: an unprefixed attribute has
    # no namespace, unlike an element in a default namespace
    expect_equal(xpath("[\\31 href]"),
                 "*[attribute::*[name() = '1href']]")
    # An unsafe local name keeps the prefix in the node test, as on the
    # element path
    expect_equal(xpath("[ns|\\31 href]"),
                 "*[@ns:*[local-name() = '1href']]")
    expect_equal(xpath("[ns|\\31 href='v']"),
                 "*[@ns:*[local-name() = '1href'] = 'v']")

    # Composability
    expect_equal(xpath(":not(*|e)"), "*[not(local-name() = 'e')]")
    expect_equal(xpath("div > *|e"), "div/*[local-name() = 'e']")

    # On the right of '+' the name becomes a node test on the self
    # axis, which like the bare name test in a path step matches an
    # unprefixed name in the null namespace only; '|e' and 'e'
    # coincide there too
    expect_equal(xpath("e + f"),
                 "e/following-sibling::*[1][self::f]")
    expect_equal(xpath("e + |f"),
                 "e/following-sibling::*[1][self::f]")
    expect_equal(xpath("e + ns|f"),
                 "e/following-sibling::*[1][self::ns:f]")
    expect_equal(xpath("e + *|f"),
                 "e/following-sibling::*[1][local-name() = 'f']")

    # Inside pseudo-class arguments, prefixed names keep resolving
    # through the namespace map (a name test on the self axis or the
    # path step itself), rather than comparing against the document's
    # literal prefix with name()
    expect_equal(xpath(":is(ns|e)"), "*[self::ns:e]")
    expect_equal(xpath(":not(ns|e)"), "*[not(self::ns:e)]")
    expect_equal(xpath(":has(ns|e)"), "*[.//ns:e]")
    expect_equal(xpath(":has(> ns|e)"), "*[child::ns:e]")
    # Under '+' the position predicate [1] must precede the name test
    # ("the next sibling, if it is an ns:e"), so the name cannot stay
    # on the path step
    expect_equal(xpath(":has(+ ns|e)"),
                 "*[following-sibling::*[1][self::ns:e]]")

    # 'ns|*' is a node test too ('*' is a valid local part), not an
    # unsafe name: stringifying it as name() = 'ns:*' could never
    # match, as name() never returns a literal '*'
    expect_equal(xpath(":is(ns|*)"), "*[self::ns:*]")
    expect_equal(xpath(":not(ns|*)"), "*[not(self::ns:*)]")
    expect_equal(xpath(":has(ns|*)"), "*[.//ns:*]")
})

test_that("namespace selector specificity is correct", {
    spec <- function(css) parse(css)[[1]]$specificity()

    # Universal selectors and namespace components contribute nothing
    expect_equal(spec("*|e"), c(0, 0, 1))
    expect_equal(spec("|e"), c(0, 0, 1))
    expect_equal(spec("*|*"), c(0, 0, 0))
    expect_equal(spec("|*"), c(0, 0, 0))
})

test_that("a prefix that is not an XPath name is rejected", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x, prefix = "")

    # A prefix is written into the node test as it stands, so it has to
    # be a name XPath can parse. Unlike a local name, it has no
    # fallback: comparing 'prefix:name' against name() would test how
    # the document spells the prefix, where every prefix the translator
    # emits is resolved by what the caller bound it to
    expect_error(css("\\31 ns|div"),
                 "The namespace prefix '1ns' is not an XPath name")
    expect_error(css("[\\31 ns|href]"),
                 "The namespace prefix '1ns' is not an XPath name")
    expect_error(css("ns\\:x|div"),
                 "The namespace prefix 'ns:x' is not an XPath name")
    expect_error(css("ns\\|a|b"),
                 "The namespace prefix 'ns\\|a' is not an XPath name")
    expect_error(css("\\D800|a"), "is not an XPath name")

    # The condition is an XML NCName, which is not restricted to ASCII:
    # a prefix XPath can name is passed through whatever its script
    expect_equal(css("äöü|a"), "äöü:a")
    expect_equal(css("[äöü|a]"), "*[@äöü:a]")
    # ... but only as XML 1.0 spelled it, which is what libxml2 (behind
    # both the XML and the xml2 package) parses: the Fifth Edition
    # widened the tables, and a prefix accepted here has to parse
    # everywhere
    expect_error(css("nsɂ|a"), "is not an XPath name")

    # A translation error, so it carries the usual fields. There is no
    # CSS that spells this construct, so 'feature' is the phrase the
    # message reads as rather than a fragment of the selector, and
    # there is no one position to point at either
    err <- tryCatch(css("\\31 ns|div"), selectr_translation_error = identity)
    expect_equal(err$feature,
                 "a namespace prefix that is not an XPath name (`1ns`)")
    expect_equal(err$selector, "\\31 ns|div")
    expect_null(err$pos)
})

test_that("an escaped '*' is a prefix named '*', not the any-namespace one", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x, prefix = "")

    # Selectors 4 6.2: an <ns-prefix> is '[ <ident-token> | "*" ]? "|"'.
    # Only the delimiter '*' is the any-namespace wildcard; an
    # identifier that decodes to the same character names a prefix
    # '*', and no @namespace rule can bind it because it is not an
    # NCName. So it takes the same route as any other prefix XPath
    # cannot name, rather than silently widening to every namespace
    for (sel in c("\\2a|a", "\\*|a", "\\2a |a", "\\2a|*")) {
        expect_error(css(sel),
                     "The namespace prefix '\\*' is not an XPath name",
                     info = sel)
    }
    # ... and an attribute prefix is read the same way
    for (sel in c("[\\2a|href]", "[\\*|href]", "[\\2a|href=\"x\"]")) {
        expect_error(css(sel),
                     "The namespace prefix '\\*' is not an XPath name",
                     info = sel)
    }

    # A translation error, so it carries the usual fields
    err <- tryCatch(css("\\2a|a"), selectr_translation_error = identity)
    expect_equal(err$feature,
                 "a namespace prefix that is not an XPath name (`*`)")
    expect_equal(err$selector, "\\2a|a")

    # The delimiter is untouched: these still mean any namespace
    expect_equal(css("*|a"), "*[local-name() = 'a']")
    expect_equal(css("*|*"), "*")
    expect_equal(css("[*|href]"), "*[@*[local-name() = 'href']]")
    # An escaped '*' elsewhere is an ordinary name, as before
    expect_equal(css("\\2a"), "*[name() = '*' and namespace-uri() = '']")
    expect_equal(css("ns|\\2a"), "ns:*[local-name() = '*']")
    # '*|\2a' is an element named '*' in any namespace. The local name
    # is told apart from the universal selector the same way the prefix
    # is: by which token it came from, not by the character it spells
    expect_equal(css("*|\\2a"), "*[local-name() = '*']")
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

    expect_equal(matches("*|e"), c("e", "svg:e"))
    expect_equal(matches("|e"), "e")
    expect_equal(matches("|*"), c("r", "e"))
    expect_equal(matches("*|*"), c("r", "e", "svg:e"))
    expect_equal(matches("svg|e"), "svg:e")
    expect_equal(matches("[*|a]"), c("r", "svg:e"))
    expect_equal(matches("[|a]"), "r")
})

test_that("an of-type pseudo-class counts a prefixed unsafe name", {
    skip_if_not_installed("xml2")

    # 'é' cannot be an XPath name test, so the step is 'ns:*' with the
    # local name in a condition; the of-type pseudo-classes must still
    # count siblings of that type, not every element in the namespace
    doc <- xml2::read_xml(paste0(
        '<r xmlns:n="http://ns">',
        '<n:f id="f1"/><n:é id="e1"/><n:f id="f2"/><n:é id="e2"/></r>'))
    ns <- c(ns = "http://ns")
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_attr(nodes, "id")
    }

    expect_equal(ids("ns|é:first-of-type"), "e1")
    expect_equal(ids("ns|é:last-of-type"), "e2")
    expect_equal(ids("ns|é:nth-of-type(2)"), "e2")
    expect_equal(ids("ns|é:only-of-type"), character(0))
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

    expect_equal(ids("svg|g"), "g1")
    expect_equal(ids(":is(svg|g)"), "g1")
    expect_equal(ids(":not(svg|g)"), c(NA, "b1")) # r and b
    expect_equal(ids(":is(svg|*)"), "g1")
    expect_equal(ids(":not(svg|*)"), c(NA, "b1")) # r and b
    expect_equal(xml2::xml_name(xml2::xml_find_all(
                     doc, css_to_xpath(":has(svg|g)", prefix = "//"), ns)),
                 "r")
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
    expect_equal(ids("p"), "plain")
    expect_equal(ids(":is(p)"), "plain")
    expect_equal(ids(":where(p)"), "plain")
    expect_equal(ids("*:not(p)"),
                 c("root", "plain-sib", "wrapper",
                   "defaulted", "defaulted-sib"))

    # so do the combinators, on either side and in either direction
    expect_equal(ids("r > p"), "plain")
    expect_equal(ids(":is(r > p)"), "plain")
    expect_equal(ids("p + span"), "plain-sib")
    expect_equal(ids(":is(p + span)"), "plain-sib")
    expect_equal(ids("p ~ span"), "plain-sib")
    expect_equal(ids(":is(p ~ span)"), "plain-sib")
    expect_equal(ids(":has(p)"), "root")
    expect_equal(ids(":has(> p)"), "root")
    expect_equal(ids(":has(+ span)"), "plain")

    # a prefix bound to the default namespace reaches the rest,
    # again in both positions
    expect_equal(ids("d|p"), "defaulted")
    expect_equal(ids(":is(d|p)"), "defaulted")
    expect_equal(ids(":has(d|p)"), c("root", "wrapper"))
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

    expect_equal(ids("p + span"), "plain")
    expect_equal(ids("p ~ span"), "plain")
    # '|span' pins the null namespace explicitly: same elements
    expect_equal(ids("p + |span"), "plain")
    # the universal selector matches either way
    expect_equal(ids("p + *"), c("defaulted", "plain"))
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

    expect_equal(ids("|é"), "plain")
    expect_equal(ids("|é:first-of-type"), "plain")
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
    expect_equal(ids(doc, character(0)), c("one", "two"))
    expect_equal(ids(doc, list()), c("one", "two"))
    expect_equal(ids(doc, NULL), c("one", "two"))
    # and from a node or a node set, not just the document
    root <- xml2::xml_root(doc)
    expect_equal(ids(root, character(0)), c("one", "two"))
    expect_equal(ids(querySelectorAll(doc, "r, w"), character(0)),
                 c("one", "two"))
    expect_equal(xml2::xml_attr(querySelector(doc, "b", ns = character(0)), "id"),
                 "one")

    # the XML package takes it the same way
    xdoc <- XML::xmlParse(xmldoc)
    expect_equal(ids(xdoc, character(0)), c("one", "two"))
    expect_equal(ids(XML::getNodeSet(xdoc, "//w"), character(0)), "two")

    # the namespaced functions still require a namespace to filter to
    expect_error(querySelectorAllNS(doc, "b", character(0)),
                 "A namespace must be provided.")
    expect_error(querySelectorNS(doc, "b", list()),
                 "A namespace must be provided.")
})

test_that("namespace prefixes that are not valid XML names are rejected", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    doc <- xml2::read_xml('<a xmlns:s="urn:s"><s:b/></a>')
    xdoc <- XML::xmlParse('<a xmlns:s="urn:s"><s:b/></a>')

    # The condition is is_ncname(), the same test the translator applies
    # to a prefix written in the selector, so the reach of the two stops
    # in the same place. "ª" and "nsɂ" are Unicode letters outside
    # XML 1.0's Appendix B tables: libxml2 refuses them, and catching
    # them here makes that an argument error rather than an "Invalid
    # expression" over an XPath the caller never wrote
    bad_names <- c("s p", "s:p", "1s", "ª", "nsɂ")
    for (bad in bad_names) {
        ns <- setNames("urn:s", bad)
        expect_error(querySelectorAllNS(doc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorNS(doc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorAllNS(xdoc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorNS(xdoc, "s|b", ns),
                     class = "selectr_argument_error")
    }
})

test_that("a namespace prefix may be any XML name, not just an ASCII one", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    xmldoc <- '<a xmlns:s="urn:s"><s:b id="one"/><b id="two"/></a>'
    doc <- xml2::read_xml(xmldoc)
    xdoc <- XML::xmlParse(xmldoc)
    ns <- setNames("urn:s", "é")

    # A prefix the parser accepts ('äöü|a' above) is accepted as a
    # binding too, so the two entry points meet: the prefix is spliced
    # into the node test and into the descendant-or-self filter, and
    # libxml2 parses both
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "é|b", ns = ns), "id"),
                 "one")
    expect_equal(xml2::xml_attr(querySelectorAllNS(doc, "é|b", ns), "id"),
                 "one")
    expect_equal(xml2::xml_attr(querySelectorNS(doc, "é|b", ns), "id"), "one")
    ids <- function(nodes) vapply(nodes, XML::xmlGetAttr, character(1), "id")
    expect_equal(ids(querySelectorAll(xdoc, "é|b", ns = ns)), "one")
    expect_equal(ids(querySelectorAllNS(xdoc, "é|b", ns)), "one")
    expect_equal(XML::xmlGetAttr(querySelectorNS(xdoc, "é|b", ns), "id"), "one")

    # The binding is by URI, so the document's own prefix is irrelevant
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "é|*", ns = ns), "id"),
                 "one")
})
