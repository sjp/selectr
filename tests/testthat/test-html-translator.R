htmlText <- paste0('<html><head><title>t</title></head><body>',
                   '<DIV class="wrap" lang="en">',
                   '<input type="checkbox" checked>',
                   '<input type="text" disabled>',
                   '<a href="u">l</a>',
                   '</DIV></body></html>')

test_that("XML HTML documents use the html translator by default", {
    skip_if_not_installed("XML")
    doc <- XML::htmlParse(htmlText, asText = TRUE)

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
    doc <- XML::htmlParse(htmlText, asText = TRUE)

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
    doc <- XML::xmlParse('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_equal(length(querySelectorAll(doc, "B")), 1)
    expect_equal(length(querySelectorAll(doc, "b")), 0)
    expect_equal(length(querySelectorAll(doc, "input:checked")), 0)

    node <- querySelector(doc, "a")
    expect_equal(length(querySelectorAll(node, "B")), 1)

    # A node built outside any document, and an empty node set, have
    # no document to inspect, and must not error
    expect_equal(length(querySelectorAll(XML::newXMLNode("a"), "B")), 0)
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "zz"), "B")),
                 0)
})

test_that("XML nodes and node sets of an HTML document are detected", {
    skip_if_not_installed("XML")
    doc <- XML::htmlParse(htmlText, asText = TRUE)
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
    doc <- XML::htmlParse(htmlText, asText = TRUE)
    ns <- c(x = "http://www.w3.org/1999/xhtml")
    # XML::htmlParse() does not put the document in a namespace, so these
    # match nothing; the point is that the translator still applies
    expect_equal(length(querySelectorAllNS(doc, "x|input:checked", ns)),
                 0)
    expect_equal(querySelectorNS(doc, "x|input:checked", ns), NULL)
})

test_that("xml2 HTML documents use the html translator by default", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_html(htmlText)

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
    doc <- xml2::read_html(htmlText)
    node <- querySelector(doc, "div")
    nodeset <- querySelectorAll(doc, "div")

    expect_equal(length(querySelectorAll(node, "input:checked")), 1)
    expect_false(is.null(querySelector(node, "input:checked")))
    expect_equal(length(querySelectorAll(nodeset, "input:checked")), 1)
    expect_false(is.null(querySelector(nodeset, "input:checked")))
})

test_that("xml2 XML documents keep the generic translator", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml('<a><B/><input type="checkbox" checked="checked"/></a>')
    expect_equal(length(querySelectorAll(doc, "B")), 1)
    expect_equal(length(querySelectorAll(doc, "b")), 0)
    expect_equal(length(querySelectorAll(doc, "input:checked")), 0)

    node <- querySelector(doc, "a")
    expect_equal(length(querySelectorAll(node, "B")), 1)

    # An empty node set and a missing node have no document to
    # inspect, and must not error
    expect_equal(length(querySelectorAll(querySelectorAll(doc, "zz"), "B")),
                 0)
    expect_equal(length(querySelectorAll(xml2::xml_find_first(doc, "//zz"), "B")),
                 0)
})

test_that("css_to_xpath() still defaults to the generic translator", {
    expect_equal(css_to_xpath("input:checked"),
                 css_to_xpath("input:checked", translator = "generic"))
})

test_that("the xhtml translator reads xml:lang as well as lang", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(paste0(
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
        xml2::xml_attr(querySelectorAll(doc, selector, translator = "xhtml"), "id")

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

test_that("attribute values match case-insensitively in an HTML document", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_html(paste0(
        '<html><body><ul>',
        '<li id="a" lang="En-us"><input id="r" type="RADIO"></li>',
        '<li id="b" class="Wrap" data-type="Foo"><a id="t" rel="TAG"></a></li>',
        '</ul></body></html>'))

    pid <- function(selector)
        xml2::xml_attr(querySelectorAll(doc, selector), "id")

    # 'type', 'rel' and 'lang' are on HTML's case-insensitive list
    expect_equal(pid("input[type=radio]"), "r")
    expect_equal(pid("a[rel=tag]"), "t")
    expect_equal(pid('[lang|="en"]'), "a")
    expect_equal(pid('[lang|="en-US"]'), "a")

    # ... and the 's' flag asks for the exact comparison back
    expect_equal(pid("input[type=radio s]"), character(0))
    expect_equal(pid("input[type=RADIO s]"), "r")

    # 'class' and 'data-*' are not on it
    expect_equal(pid('[class="wrap"]'), character(0))
    expect_equal(pid('[data-type="foo"]'), character(0))
})

test_that("the html translator stays lang-only", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_html(paste0('<html><body><p id="x" xml:lang="en">a</p>',
                            '<p id="y" lang="en">b</p></body></html>'))
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "p:lang(en)"), "id"),
                 "y")
})

test_that("a reversed combinator step names the folded HTML element", {
    ht <- HTMLTranslator$new()
    xt <- HTMLTranslator$new(xhtml = TRUE)

    # The HTML translator lowercases the name before it becomes the node
    # test of the axis step, just as it does for a path step; XHTML
    # keeps the case it was given
    expect_equal(ht$css_to_xpath(":is(DIV.X > SPAN ~ P)", prefix = ""),
                 paste0("*[self::p and preceding-sibling::span[parent::div",
                        "[contains(concat(' ', normalize-space(@class), ' '),",
                        " ' X ')]]]"))
    expect_equal(xt$css_to_xpath(":is(Em B)", prefix = ""),
                 "*[self::B and ancestor::Em]")
    expect_equal(ht$css_to_xpath(":is(SPAN + P)", prefix = ""),
                 "*[self::p and preceding-sibling::*[1][self::span]]")

    markup <- paste0('<html><body><DIV id="hd"><SPAN id="hs1"></SPAN>',
                     '<P id="hp1"></P><p id="hp2"><Em id="he"><b id="hb">x',
                     '</b></Em></p><span id="hs2"></span><P id="hp3"></P>',
                     '</DIV></body></html>')
    cases <- list(":is(SPAN ~ P)" = c("hp1", "hp2", "hp3"),
                  ":is(Div > p)" = c("hp1", "hp2", "hp3"),
                  ":is(DIV P EM b)" = "hb",
                  ":not(div *):is(* > div)" = "hd")
    if (requireNamespace("xml2", quietly = TRUE)) {
        doc <- xml2::read_html(markup)
        for (sel in names(cases))
            expect_equal(xml2::xml_attr(querySelectorAll(doc, sel), "id"),
                         cases[[sel]], info = sel)
    }
    if (requireNamespace("XML", quietly = TRUE)) {
        doc <- XML::htmlParse(markup, asText = TRUE)
        for (sel in names(cases))
            expect_equal(vapply(querySelectorAll(doc, sel), XML::xmlGetAttr,
                                character(1), "id"),
                         cases[[sel]], info = sel)
    }
})

test_that("an 'of S' sibling step names the folded HTML element", {
    ht <- HTMLTranslator$new()
    xt <- HTMLTranslator$new(xhtml = TRUE)

    # The HTML translator lowercases the name before it becomes the node
    # test of the sibling step; XHTML keeps the case it was given
    expect_equal(ht$css_to_xpath(":nth-child(2n+1 of LI)", prefix = ""),
                 "*[self::li and count(preceding-sibling::li) mod 2 = 0]")
    expect_equal(xt$css_to_xpath(":nth-child(2n+1 of LI)", prefix = ""),
                 "*[self::LI and count(preceding-sibling::LI) mod 2 = 0]")
    expect_equal(ht$css_to_xpath(":nth-last-child(2 of Li)", prefix = ""),
                 paste0("*[self::li and following-sibling::li[1] and ",
                        "not(following-sibling::li[2])]"))
    expect_equal(ht$css_to_xpath(":nth-child(odd of *|LI)", prefix = ""),
                 paste0("*[local-name() = 'li' and count(preceding-sibling",
                        "::*[local-name() = 'li']) mod 2 = 0]"))

    markup <- paste0('<html><body><UL id="u"><LI id="h1"></LI><P id="hp1">',
                     '</P><li id="h2" class="X"></li><LI id="h3"></LI>',
                     '<p id="hp2"></p><Li id="h4" class="X"></Li>',
                     '<li id="h5"></li></UL></body></html>')
    cases <- list(":nth-child(2n+1 of LI)" = c("h1", "h3", "h5"),
                  ":nth-last-child(2 of Li)" = "h4",
                  ":nth-child(-n+2 of li.X)" = c("h2", "h4"),
                  ":nth-last-child(odd of UL > LI)" = c("h1", "h3", "h5"))
    if (requireNamespace("xml2", quietly = TRUE)) {
        doc <- xml2::read_html(markup)
        for (sel in names(cases))
            expect_equal(xml2::xml_attr(querySelectorAll(doc, sel), "id"),
                         cases[[sel]], info = sel)
    }
    if (requireNamespace("XML", quietly = TRUE)) {
        doc <- XML::htmlParse(markup, asText = TRUE)
        for (sel in names(cases))
            expect_equal(vapply(querySelectorAll(doc, sel), XML::xmlGetAttr,
                                character(1), "id"),
                         cases[[sel]], info = sel)
    }
})
