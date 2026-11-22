test_that("an argument leaves out a name test its compound already makes", {
    xpath <- function(css) css_to_xpath(css, prefix = "")
    cls_x <- "contains(concat(' ', normalize-space(@class), ' '), ' x ')"

    # The compound's node test already selects only 'li', so the
    # argument's 'self::li' is always true there, and an argument that
    # only repeats the name leaves no predicate behind
    expect_equal(xpath("li:is(li)"), "li")
    expect_equal(xpath("li:where(li)"), "li")
    expect_equal(xpath("|li:is(li)"), "li")
    expect_equal(xpath("li:is(|li)"), "li")
    expect_equal(xpath("ns|li:is(ns|li)"), "ns:li")
    expect_equal(xpath("li.x:is(li)"), paste0("li[", cls_x, "]"))
    expect_equal(xpath("li:is(li.x)"), paste0("li[", cls_x, "]"))
    expect_equal(xpath("li:is(li):is(li.x)"), paste0("li[", cls_x, "]"))
    # a complex argument keeps the test of what is to its left
    expect_equal(xpath("p:is(span ~ p)"), "p[preceding-sibling::span]")
    expect_equal(xpath("li:is(li > li)"), "li[parent::li]")
    # the candidate's own test against S goes; the sibling count, which
    # selects other elements, keeps its node test
    expect_equal(xpath("li:nth-child(3 of li)"),
                 paste0("li[preceding-sibling::li[2] and ",
                        "not(preceding-sibling::li[3])]"))
    expect_equal(xpath("li:nth-last-child(2 of li)"),
                 paste0("li[following-sibling::li[1] and ",
                        "not(following-sibling::li[2])]"))
    expect_equal(xpath("li:nth-child(2n+1 of li.x)"),
                 paste0("li[", cls_x, " and count(preceding-sibling::li[",
                        cls_x, "]) mod 2 = 0]"))
    expect_equal(xpath("li:nth-child(n of li)"), "li")
    expect_equal(xpath("li:nth-child(odd of li, ns|li)"),
                 paste0("li[count(preceding-sibling::*",
                        "[self::li or self::ns:li]) mod 2 = 0]"))
    # nested arguments, at either level
    expect_equal(xpath("li:is(:is(li))"), "li")
    expect_equal(xpath("li:is(*:is(li))"), "li")
    expect_equal(xpath("li:nth-child(2 of li:is(li))"),
                 paste0("li[preceding-sibling::li[1] and ",
                        "not(preceding-sibling::li[2])]"))
    # a list with an always-true alternative is always true, and the
    # other alternatives keep their tests
    expect_equal(xpath("li:is(li, p)"), "li")
    expect_equal(xpath("li:is(li.x, p)"), paste0("li[", cls_x, " or self::p]"))
    # as a path step after a combinator
    expect_equal(xpath("ul > li:is(li)"), "ul/li")
    expect_equal(xpath("p + li:is(li.x)"),
                 paste0("p/following-sibling::*[1][self::li][", cls_x, "]"))
})

test_that("an argument keeps a name test its compound does not make", {
    xpath <- function(css) css_to_xpath(css, prefix = "")

    # a different name, or the same name under another prefix (two
    # prefixes may be bound to the same namespace, or to different ones)
    expect_equal(xpath("li:is(p)"), "li[self::p]")
    expect_equal(xpath("ns|li:is(li)"), "ns:li[self::li]")
    expect_equal(xpath("li:is(ns|li)"), "li[self::ns:li]")
    expect_equal(xpath("ns|li:is(b|li)"), "ns:li[self::b:li]")
    # a negation
    expect_equal(xpath("li:not(p)"), "li[not(self::p)]")
    expect_equal(xpath("li:not(li)"), "li[not(self::li)]")
    # a compound whose step names no element
    expect_equal(xpath("*:is(li)"), "*[self::li]")
    expect_equal(xpath(":is(li)"), "*[self::li]")
    expect_equal(xpath("ns|*:is(ns|li)"), "ns:*[self::ns:li]")
    expect_equal(xpath("*|li:is(li)"), "*[local-name() = 'li' and self::li]")
    expect_equal(xpath("li:is(*|li)"), "li[local-name() = 'li']")
    expect_equal(xpath("\\31 x:is(\\31 x)"),
                 paste0("*[name() = '1x' and namespace-uri() = '' and ",
                        "name() = '1x' and namespace-uri() = '']"))
    # an alternative nested inside another argument
    expect_equal(xpath("li:is(:is(li, p))"), "li[self::li or self::p]")
    # the sibling step of an 'of S' count and of a reversed combinator
    # test select other elements than the candidate
    expect_equal(xpath("li:nth-child(2n+1 of p)"),
                 "li[self::p and count(preceding-sibling::p) mod 2 = 0]")
    expect_equal(xpath("li:is(li + li)"),
                 "li[preceding-sibling::*[1][self::li]]")
})

test_that("HTML compares the folded name, XHTML the name as written", {
    ht <- HTMLTranslator$new()
    xt <- HTMLTranslator$new(xhtml = TRUE)
    html <- function(css) ht$css_to_xpath(css, prefix = "")
    xhtml <- function(css) xt$css_to_xpath(css, prefix = "")

    expect_equal(html("LI:is(li)"), "li")
    expect_equal(html("li:is(LI)"), "li")
    expect_equal(html("Li:where(LI ~ li)"), "li[preceding-sibling::li]")
    expect_equal(html("LI:nth-child(2 of li)"),
                 paste0("li[preceding-sibling::li[1] and ",
                        "not(preceding-sibling::li[2])]"))
    expect_equal(html("LI:is(p)"), "li[self::p]")

    expect_equal(xhtml("li:is(li)"), "li")
    expect_equal(xhtml("LI:is(li)"), "LI[self::li]")
    expect_equal(xhtml("li:is(LI)"), "li[self::LI]")
    expect_equal(xhtml("Li:where(LI ~ li)"),
                 "Li[self::li and preceding-sibling::LI]")
    expect_equal(xhtml("LI:nth-child(2 of li)"),
                 paste0("LI[self::li and preceding-sibling::li[1] and ",
                        "not(preceding-sibling::li[2])]"))
})

# Elements in no namespace, under two prefixes bound to one namespace, in
# a third namespace and in a default namespace, nested, with classes and
# with case differences, so that dropping a name test the step does not
# imply would change some selector's result
implied_markup <- paste0(
    '<root xmlns:a="urn:a" xmlns:c="urn:a" xmlns:b="urn:b">',
    '<ul id="u1"><li id="l1" class="x"/><a:li id="al1"/><p id="p1"/>',
    '<li id="l2"/><span id="s1"/><b:li id="bl1"/><li id="l3" class="x"/>',
    '<c:li id="cl1" class="x"/><LI id="L1"/><li id="l4"><li id="l5"/>',
    '<p id="p2"/><li id="l6" class="x"/></li><a:span id="as1"/>',
    '<a:li id="al2" class="x"/><li id="l7"/><p id="p3"/></ul>',
    '<ol id="o1" xmlns="urn:a"><li id="dl1"/><li id="dl2" class="x"/>',
    '<p id="dp1"/><li id="dl3"/></ol></root>')
implied_ns <- c(a = "urn:a", c = "urn:a", b = "urn:b")
# Each selector with the XPath it had when every argument's name was
# tested again with 'self::'
implied_cases <- list(
    "li:is(li)" = "//li[self::li]",
    "li:where(li)" = "//li[self::li]",
    "|li:is(li)" = "//li[self::li]",
    "a|li:is(a|li)" = "//a:li[self::a:li]",
    "c|li:is(c|li)" = "//c:li[self::c:li]",
    "li.x:is(li)" = paste0(
        "//li[contains(concat(' ', normalize-space(@class), ' '), ' x ') ",
        "and self::li]"),
    "p:is(span ~ p)" = "//p[self::p and preceding-sibling::span]",
    "li:is(li > li)" = "//li[self::li and parent::li]",
    "li:is(li, p)" = "//li[self::li or self::p]",
    "li:is(li.x, p)" = paste0(
        "//li[contains(concat(' ', normalize-space(@class), ' '), ' x ') ",
        "and self::li or self::p]"),
    "li:is(:is(li))" = "//li[self::li]",
    "li:nth-child(3 of li)" = paste0(
        "//li[self::li and preceding-sibling::li[2] and ",
        "not(preceding-sibling::li[3])]"),
    "li:nth-last-child(2 of li)" = paste0(
        "//li[self::li and following-sibling::li[1] and ",
        "not(following-sibling::li[2])]"),
    "li:nth-child(2 of li:is(li))" =
        "//li[self::li and count(preceding-sibling::li[self::li]) = 1]",
    "li:nth-child(odd of li, a|li)" = paste0(
        "//li[(self::li or self::a:li) and count(preceding-sibling::*",
        "[self::li or self::a:li]) mod 2 = 0]"),
    "li:nth-child(n of li)" = "//li[self::li]",
    "a|li:is(c|li)" = "//a:li[self::c:li]",
    "a|li:is(li)" = "//a:li[self::li]",
    "li:is(p)" = "//li[self::p]",
    "li:not(li)" = "//li[not(self::li)]",
    "*:is(li)" = "//*[self::li]")

test_that("an argument without its implied name test matches as before (xml2)", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(implied_markup)
    found <- function(xp)
        xml2::xml_attr(xml2::xml_find_all(doc, xp, implied_ns), "id")
    for (sel in names(implied_cases)) {
        new <- css_to_xpath(sel, prefix = "//")
        expect_equal(found(new), found(implied_cases[[sel]]), info = sel)
        expect_equal(xml2::xml_attr(querySelectorAll(doc, sel, implied_ns),
                                    "id"),
                     found(new), info = sel)
    }
    expect_gt(length(unlist(lapply(implied_cases, found))), 0)
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "li:is(li)"), "id"),
                 paste0("l", 1:7))
})

test_that("an argument without its implied name test matches as before (XML)", {
    skip_if_not_installed("XML")

    doc <- XML::xmlParse(implied_markup, asText = TRUE)
    found <- function(xp)
        unlist(XML::xpathApply(doc, xp, XML::xmlGetAttr, "id",
                               namespaces = implied_ns))
    for (sel in names(implied_cases)) {
        new <- css_to_xpath(sel, prefix = "//")
        expect_equal(found(new), found(implied_cases[[sel]]), info = sel)
        expect_equal(unlist(lapply(querySelectorAll(doc, sel, implied_ns),
                                   XML::xmlGetAttr, "id")),
                     found(new), info = sel)
    }
    expect_gt(length(unlist(lapply(implied_cases, found))), 0)
    expect_equal(unlist(lapply(querySelectorAll(doc, "li:is(li)"),
                               XML::xmlGetAttr, "id")),
                 paste0("l", 1:7))
})

test_that("HTML drops only the name test the folded step implies", {
    markup <- paste0('<html><body><ul><LI id="h1" class="x"></LI>',
                     '<li id="h2"></li><p id="hp"></p><Li id="h3"></Li>',
                     '<span id="hs"></span><li id="h4" class="x"></li>',
                     '</ul></body></html>')
    cases <- list("LI:is(li)" = c("h1", "h2", "h3", "h4"),
                  "li:is(LI.x)" = c("h1", "h4"),
                  "Li:where(SPAN ~ li)" = "h4",
                  "LI:nth-child(2 of li)" = "h2",
                  "li:is(LI, P)" = c("h1", "h2", "h3", "h4"),
                  "LI:is(p)" = character(0))
    if (requireNamespace("xml2", quietly = TRUE)) {
        doc <- xml2::read_html(markup)
        for (sel in names(cases))
            expect_equal(xml2::xml_attr(querySelectorAll(doc, sel), "id"),
                         cases[[sel]], info = sel)
    }
    if (requireNamespace("XML", quietly = TRUE)) {
        doc <- XML::htmlParse(markup, asText = TRUE)
        for (sel in names(cases))
            expect_equal(as.character(vapply(querySelectorAll(doc, sel),
                                             XML::xmlGetAttr,
                                             character(1), "id")),
                         cases[[sel]], info = sel)
    }
})
