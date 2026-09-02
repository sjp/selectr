test_that("a leading :scope generates XPath anchored at the context node", {
    # A selector starting with :scope is anchored at the query's
    # scoping root, so the 'self' axis replaces the usual prefix
    expect_equal(css_to_xpath(":scope"),
                 "self::*")
    expect_equal(css_to_xpath(":scope > a"),
                 "self::*/a")
    expect_equal(css_to_xpath(":scope a"),
                 "self::*//a")
    expect_equal(css_to_xpath(":scope ~ a"),
                 "self::*/following-sibling::a")
    expect_equal(css_to_xpath(":scope + a"),
                 "self::*/following-sibling::*[1][self::a]")

    # Pseudo-class names are case-insensitive
    expect_equal(css_to_xpath(":SCOPE > a"),
                 "self::*/a")

    # Other simple selectors in the compound constrain the scoping root
    expect_equal(css_to_xpath("div:scope > a"),
                 "self::div/a")
    expect_equal(css_to_xpath(":scope.foo"),
                 "self::*[@class and contains(concat(' ', normalize-space(@class), ' '), ' foo ')]")
    expect_equal(css_to_xpath(":scope:first-child"),
                 "self::*[count(preceding-sibling::*) = 0]")

    # In a selector list only the scoped selector is anchored
    expect_equal(css_to_xpath(":scope > a, b"),
                 "self::*/a | descendant-or-self::b")

    # The 'self' axis replaces the prefix whatever its value: the
    # scoping root is the context node by definition
    expect_equal(css_to_xpath(":scope > a", prefix = "//"),
                 "self::*/a")
    expect_equal(css_to_xpath(":scope > a", prefix = ""),
                 "self::*/a")

    # Inherited unchanged by the HTML translator
    expect_equal(css_to_xpath(":scope > a", translator = "html"),
                 "self::*/a")
})

test_that("a non-leading :scope is rejected", {
    err <- "The pseudo-class :scope is only supported at the start of a selector"

    # To the right of a combinator there is no XPath 1.0 equivalent
    expect_error(css_to_xpath("a > :scope"), err, fixed = TRUE)
    expect_error(css_to_xpath("a :scope"), err, fixed = TRUE)
    expect_error(css_to_xpath("a ~ :scope"), err, fixed = TRUE)
    expect_error(css_to_xpath("a + :scope"), err, fixed = TRUE)
    expect_error(css_to_xpath(":scope > :scope"), err, fixed = TRUE)

    # Nor inside functional pseudo-class arguments
    expect_error(css_to_xpath(":not(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":is(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":is(:scope > a)"), err, fixed = TRUE)
    # :scope as the rightmost compound of the chain, not just the left
    expect_error(css_to_xpath(":is(a > :scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":where(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(> :scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(a > :scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":nth-child(2 of :scope)"), err, fixed = TRUE)
})

test_that(":scope works correctly with XML documents", {
    skip_if_not_installed("XML")
    library(XML)

    xml <- paste0(
        '<root>',
        '<section id="s1">',
        '<a id="a1"/>',
        '<div id="d1"><a id="a2"/></div>',
        '<a id="a3"/>',
        '</section>',
        '<a id="a4"/>',
        '</root>'
    )

    doc <- xmlParse(xml)
    section <- getNodeSet(doc, "//section")[[1]]

    get_ids <- function(node, css) {
        results <- querySelectorAll(node, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # Only the children of the queried node, not all descendants and
    # not the document's other 'a' elements
    expect_equal(get_ids(section, ":scope > a"),
                 c("a1", "a3"))

    # All descendants of the queried node
    expect_equal(get_ids(section, ":scope a"),
                 c("a1", "a2", "a3"))

    # Siblings following the queried node
    expect_equal(get_ids(section, ":scope ~ a"),
                 "a4")

    # A bare :scope matches the queried node itself
    scope <- querySelector(section, ":scope")
    expect_equal(xmlGetAttr(scope, "id"), "s1")

    # A :scope constrained by other simple selectors only matches if
    # the queried node does
    expect_equal(get_ids(section, "section:scope > a"),
                 c("a1", "a3"))
    expect_equal(length(querySelectorAll(section, "div:scope > a")),
                 0)

    # For a document, the scoping root is the root element itself,
    # not the document node, so a bare ':scope' matches the root and
    # ':scope > x' looks for a child of the root named 'x' -- unlike
    # the DOM, where document.querySelectorAll(':scope > html') finds
    # the root element and a bare ':scope' matches nothing (see #016)
    expect_equal(get_ids(doc, ":scope > section"),
                 "s1")
    root <- querySelector(doc, ":scope")
    expect_equal(xmlName(root), "root")
    expect_equal(length(querySelectorAll(doc, ":scope > root")),
                 0)
})

test_that(":scope works correctly with xml2 documents", {
    skip_if_not_installed("xml2")
    library(xml2)

    xml <- paste0(
        '<root>',
        '<section id="s1">',
        '<a id="a1"/>',
        '<div id="d1"><a id="a2"/></div>',
        '<a id="a3"/>',
        '</section>',
        '<a id="a4"/>',
        '</root>'
    )

    doc <- read_xml(xml)
    section <- xml_find_first(doc, "//section")

    get_ids <- function(node, css) {
        results <- querySelectorAll(node, css)
        xml_attr(results, "id")
    }

    # Only the children of the queried node, not all descendants and
    # not the document's other 'a' elements
    expect_equal(get_ids(section, ":scope > a"),
                 c("a1", "a3"))

    # All descendants of the queried node
    expect_equal(get_ids(section, ":scope a"),
                 c("a1", "a2", "a3"))

    # Siblings following the queried node
    expect_equal(get_ids(section, ":scope ~ a"),
                 "a4")

    # A bare :scope matches the queried node itself
    scope <- querySelector(section, ":scope")
    expect_equal(xml_attr(scope, "id"), "s1")

    # A :scope constrained by other simple selectors only matches if
    # the queried node does
    expect_equal(get_ids(section, "section:scope > a"),
                 c("a1", "a3"))
    expect_equal(length(querySelectorAll(section, "div:scope > a")),
                 0)

    # For a document, the scoping root is the root element itself,
    # not the document node -- see the identical XML case above (#016)
    expect_equal(get_ids(doc, ":scope > section"),
                 "s1")
    root <- querySelector(doc, ":scope")
    expect_equal(xml_name(root), "root")
    expect_equal(length(querySelectorAll(doc, ":scope > root")),
                 0)
})
