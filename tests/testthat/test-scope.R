context(":scope pseudo-class")

test_that("a leading :scope generates XPath anchored at the context node", {
    # A selector starting with :scope is anchored at the query's
    # scoping root, so the 'self' axis replaces the usual prefix
    expect_that(css_to_xpath(":scope"),
                equals("self::*"))
    expect_that(css_to_xpath(":scope > a"),
                equals("self::*/a"))
    expect_that(css_to_xpath(":scope a"),
                equals("self::*//a"))
    expect_that(css_to_xpath(":scope ~ a"),
                equals("self::*/following-sibling::a"))
    expect_that(css_to_xpath(":scope + a"),
                equals("self::*/following-sibling::*[1][self::a]"))

    # Pseudo-class names are case-insensitive
    expect_that(css_to_xpath(":SCOPE > a"),
                equals("self::*/a"))

    # Other simple selectors in the compound constrain the scoping root
    expect_that(css_to_xpath("div:scope > a"),
                equals("self::div/a"))
    expect_that(css_to_xpath(":scope.foo"),
                equals("self::*[@class and contains(concat(' ', normalize-space(@class), ' '), ' foo ')]"))
    expect_that(css_to_xpath(":scope:first-child"),
                equals("self::*[count(preceding-sibling::*) = 0]"))

    # In a selector list only the scoped selector is anchored
    expect_that(css_to_xpath(":scope > a, b"),
                equals("self::*/a | descendant-or-self::b"))

    # The 'self' axis replaces the prefix whatever its value: the
    # scoping root is the context node by definition
    expect_that(css_to_xpath(":scope > a", prefix = "//"),
                equals("self::*/a"))
    expect_that(css_to_xpath(":scope > a", prefix = ""),
                equals("self::*/a"))

    # Inherited unchanged by the HTML translator
    expect_that(css_to_xpath(":scope > a", translator = "html"),
                equals("self::*/a"))
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
    expect_error(css_to_xpath(":where(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(:scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(> :scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":has(a > :scope)"), err, fixed = TRUE)
    expect_error(css_to_xpath(":nth-child(2 of :scope)"), err, fixed = TRUE)
})

test_that(":scope works correctly with XML documents", {
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
    expect_that(get_ids(section, ":scope > a"),
                equals(c("a1", "a3")))

    # All descendants of the queried node
    expect_that(get_ids(section, ":scope a"),
                equals(c("a1", "a2", "a3")))

    # Siblings following the queried node
    expect_that(get_ids(section, ":scope ~ a"),
                equals("a4"))

    # A bare :scope matches the queried node itself
    scope <- querySelector(section, ":scope")
    expect_that(xmlGetAttr(scope, "id"), equals("s1"))

    # A :scope constrained by other simple selectors only matches if
    # the queried node does
    expect_that(get_ids(section, "section:scope > a"),
                equals(c("a1", "a3")))
    expect_that(length(querySelectorAll(section, "div:scope > a")),
                equals(0))

    # For a document, the scoping root is the root element
    expect_that(get_ids(doc, ":scope > section"),
                equals("s1"))
})

test_that(":scope works correctly with xml2 documents", {
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
    expect_that(get_ids(section, ":scope > a"),
                equals(c("a1", "a3")))

    # All descendants of the queried node
    expect_that(get_ids(section, ":scope a"),
                equals(c("a1", "a2", "a3")))

    # Siblings following the queried node
    expect_that(get_ids(section, ":scope ~ a"),
                equals("a4"))

    # A bare :scope matches the queried node itself
    scope <- querySelector(section, ":scope")
    expect_that(xml_attr(scope, "id"), equals("s1"))

    # A :scope constrained by other simple selectors only matches if
    # the queried node does
    expect_that(get_ids(section, "section:scope > a"),
                equals(c("a1", "a3")))
    expect_that(length(querySelectorAll(section, "div:scope > a")),
                equals(0))

    # For a document, the scoping root is the root element
    expect_that(get_ids(doc, ":scope > section"),
                equals("s1"))
})
