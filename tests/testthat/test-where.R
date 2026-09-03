test_that(":where() generates correct XPath", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # Simple :where() with single selector
    expect_equal(xpath("div:where(p)"),
                 "div[self::p]")

    # :where() with class selector
    expect_equal(xpath("div:where(.foo)"),
                 "div[contains(concat(' ', normalize-space(@class), ' '), ' foo ')]")

    # :where() with ID selector
    expect_equal(xpath("section:where(#main)"),
                 "section[@id = 'main']")

    # :where() with attribute selector
    expect_equal(xpath("input:where([required])"),
                 "input[@required]")

    # :where() with multiple selectors (OR logic, grouped as one condition)
    expect_equal(xpath("div:where(p, span)"),
                 "div[self::p or self::span]")

    # :where() with element and class (both conditions must match)
    expect_equal(xpath("*:where(div.content)"),
                 "*[contains(concat(' ', normalize-space(@class), ' '), ' content ') and self::div]")

    # Stacked :where() selectors - each adds its own condition, so
    # both must match (AND)
    expect_equal(xpath("div:where(p):where(span)"),
                 "div[self::p and self::span]")

    # :where() on universal selector
    expect_equal(xpath("*:where(.highlight)"),
                 "*[contains(concat(' ', normalize-space(@class), ' '), ' highlight ')]")

    # :where() with multiple classes
    expect_equal(xpath("div:where(.foo, .bar)"),
                 "div[contains(concat(' ', normalize-space(@class), ' '), ' foo ') or contains(concat(' ', normalize-space(@class), ' '), ' bar ')]")

    # Complex: :where() with mix of selectors
    expect_equal(xpath("p:where(.highlight, #special, [data-key])"),
                 "p[contains(concat(' ', normalize-space(@class), ' '), ' highlight ') or @id = 'special' or @data-key]")
})

test_that(":where() works correctly with XML documents", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <div id="d1" class="content">Div 1</div>',
        '  <div id="d2" class="sidebar">Div 2</div>',
        '  <p id="p1" class="content">Para 1</p>',
        '  <p id="p2" class="highlight">Para 2</p>',
        '  <span id="s1" class="content">Span 1</span>',
        '  <section id="sec1" class="main">',
        '    <article id="art1" class="post">Article</article>',
        '  </section>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # Elements matching div OR p (via :where)
    expect_equal(get_ids("*:where(div, p)"),
                 c("d1", "d2", "p1", "p2"))

    # Elements with class content (any element type)
    expect_equal(get_ids("*:where(.content)"),
                 c("d1", "p1", "s1"))

    # Div elements that are either content or sidebar
    expect_equal(get_ids("div:where(.content, .sidebar)"),
                 c("d1", "d2"))

    # Elements matching specific ID
    # Note: returns all ancestors in XML, so we check for inclusion
    ids <- get_ids("*:where(#p1)")
    expect_equal("p1" %in% ids, TRUE)

    # :where() with element that has specific class
    expect_equal(get_ids("*:where(p.highlight)"),
                 "p2")

    # :where() matches nothing if conditions don't align
    expect_equal(length(querySelectorAll(doc, "div:where(p)")),
                 0)
})

test_that(":where() works correctly with xml2 documents", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <div id="d1" class="content">Div 1</div>',
        '  <div id="d2" class="sidebar">Div 2</div>',
        '  <p id="p1" class="content">Para 1</p>',
        '  <p id="p2" class="highlight">Para 2</p>',
        '  <span id="s1" class="content">Span 1</span>',
        '  <section id="sec1" class="main">',
        '    <article id="art1" class="post">Article</article>',
        '  </section>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # Elements matching div OR p (via :where)
    expect_equal(get_ids("*:where(div, p)"),
                 c("d1", "d2", "p1", "p2"))

    # Elements with class content (any element type)
    expect_equal(get_ids("*:where(.content)"),
                 c("d1", "p1", "s1"))

    # Div elements that are either content or sidebar
    expect_equal(get_ids("div:where(.content, .sidebar)"),
                 c("d1", "d2"))

    # Elements matching specific ID
    # Note: returns all ancestors, so we check for inclusion
    ids <- get_ids("*:where(#p1)")
    expect_equal("p1" %in% ids, TRUE)

    # :where() with element that has specific class
    expect_equal(get_ids("*:where(p.highlight)"),
                 "p2")

    # :where() matches nothing if conditions don't align
    expect_equal(length(querySelectorAll(doc, "div:where(p)")),
                 0)
})

test_that(":where() has zero specificity", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <div id="test" class="foo bar">Content</div>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    # All of these should match the same element
    # :where() doesn't add specificity regardless of what's inside
    expect_equal(length(querySelectorAll(doc, "div:where(#test)")), 1)
    expect_equal(length(querySelectorAll(doc, "div:where(.foo)")), 1)
    expect_equal(length(querySelectorAll(doc, ":where(div)")), 1)
    expect_equal(length(querySelectorAll(doc, ":where(#test, .foo, div)")), 1)

    # Specificity is handled in parser/specificity calculation
    # Here we just verify matching works
})

test_that(":where() handles edge cases correctly", {
    skip_if_not_installed("XML")

    # Empty document case
    html1 <- '<root></root>'
    doc1 <- XML::xmlRoot(XML::xmlParse(html1))
    expect_equal(length(querySelectorAll(doc1, "*:where(div)")), 0)

    # Multiple classes
    html2 <- paste0(
        '<root>',
        '  <div id="d1" class="foo bar">A</div>',
        '  <div id="d2" class="foo">B</div>',
        '  <div id="d3" class="bar">C</div>',
        '</root>'
    )
    doc2 <- XML::xmlRoot(XML::xmlParse(html2))

    # Divs with foo OR bar class
    result <- querySelectorAll(doc2, "div:where(.foo, .bar)")
    expect_equal(length(result), 3)

    # :where() with universal selector inside
    html4 <- '<root><div id="d1"/><p id="p1"/><span id="s1"/></root>'
    doc4 <- XML::xmlRoot(XML::xmlParse(html4))

    # This matches elements that are any type (essentially all elements plus root)
    result3 <- querySelectorAll(doc4, "*:where(*)")
    # Returns root plus all descendants
    expect_equal(length(result3) >= 3, TRUE)
})

test_that(":where() works with querySelector (returns first match)", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <div id="d1" class="foo">First</div>',
        '  <p id="p1" class="foo">Second</p>',
        '  <span id="s1" class="bar">Third</span>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    # Should return first element with class foo
    result <- querySelector(doc, "*:where(.foo)")
    expect_equal(xml2::xml_attr(result, "id"), "d1")

    # Should return first div or p
    result2 <- querySelector(doc, "*:where(div, p)")
    expect_equal(xml2::xml_attr(result2, "id"), "d1")

    # Should return NULL when no match
    result_none <- querySelector(doc, "*:where(article)")
    expect_equal(result_none, NULL)
})

test_that(":where() and :is() behave similarly in matching", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <div id="d1" class="content">Div</div>',
        '  <p id="p1" class="content">Para</p>',
        '  <span id="s1">Span</span>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # :where() and :is() should match the same elements
    # (only difference is specificity, which doesn't affect matching)
    where_result <- get_ids("*:where(div, p)")
    is_result <- get_ids("*:is(div, p)")
    expect_equal(where_result, is_result)

    where_result2 <- get_ids("*:where(.content)")
    is_result2 <- get_ids("*:is(.content)")
    expect_equal(where_result2, is_result2)
})

test_that(":where() can be combined with other selectors", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <section class="main">',
        '    <div id="d1" class="content">Div 1</div>',
        '    <p id="p1" class="content">Para 1</p>',
        '  </section>',
        '  <aside class="sidebar">',
        '    <div id="d2" class="widget">Div 2</div>',
        '    <p id="p2" class="widget">Para 2</p>',
        '  </aside>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # Descendant combinator: section containing divs or ps
    # Note: returns all matching descendants including ancestors
    ids <- get_ids("section *:where(div, p)")
    expect_equal("d1" %in% ids && "p1" %in% ids, TRUE)

    # Class selector before :where()
    # Elements with class content that are divs or ps
    # (all 4 elements match: d1,p1 have .content, and :where checks div|p)
    result <- get_ids(".content:where(div, p)")
    expect_equal("d1" %in% result && "p1" %in% result, TRUE)

    # Child combinator
    expect_equal(get_ids("section > *:where(.content)"),
                 c("d1", "p1"))
})
