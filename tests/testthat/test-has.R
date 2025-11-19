context(":has() pseudo-class")

test_that(":has() generates correct XPath", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # Simple :has() with element
    expect_that(xpath("div:has(p)"),
                equals("div[(.//*[(name() = 'p')])]"))

    # :has() with class selector
    expect_that(xpath("div:has(.foo)"),
                equals("div[(.//*[(@class and contains(concat(' ', normalize-space(@class), ' '), ' foo '))])]"))

    # :has() with ID selector
    expect_that(xpath("section:has(#main)"),
                equals("section[(.//*[(@id = 'main')])]"))

    # :has() with attribute selector
    expect_that(xpath("form:has([required])"),
                equals("form[(.//*[(@required)])]"))

    # :has() with multiple selectors (OR logic)
    expect_that(xpath("div:has(p, span)"),
                equals("div[(.//*[(name() = 'p')] | .//*[(name() = 'span')])]"))

    # Multiple :has() selectors
    expect_that(xpath("div:has(p):has(span)"),
                equals("div[(.//*[(name() = 'p')]) and (.//*[(name() = 'span')])]"))

    # :has() on universal selector
    expect_that(xpath("*:has(img)"),
                equals("*[(.//*[(name() = 'img')])]"))

    # Complex: :has() with class on descendant
    expect_that(xpath("section:has(div.content)"),
                equals("section[(.//*[(@class and contains(concat(' ', normalize-space(@class), ' '), ' content ')) and (name() = 'div')])]"))
})

test_that(":has() works correctly with XML documents", {
    library(XML)

    # Create test document
    html <- paste0(
        '<root>',
        '  <section id="s1">',
        '    <div class="content">',
        '      <p>Paragraph in section 1</p>',
        '    </div>',
        '  </section>',
        '  <section id="s2">',
        '    <div class="sidebar">',
        '      <span>Span in section 2</span>',
        '    </div>',
        '  </section>',
        '  <section id="s3">',
        '    <header>',
        '      <h1>Title</h1>',
        '    </header>',
        '  </section>',
        '  <article id="a1">',
        '    <p>Article paragraph</p>',
        '  </article>',
        '</root>'
    )

    doc <- xmlRoot(xmlParse(html))

    # Helper to get IDs
    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # Section containing a p element
    expect_that(get_ids("section:has(p)"),
                equals("s1"))

    # Section containing a div
    expect_that(get_ids("section:has(div)"),
                equals(c("s1", "s2")))

    # Section containing an h1
    expect_that(get_ids("section:has(h1)"),
                equals("s3"))

    # Section with div.content
    expect_that(get_ids("section:has(div.content)"),
                equals("s1"))

    # Section with div.sidebar
    expect_that(get_ids("section:has(div.sidebar)"),
                equals("s2"))

    # Any element containing a p
    # Note: XML returns root element too since it's also ancestor
    ids <- get_ids(":has(p)")
    expect_that("s1" %in% ids && "a1" %in% ids, equals(TRUE))

    # Multiple selectors: section with p OR span
    expect_that(get_ids("section:has(p, span)"),
                equals(c("s1", "s2")))

    # Chained :has() - section with both div and p
    expect_that(get_ids("section:has(div):has(p)"),
                equals("s1"))

    # :has() should not match the element itself
    expect_that(length(querySelectorAll(doc, "p:has(p)")),
                equals(0))
})

test_that(":has() works correctly with xml2 documents", {
    library(xml2)

    # Create test document
    html <- paste0(
        '<root>',
        '  <section id="s1">',
        '    <div class="content">',
        '      <p>Paragraph in section 1</p>',
        '    </div>',
        '  </section>',
        '  <section id="s2">',
        '    <div class="sidebar">',
        '      <span>Span in section 2</span>',
        '    </div>',
        '  </section>',
        '  <section id="s3">',
        '    <header>',
        '      <h1>Title</h1>',
        '    </header>',
        '  </section>',
        '  <article id="a1">',
        '    <p>Article paragraph</p>',
        '  </article>',
        '</root>'
    )

    doc <- read_xml(html)

    # Helper to get IDs
    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml_attr(results, "id")
    }

    # Section containing a p element
    expect_that(get_ids("section:has(p)"),
                equals("s1"))

    # Section containing a div
    expect_that(get_ids("section:has(div)"),
                equals(c("s1", "s2")))

    # Section containing an h1
    expect_that(get_ids("section:has(h1)"),
                equals("s3"))

    # Section with div.content
    expect_that(get_ids("section:has(div.content)"),
                equals("s1"))

    # Section with div.sidebar
    expect_that(get_ids("section:has(div.sidebar)"),
                equals("s2"))

    # Any element containing a p
    # Note: returns all ancestors including root
    ids <- get_ids(":has(p)")
    expect_that("s1" %in% ids && "a1" %in% ids, equals(TRUE))

    # Multiple selectors: section with p OR span
    expect_that(get_ids("section:has(p, span)"),
                equals(c("s1", "s2")))

    # Chained :has() - section with both div and p
    expect_that(get_ids("section:has(div):has(p)"),
                equals("s1"))

    # :has() should not match the element itself
    expect_that(length(querySelectorAll(doc, "p:has(p)")),
                equals(0))
})

test_that(":has() handles edge cases correctly", {
    library(XML)

    # Empty elements
    html1 <- '<root><div id="d1"></div><div id="d2"><p></p></div></root>'
    doc1 <- xmlRoot(xmlParse(html1))

    # Only d2 has a p descendant
    result1 <- querySelectorAll(doc1, "div:has(p)")
    expect_that(length(result1), equals(1))
    expect_that(xmlGetAttr(result1[[1]], "id"), equals("d2"))

    # Nested :has()
    html2 <- paste0(
        '<root>',
        '  <section id="s1">',
        '    <article>',
        '      <div>',
        '        <p class="highlight">Text</p>',
        '      </div>',
        '    </article>',
        '  </section>',
        '  <section id="s2">',
        '    <article>',
        '      <p>Text</p>',
        '    </article>',
        '  </section>',
        '</root>'
    )
    doc2 <- xmlRoot(xmlParse(html2))

    # Section containing article with div
    result2 <- querySelectorAll(doc2, "section:has(article:has(div))")
    expect_that(length(result2), equals(1))
    expect_that(xmlGetAttr(result2[[1]], "id"), equals("s1"))

    # Section containing p.highlight
    result3 <- querySelectorAll(doc2, "section:has(p.highlight)")
    expect_that(length(result3), equals(1))
    expect_that(xmlGetAttr(result3[[1]], "id"), equals("s1"))

    # :has() with universal selector
    html3 <- '<root><div id="d1"><span/></div><div id="d2"></div></root>'
    doc3 <- xmlRoot(xmlParse(html3))

    # Div that has any descendant
    result4 <- querySelectorAll(doc3, "div:has(*)")
    expect_that(length(result4), equals(1))
    expect_that(xmlGetAttr(result4[[1]], "id"), equals("d1"))
})

test_that(":has() works with querySelector (returns first match)", {
    library(xml2)

    html <- paste0(
        '<root>',
        '  <section id="s1"><p>First</p></section>',
        '  <section id="s2"><p>Second</p></section>',
        '  <section id="s3"><span>Third</span></section>',
        '</root>'
    )

    doc <- read_xml(html)

    # Should return first section with p
    result <- querySelector(doc, "section:has(p)")
    expect_that(xml_attr(result, "id"), equals("s1"))

    # Should return NULL when no match
    result_none <- querySelector(doc, "section:has(article)")
    expect_that(result_none, equals(NULL))
})
