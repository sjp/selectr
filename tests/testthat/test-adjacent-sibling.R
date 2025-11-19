context("adjacent sibling combinator")

test_that("adjacent sibling combinator generates simplified XPath", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # Simple element + element
    expect_that(xpath('a + b'),
                equals("a/following-sibling::*[1][self::b]"))

    # With attribute on right side
    expect_that(xpath('a + b[id]'),
                equals("a/following-sibling::*[1][self::b][(@id)]"))

    # With class on right side
    expect_that(xpath('a + b.test'),
                equals("a/following-sibling::*[1][self::b][(@class and contains(concat(' ', normalize-space(@class), ' '), ' test '))]"))

    # With ID on right side
    expect_that(xpath('a + b#myid'),
                equals("a/following-sibling::*[1][self::b][(@id = 'myid')]"))

    # With multiple attributes on right side
    expect_that(xpath('a + b[id][title]'),
                equals("a/following-sibling::*[1][self::b][(@id) and (@title)]"))

    # With class and attribute on right side
    expect_that(xpath('a + b.test[title]'),
                equals("a/following-sibling::*[1][self::b][(@class and contains(concat(' ', normalize-space(@class), ' '), ' test ')) and (@title)]"))

    # With conditions on both sides
    expect_that(xpath('a.link + b[id]'),
                equals("a[(@class and contains(concat(' ', normalize-space(@class), ' '), ' link '))]/following-sibling::*[1][self::b][(@id)]"))

    expect_that(xpath('a[href] + b.test'),
                equals("a[(@href)]/following-sibling::*[1][self::b][(@class and contains(concat(' ', normalize-space(@class), ' '), ' test '))]"))

    # With ID on left, class and attribute on right
    expect_that(xpath('div#main + p.intro[title]'),
                equals("div[(@id = 'main')]/following-sibling::*[1][self::p][(@class and contains(concat(' ', normalize-space(@class), ' '), ' intro ')) and (@title)]"))

    # Universal selector on right
    expect_that(xpath('h1 + *[rel=up]'),
                equals("h1/following-sibling::*[1][self::*][(@rel = 'up')]"))

    # Combined with child combinator
    expect_that(xpath('div > h1 + p'),
                equals("div/h1/following-sibling::*[1][self::p]"))

    expect_that(xpath('div#main > h1 + p[class]'),
                equals("div[(@id = 'main')]/h1/following-sibling::*[1][self::p][(@class)]"))

    # With descendant combinator
    expect_that(xpath('section a + b'),
                equals("section//a/following-sibling::*[1][self::b]"))

    # Complex: multiple combinators and conditions
    expect_that(xpath('article.post > h2.title + p.intro[data-info]'),
                equals("article[(@class and contains(concat(' ', normalize-space(@class), ' '), ' post '))]/h2[(@class and contains(concat(' ', normalize-space(@class), ' '), ' title '))]/following-sibling::*[1][self::p][(@class and contains(concat(' ', normalize-space(@class), ' '), ' intro ')) and (@data-info)]"))
})

test_that("adjacent sibling combinator works correctly with querySelector", {
    skip_if_not_installed("XML")
    library(XML)

    # Test with immediate adjacent siblings
    doc1 <- htmlParse('<html><body><a id="a1">A</a><b id="b1">B</b></body></html>')
    results1 <- querySelectorAll(doc1, "a + b")
    expect_equal(length(results1), 1)
    expect_equal(xmlGetAttr(results1[[1]], "id"), "b1")

    # Test with intervening element (should NOT match)
    doc2 <- htmlParse('<html><body><a id="a1">A</a><c>C</c><b id="b1">B</b></body></html>')
    results2 <- querySelectorAll(doc2, "a + b")
    expect_equal(length(results2), 0)

    # Test with attributes on right side
    doc3 <- htmlParse('<html><body>
        <a>Link1</a><b id="b1">B1</b>
        <a>Link2</a><b>B2</b>
    </body></html>')
    results3 <- querySelectorAll(doc3, "a + b[id]")
    expect_equal(length(results3), 1)
    expect_equal(xmlGetAttr(results3[[1]], "id"), "b1")

    # Test with classes on both sides
    doc4 <- htmlParse('<html><body>
        <a class="link">Link</a><b class="text">B1</b>
        <a>Link2</a><b class="text">B2</b>
    </body></html>')
    results4 <- querySelectorAll(doc4, "a.link + b.text")
    expect_equal(length(results4), 1)

    # Test with multiple adjacent pairs
    doc5 <- htmlParse('<html><body>
        <a>A1</a><b id="b1">B1</b>
        <a>A2</a><b id="b2">B2</b>
    </body></html>')
    results5 <- querySelectorAll(doc5, "a + b")
    expect_equal(length(results5), 2)
    expect_equal(xmlGetAttr(results5[[1]], "id"), "b1")
    expect_equal(xmlGetAttr(results5[[2]], "id"), "b2")
})

test_that("adjacent sibling maintains correct semantics", {
    skip_if_not_installed("XML")
    library(XML)

    # Verify it only matches IMMEDIATE adjacent siblings
    doc <- htmlParse('<html><body>
        <section>
            <h1>Title</h1>
            <p id="p1">Immediate</p>
            <div>Intervening</div>
            <p id="p2">Not immediate</p>
        </section>
        <article>
            <h2>Subtitle</h2>
            <p id="p3">Immediate</p>
        </article>
    </body></html>')

    results <- querySelectorAll(doc, "h1 + p, h2 + p")
    expect_equal(length(results), 2)
    ids <- sapply(results, xmlGetAttr, "id")
    expect_true("p1" %in% ids)
    expect_true("p3" %in% ids)
    expect_false("p2" %in% ids)

    # Test that it respects element type
    doc2 <- htmlParse('<html><body>
        <a>Link</a><b>B</b><c>C</c>
    </body></html>')

    results_b <- querySelectorAll(doc2, "a + b")
    expect_equal(length(results_b), 1)
    expect_equal(xmlName(results_b[[1]]), "b")

    results_c <- querySelectorAll(doc2, "a + c")
    expect_equal(length(results_c), 0) # c is not immediately after a

    results_star <- querySelectorAll(doc2, "a + *")
    expect_equal(length(results_star), 1)
    expect_equal(xmlName(results_star[[1]]), "b")
})

test_that("adjacent sibling with pseudo-classes", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # Adjacent sibling with pseudo-class on right
    expect_that(xpath('h1 + p:first-child'),
                equals("h1/following-sibling::*[1][self::p][(count(preceding-sibling::*) = 0)]"))

    # Adjacent sibling with nth-child
    expect_that(xpath('h1 + p:nth-child(2)'),
                equals("h1/following-sibling::*[1][self::p][(count(preceding-sibling::*) = 1)]"))
})
