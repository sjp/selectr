context(":nth-child() and :nth-last-child() pseudo-classes")

test_that(":nth-child() generates correct XPath", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # :nth-child(1) - first child
    result <- xpath("li:nth-child(1)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\) = 0", result))

    # :nth-child(2) - second child
    result <- xpath("li:nth-child(2)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\) = 1", result))

    # :nth-child(odd) - odd children
    result <- xpath("li:nth-child(odd)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", result))
    expect_true(grepl("mod 2", result))

    # :nth-child(even) - even children
    result <- xpath("li:nth-child(even)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", result))
    expect_true(grepl("mod 2", result))

    # :nth-child(2n) - every 2nd child (even)
    result <- xpath("li:nth-child(2n)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", result))

    # :nth-child(3n+1) - every 3rd starting from 1st
    result <- xpath("li:nth-child(3n+1)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", result))

    # :nth-child(n) - all children (simplifies to just the element)
    result <- xpath("li:nth-child(n)")
    expect_that(result, equals("li"))

    # :nth-child(-n+3) - first 3 children
    result <- xpath("li:nth-child(-n+3)")
    expect_true(grepl("count\\(preceding-sibling::\\*\\)", result))
})

test_that(":nth-last-child() generates correct XPath", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # :nth-last-child(1) - last child
    result <- xpath("li:nth-last-child(1)")
    expect_true(grepl("count\\(following-sibling::\\*\\) = 0", result))

    # :nth-last-child(2) - second from last
    result <- xpath("li:nth-last-child(2)")
    expect_true(grepl("count\\(following-sibling::\\*\\) = 1", result))

    # :nth-last-child(odd) - odd from end
    result <- xpath("li:nth-last-child(odd)")
    expect_true(grepl("count\\(following-sibling::\\*\\)", result))
    expect_true(grepl("mod 2", result))

    # :nth-last-child(even) - even from end
    result <- xpath("li:nth-last-child(even)")
    expect_true(grepl("count\\(following-sibling::\\*\\)", result))
    expect_true(grepl("mod 2", result))

    # :nth-last-child(-n+2) - last 2 children
    result <- xpath("li:nth-last-child(-n+2)")
    expect_true(grepl("count\\(following-sibling::\\*\\)", result))
})

test_that(":nth-child() works correctly with XML documents", {
    library(XML)

    html <- paste0(
        '<root>',
        '  <ul id="list1">',
        '    <li id="li1" class="odd">Item 1</li>',
        '    <li id="li2" class="even">Item 2</li>',
        '    <li id="li3" class="odd">Item 3</li>',
        '    <li id="li4" class="even">Item 4</li>',
        '    <li id="li5" class="odd">Item 5</li>',
        '  </ul>',
        '  <ul id="list2">',
        '    <li id="li6">A</li>',
        '    <li id="li7">B</li>',
        '    <li id="li8">C</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xmlRoot(xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # First child
    expect_that(get_ids("li:nth-child(1)"),
                equals(c("li1", "li6")))

    # Second child
    expect_that(get_ids("li:nth-child(2)"),
                equals(c("li2", "li7")))

    # Third child
    expect_that(get_ids("li:nth-child(3)"),
                equals(c("li3", "li8")))

    # Odd children (1, 3, 5)
    expect_that(get_ids("li:nth-child(odd)"),
                equals(c("li1", "li3", "li5", "li6", "li8")))

    # Even children (2, 4)
    expect_that(get_ids("li:nth-child(even)"),
                equals(c("li2", "li4", "li7")))

    # Every 2nd child starting from 2 (same as even)
    expect_that(get_ids("li:nth-child(2n)"),
                equals(c("li2", "li4", "li7")))

    # Every 2nd child starting from 1 (same as odd)
    expect_that(get_ids("li:nth-child(2n+1)"),
                equals(c("li1", "li3", "li5", "li6", "li8")))

    # Every 3rd child starting from 1 (1, 4)
    expect_that(get_ids("li:nth-child(3n+1)"),
                equals(c("li1", "li4", "li6")))

    # Every 3rd child starting from 2 (2, 5)
    expect_that(get_ids("li:nth-child(3n+2)"),
                equals(c("li2", "li5", "li7")))

    # First 3 children
    expect_that(get_ids("li:nth-child(-n+3)"),
                equals(c("li1", "li2", "li3", "li6", "li7", "li8")))

    # All children (n matches all positive integers)
    all_ids <- get_ids("li:nth-child(n)")
    expect_that(length(all_ids), equals(8))
})

test_that(":nth-last-child() works correctly with XML documents", {
    library(XML)

    html <- paste0(
        '<root>',
        '  <ul id="list1">',
        '    <li id="li1">Item 1</li>',
        '    <li id="li2">Item 2</li>',
        '    <li id="li3">Item 3</li>',
        '    <li id="li4">Item 4</li>',
        '    <li id="li5">Item 5</li>',
        '  </ul>',
        '  <ul id="list2">',
        '    <li id="li6">A</li>',
        '    <li id="li7">B</li>',
        '    <li id="li8">C</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xmlRoot(xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # Last child
    expect_that(get_ids("li:nth-last-child(1)"),
                equals(c("li5", "li8")))

    # Second from last
    expect_that(get_ids("li:nth-last-child(2)"),
                equals(c("li4", "li7")))

    # Third from last
    expect_that(get_ids("li:nth-last-child(3)"),
                equals(c("li3", "li6")))

    # Odd from end (last=1, 3rd-last=3, 5th-last=5)
    expect_that(get_ids("li:nth-last-child(odd)"),
                equals(c("li1", "li3", "li5", "li6", "li8")))

    # Even from end (2nd-last=2, 4th-last=4)
    expect_that(get_ids("li:nth-last-child(even)"),
                equals(c("li2", "li4", "li7")))

    # Last 2 children
    expect_that(get_ids("li:nth-last-child(-n+2)"),
                equals(c("li4", "li5", "li7", "li8")))

    # Last 3 children
    expect_that(get_ids("li:nth-last-child(-n+3)"),
                equals(c("li3", "li4", "li5", "li6", "li7", "li8")))
})

test_that(":nth-child() works correctly with xml2 documents", {
    library(xml2)

    html <- paste0(
        '<root>',
        '  <ul id="list1">',
        '    <li id="li1">Item 1</li>',
        '    <li id="li2">Item 2</li>',
        '    <li id="li3">Item 3</li>',
        '    <li id="li4">Item 4</li>',
        '    <li id="li5">Item 5</li>',
        '  </ul>',
        '  <ul id="list2">',
        '    <li id="li6">A</li>',
        '    <li id="li7">B</li>',
        '    <li id="li8">C</li>',
        '  </ul>',
        '</root>'
    )

    doc <- read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml_attr(results, "id")
    }

    # First child
    expect_that(get_ids("li:nth-child(1)"),
                equals(c("li1", "li6")))

    # Second child
    expect_that(get_ids("li:nth-child(2)"),
                equals(c("li2", "li7")))

    # Odd children
    expect_that(get_ids("li:nth-child(odd)"),
                equals(c("li1", "li3", "li5", "li6", "li8")))

    # Even children
    expect_that(get_ids("li:nth-child(even)"),
                equals(c("li2", "li4", "li7")))

    # First 3 children
    expect_that(get_ids("li:nth-child(-n+3)"),
                equals(c("li1", "li2", "li3", "li6", "li7", "li8")))
})

test_that(":nth-last-child() works correctly with xml2 documents", {
    library(xml2)

    html <- paste0(
        '<root>',
        '  <ul id="list1">',
        '    <li id="li1">Item 1</li>',
        '    <li id="li2">Item 2</li>',
        '    <li id="li3">Item 3</li>',
        '    <li id="li4">Item 4</li>',
        '    <li id="li5">Item 5</li>',
        '  </ul>',
        '</root>'
    )

    doc <- read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml_attr(results, "id")
    }

    # Last child
    expect_that(get_ids("li:nth-last-child(1)"),
                equals("li5"))

    # Second from last
    expect_that(get_ids("li:nth-last-child(2)"),
                equals("li4"))

    # Last 2 children
    expect_that(get_ids("li:nth-last-child(-n+2)"),
                equals(c("li4", "li5")))
})

test_that(":nth-child() and :nth-last-child() can be combined", {
    library(XML)

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1">1</li>',
        '    <li id="li2">2</li>',
        '    <li id="li3">3</li>',
        '    <li id="li4">4</li>',
        '    <li id="li5">5</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xmlRoot(xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # Second child AND second from last (middle element in list of 5)
    expect_that(get_ids("li:nth-child(2):nth-last-child(4)"),
                equals("li2"))

    # Middle element (3rd child AND 3rd from last)
    expect_that(get_ids("li:nth-child(3):nth-last-child(3)"),
                equals("li3"))

    # First child that's also last child (only child)
    # This won't match in our test case since we have 5 items
    expect_that(length(querySelectorAll(doc, "li:nth-child(1):nth-last-child(1)")),
                equals(0))
})

test_that(":nth-child() edge cases", {
    library(XML)

    # Empty list
    html1 <- '<root><ul id="empty"></ul></root>'
    doc1 <- xmlRoot(xmlParse(html1))
    expect_that(length(querySelectorAll(doc1, "li:nth-child(1)")),
                equals(0))

    # Single child
    html2 <- '<root><ul><li id="only">Only</li></ul></root>'
    doc2 <- xmlRoot(xmlParse(html2))

    # Should match as first child
    result <- querySelectorAll(doc2, "li:nth-child(1)")
    expect_that(length(result), equals(1))
    expect_that(xmlGetAttr(result[[1]], "id"), equals("only"))

    # Should also match as last child
    result2 <- querySelectorAll(doc2, "li:nth-last-child(1)")
    expect_that(length(result2), equals(1))
    expect_that(xmlGetAttr(result2[[1]], "id"), equals("only"))

    # Mixed element types
    html3 <- paste0(
        '<root>',
        '  <div>',
        '    <p id="p1">Para</p>',
        '    <div id="d1">Div</div>',
        '    <p id="p2">Para</p>',
        '    <span id="s1">Span</span>',
        '  </div>',
        '</root>'
    )
    doc3 <- xmlRoot(xmlParse(html3))

    get_ids <- function(css) {
        results <- querySelectorAll(doc3, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # First child (p element that is first child)
    expect_that(get_ids("p:nth-child(1)"),
                equals("p1"))

    # Second child (div element that is second child)
    expect_that(get_ids("div:nth-child(2)"),
                equals("d1"))

    # All p elements that are odd children
    expect_that(get_ids("p:nth-child(odd)"),
                equals(c("p1", "p2")))
})

test_that(":nth-child() with querySelector returns first match", {
    library(xml2)

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1">1</li>',
        '    <li id="li2">2</li>',
        '  </ul>',
        '  <ul>',
        '    <li id="li3">3</li>',
        '    <li id="li4">4</li>',
        '  </ul>',
        '</root>'
    )

    doc <- read_xml(html)

    # Should return first element that's a first child (li1)
    result <- querySelector(doc, "li:nth-child(1)")
    expect_that(xml_attr(result, "id"), equals("li1"))

    # Should return first element that's a second child (li2)
    result2 <- querySelector(doc, "li:nth-child(2)")
    expect_that(xml_attr(result2, "id"), equals("li2"))
})

test_that(":nth-child() with different element types", {
    library(XML)

    # Test that :nth-child counts all siblings, not just same type
    html <- paste0(
        '<root>',
        '  <div>',
        '    <h1 id="h1">Heading</h1>',
        '    <p id="p1">Para 1</p>',
        '    <p id="p2">Para 2</p>',
        '    <span id="s1">Span</span>',
        '  </div>',
        '</root>'
    )

    doc <- xmlRoot(xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) xmlGetAttr(x, "id"))
    }

    # p:nth-child(2) - p that is 2nd child (p1)
    expect_that(get_ids("p:nth-child(2)"),
                equals("p1"))

    # p:nth-child(3) - p that is 3rd child (p2)
    expect_that(get_ids("p:nth-child(3)"),
                equals("p2"))

    # p:nth-child(1) - p that is first child (none)
    expect_that(length(querySelectorAll(doc, "p:nth-child(1)")),
                equals(0))

    # span:nth-child(4) - span that is 4th child (s1)
    expect_that(get_ids("span:nth-child(4)"),
                equals("s1"))
})

test_that(":nth-child() with complex selectors", {
    library(xml2)

    html <- paste0(
        '<root>',
        '  <ul class="menu">',
        '    <li id="li1" class="item">First</li>',
        '    <li id="li2" class="item active">Second</li>',
        '    <li id="li3" class="item">Third</li>',
        '  </ul>',
        '</root>'
    )

    doc <- read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml_attr(results, "id")
    }

    # Class selector with :nth-child
    expect_that(get_ids(".item:nth-child(1)"),
                equals("li1"))

    # Multiple classes with :nth-child
    expect_that(get_ids(".item.active:nth-child(2)"),
                equals("li2"))

    # Descendant combinator with :nth-child
    expect_that(get_ids(".menu li:nth-child(2)"),
                equals("li2"))

    # Child combinator with :nth-child
    expect_that(get_ids(".menu > li:nth-child(3)"),
                equals("li3"))
})
