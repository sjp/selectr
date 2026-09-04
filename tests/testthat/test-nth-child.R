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
    expect_equal(result, "li")

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
    skip_if_not_installed("XML")

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

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # First child
    expect_equal(get_ids("li:nth-child(1)"),
                 c("li1", "li6"))

    # Second child
    expect_equal(get_ids("li:nth-child(2)"),
                 c("li2", "li7"))

    # Third child
    expect_equal(get_ids("li:nth-child(3)"),
                 c("li3", "li8"))

    # Odd children (1, 3, 5)
    expect_equal(get_ids("li:nth-child(odd)"),
                 c("li1", "li3", "li5", "li6", "li8"))

    # Even children (2, 4)
    expect_equal(get_ids("li:nth-child(even)"),
                 c("li2", "li4", "li7"))

    # Every 2nd child starting from 2 (same as even)
    expect_equal(get_ids("li:nth-child(2n)"),
                 c("li2", "li4", "li7"))

    # Every 2nd child starting from 1 (same as odd)
    expect_equal(get_ids("li:nth-child(2n+1)"),
                 c("li1", "li3", "li5", "li6", "li8"))

    # Every 3rd child starting from 1 (1, 4)
    expect_equal(get_ids("li:nth-child(3n+1)"),
                 c("li1", "li4", "li6"))

    # Every 3rd child starting from 2 (2, 5)
    expect_equal(get_ids("li:nth-child(3n+2)"),
                 c("li2", "li5", "li7"))

    # First 3 children
    expect_equal(get_ids("li:nth-child(-n+3)"),
                 c("li1", "li2", "li3", "li6", "li7", "li8"))

    # All children (n matches all positive integers)
    all_ids <- get_ids("li:nth-child(n)")
    expect_equal(length(all_ids), 8)
})

test_that(":nth-last-child() works correctly with XML documents", {
    skip_if_not_installed("XML")

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

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # Last child
    expect_equal(get_ids("li:nth-last-child(1)"),
                 c("li5", "li8"))

    # Second from last
    expect_equal(get_ids("li:nth-last-child(2)"),
                 c("li4", "li7"))

    # Third from last
    expect_equal(get_ids("li:nth-last-child(3)"),
                 c("li3", "li6"))

    # Odd from end (last=1, 3rd-last=3, 5th-last=5)
    expect_equal(get_ids("li:nth-last-child(odd)"),
                 c("li1", "li3", "li5", "li6", "li8"))

    # Even from end (2nd-last=2, 4th-last=4)
    expect_equal(get_ids("li:nth-last-child(even)"),
                 c("li2", "li4", "li7"))

    # Last 2 children
    expect_equal(get_ids("li:nth-last-child(-n+2)"),
                 c("li4", "li5", "li7", "li8"))

    # Last 3 children
    expect_equal(get_ids("li:nth-last-child(-n+3)"),
                 c("li3", "li4", "li5", "li6", "li7", "li8"))
})

test_that(":nth-child() works correctly with xml2 documents", {
    skip_if_not_installed("xml2")

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

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # First child
    expect_equal(get_ids("li:nth-child(1)"),
                 c("li1", "li6"))

    # Second child
    expect_equal(get_ids("li:nth-child(2)"),
                 c("li2", "li7"))

    # Odd children
    expect_equal(get_ids("li:nth-child(odd)"),
                 c("li1", "li3", "li5", "li6", "li8"))

    # Even children
    expect_equal(get_ids("li:nth-child(even)"),
                 c("li2", "li4", "li7"))

    # First 3 children
    expect_equal(get_ids("li:nth-child(-n+3)"),
                 c("li1", "li2", "li3", "li6", "li7", "li8"))
})

test_that(":nth-last-child() works correctly with xml2 documents", {
    skip_if_not_installed("xml2")

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

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # Last child
    expect_equal(get_ids("li:nth-last-child(1)"),
                 "li5")

    # Second from last
    expect_equal(get_ids("li:nth-last-child(2)"),
                 "li4")

    # Last 2 children
    expect_equal(get_ids("li:nth-last-child(-n+2)"),
                 c("li4", "li5"))
})

test_that(":nth-child() and :nth-last-child() can be combined", {
    skip_if_not_installed("XML")

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

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # Second child AND second from last (middle element in list of 5)
    expect_equal(get_ids("li:nth-child(2):nth-last-child(4)"),
                 "li2")

    # Middle element (3rd child AND 3rd from last)
    expect_equal(get_ids("li:nth-child(3):nth-last-child(3)"),
                 "li3")

    # First child that's also last child (only child)
    # This won't match in our test case since we have 5 items
    expect_equal(length(querySelectorAll(doc, "li:nth-child(1):nth-last-child(1)")),
                 0)
})

test_that(":nth-child() edge cases", {
    skip_if_not_installed("XML")

    # Empty list
    html1 <- '<root><ul id="empty"></ul></root>'
    doc1 <- XML::xmlRoot(XML::xmlParse(html1))
    expect_equal(length(querySelectorAll(doc1, "li:nth-child(1)")),
                 0)

    # Single child
    html2 <- '<root><ul><li id="only">Only</li></ul></root>'
    doc2 <- XML::xmlRoot(XML::xmlParse(html2))

    # Should match as first child
    result <- querySelectorAll(doc2, "li:nth-child(1)")
    expect_equal(length(result), 1)
    expect_equal(XML::xmlGetAttr(result[[1]], "id"), "only")

    # Should also match as last child
    result2 <- querySelectorAll(doc2, "li:nth-last-child(1)")
    expect_equal(length(result2), 1)
    expect_equal(XML::xmlGetAttr(result2[[1]], "id"), "only")

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
    doc3 <- XML::xmlRoot(XML::xmlParse(html3))

    get_ids <- function(css) {
        results <- querySelectorAll(doc3, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # First child (p element that is first child)
    expect_equal(get_ids("p:nth-child(1)"),
                 "p1")

    # Second child (div element that is second child)
    expect_equal(get_ids("div:nth-child(2)"),
                 "d1")

    # All p elements that are odd children
    expect_equal(get_ids("p:nth-child(odd)"),
                 c("p1", "p2"))
})

test_that(":nth-child() with querySelector returns first match", {
    skip_if_not_installed("xml2")

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

    doc <- xml2::read_xml(html)

    # Should return first element that's a first child (li1)
    result <- querySelector(doc, "li:nth-child(1)")
    expect_equal(xml2::xml_attr(result, "id"), "li1")

    # Should return first element that's a second child (li2)
    result2 <- querySelector(doc, "li:nth-child(2)")
    expect_equal(xml2::xml_attr(result2, "id"), "li2")
})

test_that(":nth-child() with different element types", {
    skip_if_not_installed("XML")

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

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # p:nth-child(2) - p that is 2nd child (p1)
    expect_equal(get_ids("p:nth-child(2)"),
                 "p1")

    # p:nth-child(3) - p that is 3rd child (p2)
    expect_equal(get_ids("p:nth-child(3)"),
                 "p2")

    # p:nth-child(1) - p that is first child (none)
    expect_equal(length(querySelectorAll(doc, "p:nth-child(1)")),
                 0)

    # span:nth-child(4) - span that is 4th child (s1)
    expect_equal(get_ids("span:nth-child(4)"),
                 "s1")
})

test_that(":nth-child() with complex selectors", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <ul class="menu">',
        '    <li id="li1" class="item">First</li>',
        '    <li id="li2" class="item active">Second</li>',
        '    <li id="li3" class="item">Third</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # Class selector with :nth-child
    expect_equal(get_ids(".item:nth-child(1)"),
                 "li1")

    # Multiple classes with :nth-child
    expect_equal(get_ids(".item.active:nth-child(2)"),
                 "li2")

    # Descendant combinator with :nth-child
    expect_equal(get_ids(".menu li:nth-child(2)"),
                 "li2")

    # Child combinator with :nth-child
    expect_equal(get_ids(".menu > li:nth-child(3)"),
                 "li3")
})

test_that(":nth-child() early-exit condition 1: a=1, b-1<=0 (matches all)", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1" class="item">1</li>',
        '    <li id="li2" class="item">2</li>',
        '    <li id="li3" class="item">3</li>',
        '    <li id="li4">4</li>',
        '  </ul>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # :nth-child(n) -> a=1, b=0, b-1=-1<=0 -> matches all
    expect_equal(get_ids("li:nth-child(n)"),
                 c("li1", "li2", "li3", "li4"))

    # :nth-child(1n+0) -> a=1, b=0, b-1=-1<=0 -> matches all
    expect_equal(get_ids("li:nth-child(1n+0)"),
                 c("li1", "li2", "li3", "li4"))

    # :nth-child(n+1) -> a=1, b=1, b-1=0<=0 -> matches all
    expect_equal(get_ids("li:nth-child(n+1)"),
                 c("li1", "li2", "li3", "li4"))

    # :nth-child(1n+1) -> a=1, b=1, b-1=0<=0 -> matches all
    expect_equal(get_ids("li:nth-child(1n+1)"),
                 c("li1", "li2", "li3", "li4"))

    # :nth-child(n-1) -> a=1, b=-1, b-1=-2<=0 -> matches all
    expect_equal(get_ids("li:nth-child(n-1)"),
                 c("li1", "li2", "li3", "li4"))

    # :nth-child(n-5) -> a=1, b=-5, b-1=-6<=0 -> matches all
    expect_equal(get_ids("li:nth-child(n-5)"),
                 c("li1", "li2", "li3", "li4"))
})

test_that(":nth-child() early-exit condition 1 with selector list", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1" class="item">1</li>',
        '    <li id="li2" class="item">2</li>',
        '    <li id="li3">3</li>',
        '    <li id="li4" class="item">4</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # :nth-child(n of .item) -> a=1, b=0 -> but filtered by .item
    expect_equal(get_ids("li:nth-child(n of .item)"),
                 c("li1", "li2", "li4"))

    # :nth-child(1n+0 of .item) -> same as above
    expect_equal(get_ids("li:nth-child(1n+0 of .item)"),
                 c("li1", "li2", "li4"))

    # :nth-child(n+1 of .item) -> a=1, b=1 -> filtered by .item
    expect_equal(get_ids("li:nth-child(n+1 of .item)"),
                 c("li1", "li2", "li4"))
})

test_that(":nth-last-child() early-exit condition 1: a=1, b-1<=0", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1">1</li>',
        '    <li id="li2" class="special">2</li>',
        '    <li id="li3">3</li>',
        '  </ul>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        sapply(results, function(x) XML::xmlGetAttr(x, "id"))
    }

    # :nth-last-child(n) -> a=1, b=0, b-1=-1<=0 -> matches all
    expect_equal(get_ids("li:nth-last-child(n)"),
                 c("li1", "li2", "li3"))

    # :nth-last-child(n+1) -> a=1, b=1, b-1=0<=0 -> matches all
    expect_equal(get_ids("li:nth-last-child(n+1)"),
                 c("li1", "li2", "li3"))

    # :nth-last-child(n of .special) -> filtered by .special
    expect_equal(get_ids("li:nth-last-child(n of .special)"),
                 "li2")
})

test_that(":nth-child() early-exit condition 2: a<0, b-1<0 (matches none)", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1" class="item">1</li>',
        '    <li id="li2" class="item">2</li>',
        '    <li id="li3">3</li>',
        '  </ul>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    # :nth-child(-n) -> a=-1, b=0, b-1=-1<0 -> impossible, matches none
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-n)")),
                 0)

    # :nth-child(-n-1) -> a=-1, b=-1, b-1=-2<0 -> impossible, matches none
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-n-1)")),
                 0)

    # :nth-child(-2n-1) -> a=-2, b=-1, b-1=-2<0 -> impossible, matches none
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-2n-1)")),
                 0)

    # :nth-child(-3n-5) -> a=-3, b=-5, b-1=-6<0 -> impossible, matches none
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-3n-5)")),
                 0)

    # Verify XPath contains "0" condition
    xpath <- css_to_xpath("li:nth-child(-n)")
    expect_true(grepl("\\[.*0.*\\]", xpath))
})

test_that(":nth-child(0) uses the same early-exit form as :nth-child(-n)", {
    skip_if_not_installed("xml2")
    # a=0, b=0, b-1=-1<0: an impossible count, just like a<0/b-1<0. Both
    # should produce the tidy "[0]" early exit rather than an always-false
    # "count(...) = -1" comparison built from the literal b-1
    expect_equal(css_to_xpath("li:nth-child(0)"), css_to_xpath("li:nth-child(-n)"))
    expect_equal(length(querySelectorAll(xml2::read_xml(
        "<root><li/><li/></root>"), "li:nth-child(0)")), 0)
})

test_that(":nth-child() early-exit condition 2 with selector list", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1" class="item">1</li>',
        '    <li id="li2" class="item">2</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    # :nth-child(-n of .item) -> a=-1, b=0 -> impossible even with selector
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-n of .item)")),
                 0)

    # :nth-child(-2n-1 of .item) -> a=-2, b=-1 -> impossible
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-2n-1 of .item)")),
                 0)

    # The count is impossible whether or not the element matches S, so
    # the always-false "0" absorbs the 'of S' test rather than being
    # AND-ed with it
    expect_equal(css_to_xpath("li:nth-child(-n of .item)"),
                 css_to_xpath("li:nth-child(-n)"))
    xpath <- css_to_xpath("li:nth-child(-n of .item)")
    expect_true(grepl("[0]", xpath, fixed = TRUE))
    expect_false(grepl("item", xpath))
})

test_that(":nth-last-child() early-exit condition 2: a<0, b-1<0", {
    skip_if_not_installed("XML")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1">1</li>',
        '    <li id="li2" class="special">2</li>',
        '    <li id="li3">3</li>',
        '  </ul>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(html))

    # :nth-last-child(-n) -> a=-1, b=0, b-1=-1<0 -> impossible
    expect_equal(length(querySelectorAll(doc, "li:nth-last-child(-n)")),
                 0)

    # :nth-last-child(-n-1) -> a=-1, b=-1, b-1=-2<0 -> impossible
    expect_equal(length(querySelectorAll(doc, "li:nth-last-child(-n-1)")),
                 0)

    # :nth-last-child(-n of .special) -> impossible even with selector
    expect_equal(length(querySelectorAll(doc, "li:nth-last-child(-n of .special)")),
                 0)
})

test_that(":nth-child() boundary between early-exit conditions", {
    skip_if_not_installed("xml2")

    html <- paste0(
        '<root>',
        '  <ul>',
        '    <li id="li1">1</li>',
        '    <li id="li2">2</li>',
        '    <li id="li3">3</li>',
        '  </ul>',
        '</root>'
    )

    doc <- xml2::read_xml(html)

    get_ids <- function(css) {
        results <- querySelectorAll(doc, css)
        xml2::xml_attr(results, "id")
    }

    # :nth-child(-n+0) -> a=-1, b=0, b-1=-1 -> NOT early-exit (b-1<0 but a<0 not b-1<0)
    # This should match nothing (0 or fewer siblings)
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-n+0)")),
                 0)

    # :nth-child(-n+1) -> a=-1, b=1, b-1=0 -> NOT early-exit (b-1 not <0)
    # This should match first child only
    expect_equal(get_ids("li:nth-child(-n+1)"),
                 "li1")

    # :nth-child(-n+2) -> a=-1, b=2, b-1=1 -> matches first 2
    expect_equal(get_ids("li:nth-child(-n+2)"),
                 c("li1", "li2"))

    # :nth-child(-2n+2) -> a=-2, b=2, b-1=1 -> matches 2nd child
    expect_equal(get_ids("li:nth-child(-2n+2)"),
                 "li2")

    # :nth-child(-2n+0) -> a=-2, b=0, b-1=-1<0 -> early-exit condition 2
    expect_equal(length(querySelectorAll(doc, "li:nth-child(-2n+0)")),
                 0)
})

test_that("large An+B values are written out in full", {
    xpath <- function(css) {
        css_to_xpath(css, prefix = "")
    }

    # XPath 1.0 numbers have no exponent form, so neither a nor b may
    # reach the expression as "1e+05"
    expect_equal(xpath("li:nth-child(100001)"),
                 "li[count(preceding-sibling::*) = 100000]")
    expect_equal(xpath("li:nth-child(n+100001)"),
                 "li[count(preceding-sibling::*) >= 100000]")
    expect_equal(xpath("li:nth-child(200000n)"),
                 "li[(count(preceding-sibling::*) + 1) mod 200000 = 0]")
    expect_equal(xpath("li:nth-child(1000000n+1000001)"),
                 paste0("li[count(preceding-sibling::*) >= 1000000 and ",
                        "count(preceding-sibling::*) mod 1000000 = 0]"))
    expect_equal(xpath("li:nth-last-child(-100000n+100000)"),
                 paste0("li[count(following-sibling::*) <= 99999 and ",
                        "(count(following-sibling::*) + 1) mod -100000 = 0]"))
    expect_equal(xpath("li:nth-of-type(100000n+3)"),
                 paste0("li[count(preceding-sibling::li) >= 2 and ",
                        "(count(preceding-sibling::li) + 99998) mod 100000 = 0]"))

    # and the formatting does not follow the session's number-printing
    # options
    op <- options(scipen = -9)
    on.exit(options(op))
    expect_equal(xpath("li:nth-child(100001)"),
                 "li[count(preceding-sibling::*) = 100000]")
    expect_equal(xpath("li:nth-child(200000n)"),
                 "li[(count(preceding-sibling::*) + 1) mod 200000 = 0]")
})

test_that("large An+B values select the right nodes", {
    skip_if_not_installed("XML")
    skip_if_not_installed("xml2")

    html <- paste0(
        "<root>",
        "  <ul>",
        "    <li id=\"li1\">1</li>",
        "    <li id=\"li2\">2</li>",
        "    <li id=\"li3\">3</li>",
        "  </ul>",
        "</root>"
    )
    docs <- list(XML = XML::xmlParse(html), xml2 = xml2::read_xml(html))

    ids <- function(doc, css) {
        results <- querySelectorAll(doc, css)
        if (inherits(doc, "XMLInternalDocument"))
            as.character(sapply(results, XML::xmlGetAttr, "id"))
        else
            xml2::xml_attr(results, "id")
    }

    for (doc in docs) {
        # b beyond the number of siblings matches nothing
        expect_equal(length(querySelectorAll(doc, "li:nth-child(100001)")), 0)
        # only n = 0 lands inside the list
        expect_equal(ids(doc, "li:nth-child(200000n+2)"), "li2")
        expect_equal(ids(doc, "li:nth-last-child(100000n+1)"), "li3")
        expect_equal(ids(doc, "li:nth-of-type(1000000n+1)"), "li1")
    }
})
