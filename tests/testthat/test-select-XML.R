test_that("selection works correctly on a large barrage of tests", {
    HTML_IDS <- fixture_html_ids()

    skip_if_not_installed("XML")
    library(XML)
    document <- xmlRoot(xmlParse(HTML_IDS))
    gt <- GenericTranslator$new()
    ht <- HTMLTranslator$new()

    select_ids <- function(selector, html_only) {
        if (html_only) {
            xpath <- ht$css_to_xpath(selector)
            items <- getNodeSet(document, xpath)
        } else {
            xpath <- gt$css_to_xpath(selector)
            items <- getNodeSet(document, xpath)
        }
        n <- length(items)
        if (!n)
            return(NULL)
        result <- character(n)
        for (i in seq_len(n)) {
            element <- items[[i]]
            tmp <- xmlAttrs(element)["id"]
            if (is.null(tmp))
                tmp <- "nil"
            result[i] <- tmp
        }
        result
    }

    pcss <- function(main, selectors = NULL, html_only = FALSE) {
        result <- select_ids(main, html_only)
        if (!is.null(selectors) && length(selectors)) {
            n <- length(selectors)
            for (i in seq_len(n)) {
                tmp_res <- select_ids(selectors[i], html_only = html_only)
                if (!is.null(result) && !is.null(tmp_res) &&
                    !identical(tmp_res, result))
                    stop("Difference between results of selectors")
            }
        }
        result
    }

    all_ids <- pcss('*')
    expect_equal(all_ids[1:6], c('html', 'nil', 'link-href', 'link-nohref', 'nil', 'outer-div'))
    expect_equal(tail(all_ids, 1), 'foobar-span')
    expect_equal(pcss('div'), c('outer-div', 'li-div', 'foobar-div'))
    expect_equal(pcss('DIV', html_only = TRUE), c('outer-div', 'li-div', 'foobar-div'))  # case-insensitive in HTML
    expect_equal(pcss('div div'), 'li-div')
    expect_equal(pcss('div, div div'), c('outer-div', 'li-div', 'foobar-div'))
    expect_equal(pcss('a[name]'), 'name-anchor')
    expect_equal(pcss('a[NAme]', html_only = TRUE), 'name-anchor') # case-insensitive in HTML:
    expect_equal(pcss('a[rel]'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss('a[rel="tag"]'), 'tag-anchor')
    expect_equal(pcss('a[href*="localhost"]'), 'tag-anchor')
    expect_equal(pcss('a[href*=""]'), NULL)
    expect_equal(pcss('a[href^="http"]'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss('a[href^="http:"]'), 'tag-anchor')
    expect_equal(pcss('a[href^=""]'), NULL)
    expect_equal(pcss('a[href$="org"]'), 'nofollow-anchor')
    expect_equal(pcss('a[href$=""]'), NULL)
    expect_equal(pcss('div[foobar~="bc"]', 'div[foobar~="cde"]'), 'foobar-div')
    expect_equal(pcss('[foobar~="ab bc"]', c('[foobar~=""]', '[foobar~=" \t"]')), NULL)
    expect_equal(pcss('div[foobar~="cd"]'), NULL)
    expect_equal(pcss('*[lang|="En"]', '[lang|="En-us"]'), 'second-li')
    # Attribute values are case sensitive
    expect_equal(pcss('*[lang|="en"]', '[lang|="en-US"]'), NULL)
    expect_equal(pcss('*[lang|="e"]'), NULL)
    # ... :lang() is not.
    expect_equal(pcss(':lang("EN")', '*:lang(en-US)', html_only = TRUE), c('second-li', 'li-div'))
    expect_equal(pcss(':lang("e")', html_only = TRUE), NULL)
    expect_equal(pcss('li:nth-child(-n)'), NULL)
    expect_equal(pcss('li:nth-child(n)'), c('first-li', 'second-li', 'third-li', 'fourth-li', 'fifth-li', 'sixth-li', 'seventh-li'))
    expect_equal(pcss('li:nth-child(3)'), 'third-li')
    expect_equal(pcss('li:nth-child(10)'), NULL)
    expect_equal(pcss('li:nth-child(2n)', c('li:nth-child(even)', 'li:nth-child(2n+0)')), c('second-li', 'fourth-li', 'sixth-li'))
    expect_equal(pcss('li:nth-child(+2n+1)', 'li:nth-child(odd)'), c('first-li', 'third-li', 'fifth-li', 'seventh-li'))
    expect_equal(pcss('li:nth-child(2n+4)'), c('fourth-li', 'sixth-li'))
    expect_equal(pcss('li:nth-child(3n+1)'), c('first-li', 'fourth-li', 'seventh-li'))
    expect_equal(pcss('li:nth-child(-n+3)'), c('first-li', 'second-li', 'third-li'))
    expect_equal(pcss('li:nth-child(-2n+4)'), c('second-li', 'fourth-li'))
    expect_equal(pcss('li:nth-last-child(0)'), NULL)
    expect_equal(pcss('li:nth-last-child(1)'), 'seventh-li')
    expect_equal(pcss('li:nth-last-child(2n)', 'li:nth-last-child(even)'), c('second-li', 'fourth-li', 'sixth-li'))
    expect_equal(pcss('li:nth-last-child(2n+2)'), c('second-li', 'fourth-li', 'sixth-li'))
    expect_equal(pcss('ol:first-of-type'), 'first-ol')
    expect_equal(pcss('ol:nth-child(1)'), NULL)
    expect_equal(pcss('ol:nth-of-type(2)'), 'second-ol')
    expect_equal(pcss('ol:nth-last-of-type(1)'), 'second-ol')
    expect_equal(pcss('span:only-child'), 'foobar-span')
    expect_equal(pcss('li div:only-child'), 'li-div')
    expect_equal(pcss('div *:only-child'), c('li-div', 'foobar-span'))
    #self.assertRaises(ExpressionError, pcss, 'p *:only-of-type')
    expect_equal(pcss('p:only-of-type'), 'paragraph')
    expect_equal(pcss('a:empty', 'a:EMpty'), 'name-anchor')
    expect_equal(pcss('li:empty'), c('third-li', 'fourth-li', 'fifth-li', 'sixth-li'))
    expect_equal(pcss(':root', 'html:root'), 'html')
    expect_equal(pcss('li:root', '* :root'), NULL)
    expect_equal(pcss('.a', c('.b', '*.a', 'ol.a')), 'first-ol')
    expect_equal(pcss('.c', '*.c'), c('first-ol', 'third-li', 'fourth-li'))
    expect_equal(pcss('ol *.c', c('ol li.c', 'li ~ li.c', 'ol > li.c')), c('third-li', 'fourth-li'))
    expect_equal(pcss('#first-li', c('li#first-li', '*#first-li')), 'first-li')
    expect_equal(pcss('li div', c('li > div', 'div div')), 'li-div')
    expect_equal(pcss('div > div'), NULL)
    expect_equal(pcss('div>.c', 'div > .c'), 'first-ol')
    expect_equal(pcss('div + div'), 'foobar-div')
    expect_equal(pcss('a ~ a'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss('a[rel="tag"] ~ a'), 'nofollow-anchor')
    expect_equal(pcss('ol#first-ol li:last-child'), 'seventh-li')
    expect_equal(pcss('ol#first-ol *:last-child'), c('li-div', 'seventh-li'))
    expect_equal(pcss('#outer-div:first-child'), 'outer-div')
    expect_equal(pcss('#outer-div :first-child'), c('name-anchor', 'first-li', 'li-div', 'p-b', 'checkbox-fieldset-disabled', 'area-href'))
    expect_equal(pcss('a[href]'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss(':not(*)'), NULL)
    expect_equal(pcss('a:not([href])'), 'name-anchor')
    expect_equal(pcss('ol :Not(li[class])'), c('first-li', 'second-li', 'li-div', 'fifth-li', 'sixth-li', 'seventh-li'))

    expect_equal(pcss(':is(#first-li, #second-li)'), c('first-li', 'second-li'))
    expect_equal(pcss('a:is(#name-anchor, #tag-anchor)'), c('name-anchor', 'tag-anchor'))
    expect_equal(pcss(':is(.c)'), c('first-ol', 'third-li', 'fourth-li'))
    expect_equal(pcss(':matches(#first-li, #second-li)'), c('first-li', 'second-li'))
    expect_equal(pcss('a:matches(#name-anchor, #tag-anchor)'), c('name-anchor', 'tag-anchor'))
    expect_equal(pcss(':matches(.c)'), c('first-ol', 'third-li', 'fourth-li'))
    # :is()/:where() alternatives stay grouped: they AND with conditions
    # before and after the pseudo-class instead of OR-ing across the compound
    expect_equal(pcss('li.c:is(#third-li, #fifth-li)'), 'third-li')
    expect_equal(pcss('li.c:where(#third-li, #fifth-li)'), 'third-li')
    expect_equal(pcss(':is(li, ol):first-child'), 'first-li')
    expect_equal(pcss('li:is(.c):is(#fourth-li)'), 'fourth-li')
    # An empty forgiving selector list matches nothing
    expect_equal(pcss(':is()'), NULL)
    expect_equal(pcss('li:where()'), NULL)
    # An always-true '*' argument makes the whole selector list match
    # everything; it must not be silently dropped
    expect_equal(pcss('li:is(#first-li, *)'), c('first-li', 'second-li', 'third-li', 'fourth-li', 'fifth-li', 'sixth-li', 'seventh-li'))
    expect_equal(pcss('li:not(#first-li, *)'), NULL)
    expect_equal(pcss('ol:nth-child(6 of a, *)'), 'second-ol')

    expect_equal(pcss('ol:has(li)'), 'first-ol')
    # :has(.c) matches all ancestors of elements with class 'c'
    expect_equal(pcss(':has(.c)'), c('html', 'nil', 'outer-div', 'first-ol'))

    # Invalid characters in XPath element names, should not crash
    expect_equal(pcss('di\ua0v', 'div\\['), NULL)
    expect_equal(pcss('[h\ua0ref]', '[h\\]ref]'), NULL)

    ## HTML-specific
    expect_equal(pcss(':link', html_only = TRUE), c('link-href', 'tag-anchor', 'nofollow-anchor', 'area-href'))
    expect_equal(pcss(':visited', html_only = TRUE), NULL)
    expect_equal(pcss(':enabled', html_only = TRUE), c('checkbox-unchecked', 'text-checked', 'checkbox-checked'))
    expect_equal(pcss(':disabled', html_only = TRUE), c('checkbox-disabled', 'checkbox-disabled-checked', 'fieldset', 'checkbox-fieldset-disabled'))
    expect_equal(pcss(':checked', html_only = TRUE), c('checkbox-checked', 'checkbox-disabled-checked'))
})
