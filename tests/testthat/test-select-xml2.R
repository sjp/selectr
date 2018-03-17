context("large-test")

test_that("selection works correctly on a large barrage of tests", {
    HTML_IDS <- paste0(
        c("<html id=\"html\"><head>", "  <link id=\"link-href\" href=\"foo\" />",
          "  <link id=\"link-nohref\" />", "</head><body>", "<div id=\"outer-div\">",
          " <a id=\"name-anchor\" name=\"foo\"></a>", " <a id=\"tag-anchor\" rel=\"tag\" href=\"http://localhost/foo\">link</a>",
          " <a id=\"nofollow-anchor\" rel=\"nofollow\" href=\"https://example.org\">",
          "    link</a>", " <ol id=\"first-ol\" class=\"a b c\">", "   <li id=\"first-li\">content</li>",
          "   <li id=\"second-li\" lang=\"En-us\">", "     <div id=\"li-div\">",
          "     </div>", "   </li>", "   <li id=\"third-li\" class=\"ab c\"></li>",
          "   <li id=\"fourth-li\" class=\"ab", "c\"></li>", "   <li id=\"fifth-li\"></li>",
          "   <li id=\"sixth-li\"></li>", "   <li id=\"seventh-li\">  </li>",
          " </ol>", " <p id=\"paragraph\">", "   <b id=\"p-b\">hi</b> <em id=\"p-em\">there</em>",
          "   <b id=\"p-b2\">guy</b>", "   <input type=\"checkbox\" id=\"checkbox-unchecked\" />",
          "   <input type=\"checkbox\" id=\"checkbox-disabled\" disabled=\"\" />",
          "   <input type=\"text\" id=\"text-checked\" checked=\"checked\" />",
          "   <input type=\"hidden\" />", "   <input type=\"hidden\" disabled=\"disabled\" />",
          "   <input type=\"checkbox\" id=\"checkbox-checked\" checked=\"checked\" />",
          "   <input type=\"checkbox\" id=\"checkbox-disabled-checked\"",
          "          disabled=\"disabled\" checked=\"checked\" />", "   <fieldset id=\"fieldset\" disabled=\"disabled\">",
          "     <input type=\"checkbox\" id=\"checkbox-fieldset-disabled\" />",
          "     <input type=\"hidden\" />", "   </fieldset>", " </p>",
          " <ol id=\"second-ol\">", " </ol>", " <map name=\"dummymap\">",
          "   <area shape=\"circle\" coords=\"200,250,25\" href=\"foo.html\" id=\"area-href\" />",
          "   <area shape=\"default\" id=\"area-nohref\" />", " </map>",
          "</div>", "<div id=\"foobar-div\" foobar=\"ab bc", "cde\"><span id=\"foobar-span\"></span></div>",
          "</body></html>"), collapse = "\n")

    library(xml2)
    document <- read_xml(HTML_IDS)
    gt <- GenericTranslator$new()
    ht <- HTMLTranslator$new()

    select_ids <- function(selector, html_only) {
        if (html_only) {
            xpath <- ht$css_to_xpath(selector)
            items <- xml_find_all(document, xpath)
        } else {
            xpath <- gt$css_to_xpath(selector)
            items <- xml_find_all(document, xpath)
        }
        n <- length(items)
        if (!n)
            return(NULL)
        result <- character(n)
        for (i in seq_len(n)) {
            element <- items[[i]]
            tmp <- xml_attr(element, "id")
            if (is.na(tmp))
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
                    tmp_res != result)
                    stop("Difference between results of selectors")
            }
        }
        result
    }

    all_ids <- pcss('*')
    expect_that(all_ids[1:6], equals(c('html', 'nil', 'link-href', 'link-nohref', 'nil', 'outer-div')))
    expect_that(tail(all_ids, 1), equals('foobar-span'))
    expect_that(pcss('div'), equals(c('outer-div', 'li-div', 'foobar-div')))
    expect_that(pcss('DIV', html_only=TRUE), equals(c('outer-div', 'li-div', 'foobar-div')))  # case-insensitive in HTML
    expect_that(pcss('div div'), equals('li-div'))
    expect_that(pcss('div, div div'), equals(c('outer-div', 'li-div', 'foobar-div')))
    expect_that(pcss('a[name]'), equals('name-anchor'))
    expect_that(pcss('a[NAme]', html_only=TRUE), equals('name-anchor')) # case-insensitive in HTML:
    expect_that(pcss('a[rel]'), equals(c('tag-anchor', 'nofollow-anchor')))
    expect_that(pcss('a[rel="tag"]'), equals('tag-anchor'))
    expect_that(pcss('a[href*="localhost"]'), equals('tag-anchor'))
    expect_that(pcss('a[href*=""]'), equals(NULL))
    expect_that(pcss('a[href^="http"]'), equals(c('tag-anchor', 'nofollow-anchor')))
    expect_that(pcss('a[href^="http:"]'), equals('tag-anchor'))
    expect_that(pcss('a[href^=""]'), equals(NULL))
    expect_that(pcss('a[href$="org"]'), equals('nofollow-anchor'))
    expect_that(pcss('a[href$=""]'), equals(NULL))
    expect_that(pcss('div[foobar~="bc"]', 'div[foobar~="cde"]'), equals('foobar-div'))
    expect_that(pcss('[foobar~="ab bc"]', c('[foobar~=""]', '[foobar~=" \t"]')), equals(NULL))
    expect_that(pcss('div[foobar~="cd"]'), equals(NULL))
    expect_that(pcss('*[lang|="En"]', '[lang|="En-us"]'), equals('second-li'))
    # Attribute values are case sensitive
    expect_that(pcss('*[lang|="en"]', '[lang|="en-US"]'), equals(NULL))
    expect_that(pcss('*[lang|="e"]'), equals(NULL))
    # ... :lang() is not.
    expect_that(pcss(':lang("EN")', '*:lang(en-US)', html_only=TRUE), equals(c('second-li', 'li-div')))
    expect_that(pcss(':lang("e")', html_only=TRUE), equals(NULL))
    expect_that(pcss('li:nth-child(-n)'), equals(NULL))
    expect_that(pcss('li:nth-child(n)'), equals(c('first-li', 'second-li', 'third-li', 'fourth-li', 'fifth-li', 'sixth-li', 'seventh-li')))
    expect_that(pcss('li:nth-child(3)'), equals('third-li'))
    expect_that(pcss('li:nth-child(10)'), equals(NULL))
    expect_that(pcss('li:nth-child(2n)', c('li:nth-child(even)', 'li:nth-child(2n+0)')), equals(c('second-li', 'fourth-li', 'sixth-li')))
    expect_that(pcss('li:nth-child(+2n+1)', 'li:nth-child(odd)'), equals(c('first-li', 'third-li', 'fifth-li', 'seventh-li')))
    expect_that(pcss('li:nth-child(2n+4)'), equals(c('fourth-li', 'sixth-li')))
    expect_that(pcss('li:nth-child(3n+1)'), equals(c('first-li', 'fourth-li', 'seventh-li')))
    expect_that(pcss('li:nth-child(-n+3)'), equals(c('first-li', 'second-li', 'third-li')))
    expect_that(pcss('li:nth-child(-2n+4)'), equals(c('second-li', 'fourth-li')))
    expect_that(pcss('li:nth-last-child(0)'), equals(NULL))
    expect_that(pcss('li:nth-last-child(1)'), equals('seventh-li'))
    expect_that(pcss('li:nth-last-child(2n)', 'li:nth-last-child(even)'), equals(c('second-li', 'fourth-li', 'sixth-li')))
    expect_that(pcss('li:nth-last-child(2n+2)'), equals(c('second-li', 'fourth-li', 'sixth-li')))
    expect_that(pcss('ol:first-of-type'), equals('first-ol'))
    expect_that(pcss('ol:nth-child(1)'), equals(NULL))
    expect_that(pcss('ol:nth-of-type(2)'), equals('second-ol'))
    expect_that(pcss('ol:nth-last-of-type(1)'), equals('second-ol'))
    expect_that(pcss('span:only-child'), equals('foobar-span'))
    expect_that(pcss('li div:only-child'), equals('li-div'))
    expect_that(pcss('div *:only-child'), equals(c('li-div', 'foobar-span')))
    #self.assertRaises(ExpressionError, pcss, 'p *:only-of-type')
    expect_that(pcss('p:only-of-type'), equals('paragraph'))
    expect_that(pcss('a:empty', 'a:EMpty'), equals('name-anchor'))
    expect_that(pcss('li:empty'), equals(c('third-li', 'fourth-li', 'fifth-li', 'sixth-li')))
    expect_that(pcss(':root', 'html:root'), equals('html'))
    expect_that(pcss('li:root', '* :root'), equals(NULL))
    expect_that(pcss('*:contains("link")', ':CONtains("link")'), equals(c('html', 'nil', 'outer-div', 'tag-anchor', 'nofollow-anchor')))
    expect_that(pcss('*:contains("LInk")'), equals(NULL))  # case sensitive
    expect_that(pcss('*:contains("e")'), equals(c('html', 'nil', 'outer-div', 'first-ol', 'first-li', 'paragraph', 'p-em')))
    expect_that(pcss('*:contains("E")'), equals(NULL))  # case-sensitive
    expect_that(pcss('.a', c('.b', '*.a', 'ol.a')), equals('first-ol'))
    expect_that(pcss('.c', '*.c'), equals(c('first-ol', 'third-li', 'fourth-li')))
    expect_that(pcss('ol *.c', c('ol li.c', 'li ~ li.c', 'ol > li.c')), equals(c('third-li', 'fourth-li')))
    expect_that(pcss('#first-li', c('li#first-li', '*#first-li')), equals('first-li'))
    expect_that(pcss('li div', c('li > div', 'div div')), equals('li-div'))
    expect_that(pcss('div > div'), equals(NULL))
    expect_that(pcss('div>.c', 'div > .c'), equals('first-ol'))
    expect_that(pcss('div + div'), equals('foobar-div'))
    expect_that(pcss('a ~ a'), equals(c('tag-anchor', 'nofollow-anchor')))
    expect_that(pcss('a[rel="tag"] ~ a'), equals('nofollow-anchor'))
    expect_that(pcss('ol#first-ol li:last-child'), equals('seventh-li'))
    expect_that(pcss('ol#first-ol *:last-child'), equals(c('li-div', 'seventh-li')))
    expect_that(pcss('#outer-div:first-child'), equals('outer-div'))
    expect_that(pcss('#outer-div :first-child'), equals(c('name-anchor', 'first-li', 'li-div', 'p-b', 'checkbox-fieldset-disabled', 'area-href')))
    expect_that(pcss('a[href]'), equals(c('tag-anchor', 'nofollow-anchor')))
    expect_that(pcss(':not(*)'), equals(NULL))
    expect_that(pcss('a:not([href])'), equals('name-anchor'))
    expect_that(pcss('ol :Not(li[class])'), equals(c('first-li', 'second-li', 'li-div', 'fifth-li', 'sixth-li', 'seventh-li')))
    # Invalid characters in XPath element names, should not crash
    expect_that(pcss('di\ua0v', 'div\\['), equals(NULL))
    expect_that(pcss('[h\ua0ref]', '[h\\]ref]'), equals(NULL))

    ## HTML-specific
    expect_that(pcss(':link', html_only=TRUE), equals(c('link-href', 'tag-anchor', 'nofollow-anchor', 'area-href')))
    expect_that(pcss(':visited', html_only=TRUE), equals(NULL))
    expect_that(pcss(':enabled', html_only=TRUE), equals(c('link-href', 'tag-anchor', 'nofollow-anchor', 'checkbox-unchecked', 'text-checked', 'checkbox-checked', 'area-href')))
    expect_that(pcss(':disabled', html_only=TRUE), equals(c('checkbox-disabled', 'checkbox-disabled-checked', 'fieldset', 'checkbox-fieldset-disabled')))
    expect_that(pcss(':checked', html_only=TRUE), equals(c('checkbox-checked', 'checkbox-disabled-checked')))
})
