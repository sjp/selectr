test_that("selection works correctly on a large barrage of tests", {
    HTML_IDS <- fixture_html_ids()

    skip_if_not_installed("xml2")
    document <- xml2::read_xml(HTML_IDS)
    gt <- GenericTranslator$new()
    ht <- HTMLTranslator$new()

    select_ids <- function(selector, html_only) {
        if (html_only) {
            xpath <- ht$css_to_xpath(selector)
            items <- xml2::xml_find_all(document, xpath)
        } else {
            xpath <- gt$css_to_xpath(selector)
            items <- xml2::xml_find_all(document, xpath)
        }
        n <- length(items)
        if (!n)
            return(NULL)
        result <- character(n)
        for (i in seq_len(n)) {
            element <- items[[i]]
            tmp <- xml2::xml_attr(element, "id")
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
    expect_equal(pcss('a:not(:not([href]))', 'a[href]'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss('li:is(:not([class]))'), c('first-li', 'second-li', 'fifth-li', 'sixth-li', 'seventh-li'))
    expect_equal(pcss('ol:has(:not(li))'), 'first-ol')

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

    # Complex selectors inside functional pseudo-classes (selectors-4)
    expect_equal(pcss(':is(ol li)'), c('first-li', 'second-li', 'third-li', 'fourth-li', 'fifth-li', 'sixth-li', 'seventh-li'))
    expect_equal(pcss(':is(#outer-div > a)'), c('name-anchor', 'tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss(':is(a + a)'), c('tag-anchor', 'nofollow-anchor'))
    expect_equal(pcss(':is(a ~ ol)'), c('first-ol', 'second-ol'))
    expect_equal(pcss('li:not(ol li)'), NULL)
    expect_equal(pcss(':where(ol > li)'), c('first-li', 'second-li', 'third-li', 'fourth-li', 'fifth-li', 'sixth-li', 'seventh-li'))
    expect_equal(pcss('div:has(ol li)'), 'outer-div')
    expect_equal(pcss(':has(> li + li)'), 'first-ol')
    expect_equal(pcss('li:nth-child(2 of ol li)'), 'second-li')

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

test_that("of-type pseudo-classes work on unsafe element names", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(paste0('<r><é id="first"/><b id="b"/>',
                           '<é id="second"/><x id="only"/></r>'))
    ids <- function(css) {
        result <- unlist(lapply(querySelectorAll(doc, css), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    expect_equal(ids('é:first-of-type'), 'first')
    expect_equal(ids('é:last-of-type'), 'second')
    expect_equal(ids('é:nth-of-type(2)'), 'second')
    expect_equal(ids('é:nth-last-of-type(2)'), 'first')
    expect_equal(ids('é:only-of-type'), NULL)
    expect_equal(ids('x:only-of-type'), 'only')
})

test_that(":only-child and :only-of-type match the root element", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml("<root><a/></root>")
    count <- function(css)
        length(xml2::xml_find_all(doc, css_to_xpath(css)))

    # :only-child is defined as :first-child:last-child, which matches
    # the root element, so :only-child must match it too
    expect_equal(count('root:first-child:last-child'), 1)
    expect_equal(count('root:only-child'), 1)
    expect_equal(count('root:only-of-type'), 1)
    expect_equal(count('a:only-child'), 1)
    expect_equal(count('a:only-of-type'), 1)
})

test_that(":enabled and :disabled match inputs with no type attribute", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_html(paste0('<form>',
                            '<input id="plain-disabled" disabled="" />',
                            '<input id="plain-enabled" />',
                            '<input type="hidden" id="hidden-disabled" disabled="" />',
                            '<input type="hidden" id="hidden-plain" />',
                            '</form>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    # An <input> with no type attribute defaults to type=text, so it should
    # participate in :enabled/:disabled; type=hidden inputs never do.
    expect_equal(ids('input:disabled'), 'plain-disabled')
    expect_equal(ids('input:enabled'), 'plain-enabled')
})

test_that("form pseudo-classes fold @type case-insensitively", {
    skip_if_not_installed("xml2")
    # type is an enumerated attribute whose keywords match ASCII
    # case-insensitively; an HTML parser preserves the attribute value,
    # so uppercase spellings must still be recognised
    doc <- xml2::read_html(paste0('<form>',
                            '<input id="radio-up" type="RADIO" checked="checked" />',
                            '<input id="check-up" type="CheckBox" checked="checked" />',
                            '<input id="hidden-up" type="HIDDEN" disabled="disabled" />',
                            '<input id="text-up" type="TEXT" disabled="disabled" />',
                            '<input id="hidden-req" type="Hidden" required="required" />',
                            '<input id="text-req" type="Text" required="required" />',
                            '</form>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    # type=RADIO / type=CheckBox are checkable controls
    expect_equal(ids('input:checked'), c('radio-up', 'check-up'))
    # type=HIDDEN is excluded from :disabled, the uppercase text input is not
    expect_equal(ids('input:disabled'), 'text-up')
    # likewise type=Hidden cannot be :required
    expect_equal(ids('input:required'), 'text-req')
})

test_that(":disabled/:enabled honour the disabled-fieldset legend carve-out", {
    skip_if_not_installed("xml2")
    # A disabled <fieldset> disables its descendant controls except those
    # inside its first <legend> child. Nested disabled fieldsets still
    # disable a control protected by only one legend
    doc <- xml2::read_html(paste0(
        '<form>',
        '<fieldset disabled="disabled">',
        '<legend><input id="in-legend" /></legend>',
        '<input id="in-body" />',
        '<legend><input id="second-legend" /></legend>',
        '</fieldset>',
        '<fieldset disabled="disabled">',
        '<legend>',
        '<fieldset disabled="disabled">',
        '<input id="nested-in-body" />',
        '</fieldset>',
        '</legend>',
        '</fieldset>',
        '</form>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    # Inside the first legend: enabled. In the body or a second legend:
    # disabled. The nested input has two disabled-fieldset ancestors but
    # only one protecting legend, so it stays disabled
    expect_equal(ids('input:disabled'),
                 c('in-body', 'second-legend', 'nested-in-body'))
    expect_equal(ids('input:enabled'), 'in-legend')
})

test_that(":disabled/:enabled partition options under a disabled optgroup", {
    skip_if_not_installed("xml2")
    # An <option> is "actually disabled" when its own @disabled is set or
    # when the nearest <optgroup> above it is disabled, so the two
    # pseudo-classes must partition the options
    doc <- xml2::read_html(paste0(
        '<select>',
        '<optgroup id="off" disabled="disabled">',
        '<option id="in-off">a</option>',
        '</optgroup>',
        '<optgroup id="on">',
        '<option id="own-disabled" disabled="disabled">b</option>',
        '<option id="plain">c</option>',
        '</optgroup>',
        '</select>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    expect_equal(ids('option:disabled'), c('in-off', 'own-disabled'))
    expect_equal(ids('option:enabled'), 'plain')
    # the optgroups themselves follow their own @disabled
    expect_equal(ids('optgroup:disabled'), 'off')
    expect_equal(ids('optgroup:enabled'), 'on')
})

test_that("a disabled <select> disables the optgroups and options below it", {
    skip_if_not_installed("xml2")
    # HTML's "actually disabled" makes an <optgroup> or an <option>
    # disabled when its nearest ancestor <select> is disabled, without
    # any @disabled of its own; the <option> walk also finds an
    # <optgroup> above it that is not its parent. Both walks stop at a
    # <select>, <datalist>, <hr> or <option>, so the option inside the
    # <datalist> is not reached by the disabled <select> around it
    doc <- xml2::read_html(paste0(
        '<select id="s1" disabled="disabled">',
        '<option id="o1">A</option>',
        '<optgroup id="og1"><option id="o2">B</option></optgroup>',
        '</select>',
        '<select id="s2">',
        '<optgroup id="og2" disabled="disabled">',
        '<option id="o3">C</option>',
        '<div id="d1"><option id="o4">D</option></div>',
        '</optgroup>',
        '</select>',
        '<select id="s3" disabled="disabled">',
        '<datalist id="dl"><option id="o5">E</option></datalist>',
        '</select>',
        '<optgroup id="og3" disabled="disabled">',
        '<option id="o6">F</option></optgroup>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    # every option and optgroup under a disabled select is disabled,
    # and o4 is disabled by the optgroup two levels above it
    expect_equal(ids('option:disabled'), c('o1', 'o2', 'o3', 'o4', 'o6'))
    expect_equal(ids('option:enabled'), 'o5')
    expect_equal(ids('optgroup:disabled'), c('og1', 'og2', 'og3'))
    expect_equal(ids('optgroup:enabled'), NULL)
    # the selects themselves follow their own @disabled only
    expect_equal(ids('select:disabled'), c('s1', 's3'))
    expect_equal(ids('select:enabled'), 's2')
    # a descendant combinator through the disabled select agrees with
    # the option/optgroup pseudo-classes
    expect_equal(ids('select:disabled option'), c('o1', 'o2', 'o5'))
    expect_equal(ids(':enabled'), c('s2', 'o5'))
})

test_that(":enabled/:disabled cover the form elements and nothing else", {
    skip_if_not_installed("xml2")
    # ':enabled' and ':disabled' partition the elements HTML allows to
    # be "actually disabled" and match nothing outside that set: not
    # hyperlinks (which an early draft made ':enabled', though no
    # browser matches 'a:enabled'), and not the obsolete <command> and
    # <keygen>, which ':checked' ignores as well
    doc <- xml2::read_html(paste0(
        '<body>',
        '<a id="link" href="#x">a</a>',
        '<form>',
        '<button id="button-on"></button>',
        '<button id="button-off" disabled="disabled"></button>',
        '<input id="input-on" />',
        '<input id="input-off" disabled="disabled" />',
        '<input id="check-on" type="checkbox" checked="checked" />',
        '<select id="select-on">',
        '<optgroup id="optgroup-on"><option id="option-on">a</option></optgroup>',
        '</select>',
        '<select id="select-off" disabled="disabled">',
        '<optgroup id="optgroup-off" disabled="disabled">',
        '<option id="option-off">b</option></optgroup>',
        '</select>',
        '<textarea id="textarea-on"></textarea>',
        '<textarea id="textarea-off" disabled="disabled"></textarea>',
        '<fieldset id="fieldset-on"></fieldset>',
        '<fieldset id="fieldset-off" disabled="disabled"></fieldset>',
        '<keygen id="keygen-el" />',
        '<command id="command-el" type="checkbox" checked="checked" />',
        '</form>',
        '</body>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "html")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath), xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    expect_equal(ids(':enabled'),
                 c('button-on', 'input-on', 'check-on', 'select-on',
                   'optgroup-on', 'option-on', 'textarea-on',
                   'fieldset-on'))
    expect_equal(ids(':disabled'),
                 c('button-off', 'input-off', 'select-off',
                   'optgroup-off', 'option-off', 'textarea-off',
                   'fieldset-off'))

    # a link is neither, and stays selectable as a link
    expect_equal(ids('a[href]:enabled'), NULL)
    expect_equal(ids('a[href]:disabled'), NULL)
    expect_equal(ids('a:link'), 'link')

    # the obsolete elements are matched by none of the three
    expect_equal(ids('keygen:enabled'), NULL)
    expect_equal(ids('command:enabled'), NULL)
    expect_equal(ids('command:disabled'), NULL)
    expect_equal(ids(':checked'), 'check-on')
})

test_that("HTML pseudo-classes see elements in the default XHTML namespace", {
    skip_if_not_installed("xml2")
    # The xhtml translator tells its users to write '*|input' rather than
    # 'input' so a type selector matches an element in the default XHTML
    # namespace. The pseudo-class conditions must follow the same rule
    # so a selector that gets the subject right still gets a right answer
    doc <- xml2::read_xml(paste0(
        '<html xmlns="http://www.w3.org/1999/xhtml"><body>',
        '<fieldset disabled="">',
        '<legend><input id="protected" /></legend>',
        '<input id="inside" />',
        '</fieldset>',
        '<select><optgroup disabled=""><option id="o1">a</option></optgroup></select>',
        '<select disabled=""><option id="o2">b</option></select>',
        '<input id="free" />',
        '</body></html>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "xhtml")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath, ns = xml2::xml_ns(doc)),
                                 xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    expect_equal(ids('*|input:disabled'), 'inside')
    expect_equal(ids('*|input:enabled'), c('protected', 'free'))
    expect_equal(ids('*|option:disabled'), c('o1', 'o2'))
    expect_equal(ids('*|option:enabled'), NULL)
})

test_that("HTML pseudo-classes see elements in a prefix-bound XHTML namespace", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(paste0(
        '<h:html xmlns:h="http://www.w3.org/1999/xhtml"><h:body>',
        '<h:select><h:option id="p1" selected="selected">a</h:option></h:select>',
        '<h:a id="lnk" href="x" />',
        '<h:input id="req" required="required" />',
        '</h:body></h:html>'))
    ids <- function(css) {
        xpath <- css_to_xpath(css, translator = "xhtml")
        result <- unlist(lapply(xml2::xml_find_all(doc, xpath, ns = xml2::xml_ns(doc)),
                                 xml2::xml_attr, "id"))
        if (is.null(result)) NULL else result
    }

    expect_equal(ids('h|option:checked'), 'p1')
    expect_equal(ids('h|a:link'), 'lnk')
    expect_equal(ids('h|input:required'), 'req')
    expect_equal(ids('h|input:enabled'), 'req')
})
