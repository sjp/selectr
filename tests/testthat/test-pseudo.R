test_that("parser parses canonical pseudo element expressions", {
    parse_pseudo <- function(css) {
        selectors <- lapply(css, function(x) parse(x))
        n <- length(selectors)
        results <- list()
        for (i in seq_len(n)) {
            selector <- selectors[[i]]
            if (is.list(selector)) {
                results[[i]] <- lapply(selector,
                                       function(x) {
                                           el <- x
                                           pseudo <- x$pseudo_element
                                           el$pseudo_element <- NULL
                                           list(el$repr(), pseudo)
                                       })
            } else {
                pseudo <- selector$pseudo_element
                selector$pseudo_element <- NULL
                results[[i]] <- list(list(selector$repr(), pseudo))
            }
        }
        if (n)
            results[[seq_len(n)]]
        else
            results
    }

    parse_one <- function(css) {
        result <- parse_pseudo(css)
        #if (length(result) != 1)
        #    stop("More than one result attempting to be parsed.")
        result[[1]]
    }

    expect_equal(parse_one("foo"),
                 list("Element[foo]", NULL))
    expect_equal(parse_one("*"),
                 list("Element[*]", NULL))
    expect_equal(parse_one(":empty"),
                 list("Pseudo[Element[*]:empty]", NULL))

    # Special cases for CSS 2.1 pseudo-elements
    expect_equal(parse_one(":BEfore"),
                 list("Element[*]", "before"))
    expect_equal(parse_one(":aftER"),
                 list("Element[*]", "after"))
    expect_equal(parse_one(":First-Line"),
                 list("Element[*]", "first-line"))
    expect_equal(parse_one(":First-Letter"),
                 list("Element[*]", "first-letter"))

    expect_equal(parse_one("::befoRE"),
                 list("Element[*]", "before"))
    expect_equal(parse_one("::AFter"),
                 list("Element[*]", "after"))
    expect_equal(parse_one("::firsT-linE"),
                 list("Element[*]", "first-line"))
    expect_equal(parse_one("::firsT-letteR"),
                 list("Element[*]", "first-letter"))

    expect_equal(parse_one("::Selection"),
                 list("Element[*]", "selection"))
    expect_equal(parse_one("foo:after"),
                 list("Element[foo]", "after"))
    expect_equal(parse_one("foo::selection"),
                 list("Element[foo]", "selection"))
    expect_equal(parse_one("lorem#ipsum ~ a#b.c[href]:empty::selection"),
                 list("CombinedSelector[Hash[Element[lorem]#ipsum] ~ Pseudo[Attrib[Class[Hash[Element[a]#b].c][href]]:empty]]", "selection"))

    expect_equal(parse_pseudo("foo:before, bar, baz:after"),
                 list(list("Element[foo]", "before"),
                      list("Element[bar]", NULL),
                      list("Element[baz]", "after")))
})

test_that("runtime-state pseudo-classes translate as never matching", {
    # Dynamic state that a static document does not have is accepted a
    # whole family at a time (so e.g. ':focus' and ':focus-within'
    # behave alike) and matches nothing; see the never-match table on
    # GenericTranslator in R/xpath.R
    for (translator in c("generic", "html", "xhtml")) {
        expect_equal(css_to_xpath("a:focus", translator = translator),
                     "descendant-or-self::a[0]")
        expect_equal(css_to_xpath("a:focus-within", translator = translator),
                     "descendant-or-self::a[0]")
        expect_equal(css_to_xpath("a:focus-visible", translator = translator),
                     "descendant-or-self::a[0]")
        expect_equal(css_to_xpath("a:target-within", translator = translator),
                     "descendant-or-self::a[0]")
    }

    # Pseudo-classes outside the accepted families still error, so
    # typos stay detectable
    expect_error(css_to_xpath("a:focused"),
                 "The pseudo-class :focused is unknown", fixed = TRUE)
})

test_that(":required and :optional translate from the @required attribute", {
    # HTML form state readable from a document attribute: a real
    # translation on the HTML translator (like :checked), never-match
    # on the generic translator
    not_hidden <- paste0(
        "not(translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
        "'abcdefghijklmnopqrstuvwxyz') = 'hidden')")
    # 'input' is the only element in the required/optional set the HTML
    # translator prunes against here, so the compound's known element
    # ('input') drops the 'select'/'textarea' disjuncts and their
    # local-name() tests entirely - see add_disjunction() in R/xpath.R
    required_xpath <- paste("@required and", not_hidden)
    optional_xpath <- paste("not(@required) and", not_hidden)
    for (translator in c("html", "xhtml")) {
        expect_equal(css_to_xpath("input:required", translator = translator),
                     paste0("descendant-or-self::input[",
                            required_xpath, "]"))
        expect_equal(css_to_xpath("input:optional", translator = translator),
                     paste0("descendant-or-self::input[",
                            optional_xpath, "]"))
    }
    expect_equal(css_to_xpath("input:required"),
                 "descendant-or-self::input[0]")
    expect_equal(css_to_xpath("input:optional"),
                 "descendant-or-self::input[0]")

    # ':indeterminate' has no exact static translation (see
    # 021-html-translator-static-form-pseudo-classes.md: a 'progress'-only
    # implementation was rejected as violating the "family in full or not
    # at all" policy, and the radio-group and checkbox cases are not
    # expressible in XPath 1.0/need live state) and stays unknown
    expect_error(css_to_xpath("input:indeterminate", translator = "html"),
                 "The pseudo-class :indeterminate is unknown", fixed = TRUE)
})

test_that(":required and :optional match form elements correctly", {
    skip_if_not_installed("xml2")

    form <- xml2::read_xml(paste0(
        '<form>',
        '<input id="i1" type="text" required="required"/>',
        '<input id="i2" type="text"/>',
        '<input id="i3" type="hidden" required="required"/>',
        '<input id="i4" type="hidden"/>',
        '<select id="s1" required="required"/>',
        '<select id="s2"/>',
        '<textarea id="t1" required="required"/>',
        '<textarea id="t2"/>',
        '<button id="b1"/>',
        '<div id="d1" required="required"/>',
        '</form>'
    ))

    get_ids <- function(css) {
        results <- querySelectorAll(form, css, translator = "html")
        xml2::xml_attr(results, "id")
    }

    # Only form elements that can take @required and have it; a hidden
    # input cannot be required, and a div's @required is meaningless
    expect_equal(get_ids("*:required"),
                 c("i1", "s1", "t1"))

    # The rest of the same element set; non-form elements (and hidden
    # inputs) are neither :required nor :optional
    expect_equal(get_ids("*:optional"),
                 c("i2", "s2", "t2"))

    expect_equal(get_ids("input:required"), "i1")
    expect_equal(get_ids("select:optional"), "s2")
})

test_that(paste(":read-write, :read-only, :placeholder-shown and",
                 ":default are never-matching on the generic translator"), {
    for (css in c("a:read-write", "a:read-only", "a:placeholder-shown",
                  "a:default")) {
        expect_equal(css_to_xpath(css),
                     paste0("descendant-or-self::a[0]"))
    }
})

test_that(":read-write and :read-only classify editable HTML elements", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<form id="form1">',
        '<input id="i1" type="text"/>',
        '<input id="i2" type="text" readonly="readonly"/>',
        '<input id="i3" type="text" disabled="disabled"/>',
        '<input id="i4" type="checkbox"/>',
        '<input id="i5"/>',
        '<textarea id="t1"/>',
        '<textarea id="t2" readonly="readonly"/>',
        '<fieldset id="fs1" disabled="disabled">',
        '<input id="i6" type="text"/></fieldset>',
        '<div id="d1" contenteditable="true">x</div>',
        '<div id="d2" contenteditable="">x</div>',
        '<div id="d3" contenteditable="false">x</div>',
        '<div id="d4">x</div>',
        '</form>'
    ))
    get_ids <- function(css) {
        xml2::xml_attr(querySelectorAll(doc, css, translator = "html"), "id")
    }

    # 'i4' (checkbox) does not support @readonly, so it is neither
    # :read-write nor :read-only in the strict sense - but per the HTML
    # definition it is still classified :read-only (not editable text);
    # 'i5' has no @type, which defaults to 'text' and so is read-write.
    # ':read-only' is the negation of ':read-write' over every element,
    # so non-form containers ('form1', 'fs1') are read-only too
    expect_equal(get_ids(":read-write"),
                 c("i1", "i5", "t1", "d1", "d2"))
    expect_equal(get_ids(":read-only"),
                 c("form1", "i2", "i3", "i4", "t2", "fs1", "i6", "d3", "d4"))

    # Pruned against a known element, both still agree with the
    # unpruned form
    expect_equal(get_ids("input:read-write"), c("i1", "i5"))
    expect_equal(get_ids("textarea:read-write"), "t1")
    expect_equal(get_ids("div:read-write"), c("d1", "d2"))
    expect_equal(get_ids("input:read-only"), c("i2", "i3", "i4", "i6"))
    expect_equal(get_ids("textarea:read-only"), "t2")
})

test_that(":placeholder-shown matches an empty placeholder-bearing control", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<form>',
        '<input id="i1" placeholder="hi" value=""/>',
        '<input id="i2" placeholder="hi" value="filled"/>',
        '<input id="i3" placeholder="hi"/>',
        '<input id="i4" value=""/>',
        '<textarea id="t1" placeholder="hi"></textarea>',
        '<textarea id="t2" placeholder="hi">filled</textarea>',
        '</form>'
    ))
    get_ids <- function(css) {
        xml2::xml_attr(querySelectorAll(doc, css, translator = "html"), "id")
    }

    expect_equal(get_ids(":placeholder-shown"), c("i1", "i3", "t1"))
})

test_that(":default matches selected/checked controls and the first submit button", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<div>',
        '<form>',
        '<select><option id="o1"/><option id="o2" selected="selected"/></select>',
        '<input id="c1" type="checkbox" checked="checked"/>',
        '<input id="c2" type="radio"/>',
        '<button id="b1">First</button>',
        '<button id="b2">Second</button>',
        '<input id="s1" type="submit"/>',
        '</form>',
        '<button id="nf1">No form</button>',
        '</div>'
    ))
    get_ids <- function(css) {
        xml2::xml_attr(querySelectorAll(doc, css, translator = "html"), "id")
    }

    # 'o2' (selected), 'c1' (checked) and 'b1' (first submit button in
    # its form, in document order) match; the later 'b2' and 's1' do
    # not, nor does the form-less 'nf1'
    expect_equal(get_ids(":default"), c("o2", "c1", "b1"))
})

test_that(":default finds the enclosing form in the XHTML namespace", {
    skip_if_not_installed("XML")
    skip_if_not_installed("xml2")
    # The "first submit button of its nearest enclosing form" branch
    # walks up to a <form>, and that walk has to match by local name
    # like every other element reference in the HTML pseudo-classes, or
    # a form in the default XHTML namespace is invisible to it
    doc_xml <- paste0(
        '<html xmlns="http://www.w3.org/1999/xhtml"><body>',
        '<form><input id="s1" type="submit"/>',
        '<input id="s2" type="submit"/></form>',
        '<form><button id="b1">Go</button>',
        '<input id="s3" type="submit"/></form>',
        '<input id="nf" type="submit"/>',
        '</body></html>'
    )
    ns <- c(h = "http://www.w3.org/1999/xhtml")

    doc2 <- xml2::read_xml(doc_xml)
    ids2 <- function(css) {
        xml2::xml_attr(querySelectorAll(doc2, css, translator = "xhtml"), "id")
    }
    # the first submit control of each form, and nothing outside one
    expect_equal(ids2("*|input:default"), "s1")
    expect_equal(ids2("*|button:default"), "b1")
    expect_equal(ids2(":default"), c("s1", "b1"))
    expect_equal(xml2::xml_attr(querySelectorAllNS(doc2, "h|input:default", ns,
                                             translator = "xhtml"), "id"),
                 "s1")

    doc <- XML::xmlParse(doc_xml)
    ids <- function(css) {
        sapply(querySelectorAllNS(doc, css, ns, translator = "xhtml"),
               XML::xmlGetAttr, "id")
    }
    expect_equal(ids("h|input:default"), "s1")
    expect_equal(ids("h|button:default"), "b1")
})

test_that(":empty keeps the Selectors 3 white space semantics", {
    skip_if_not_installed("XML")
    skip_if_not_installed("xml2")
    # Deliberate, browser-verified decision (see xpath_empty_pseudo):
    # a white-space-only element does not match :empty, matching what
    # every browser implements rather than the Selectors 4 loosening
    doc_xml <- paste0(
        '<root>',
        '<p id="truly-empty"></p>',
        '<p id="space"> </p>',
        '<p id="newline">\n  </p>',
        '<p id="text">x</p>',
        '<p id="child"><span/></p>',
        '<p id="comment"><!-- c --></p>',
        '</root>'
    )

    doc <- XML::xmlRoot(XML::xmlParse(doc_xml))
    ids <- sapply(querySelectorAll(doc, "p:empty"),
                  function(x) XML::xmlGetAttr(x, "id"))
    expect_equal(ids, c("truly-empty", "comment"))

    doc2 <- xml2::read_xml(doc_xml)
    expect_equal(xml2::xml_attr(querySelectorAll(doc2, "p:empty"), "id"),
                 c("truly-empty", "comment"))
})

test_that(":any-link matches the same elements as :link", {
    # ':any-link' is ':link or :visited'; the static-document
    # convention treats every link as unvisited, so the HTML
    # translators give it the :link condition verbatim.
    # 'e' names neither of ':link''s elements (a, area), so once the
    # HTML translator prunes against the compound's known element, the
    # predicate is a bare, always-false '0' - see add_disjunction() in
    # R/xpath.R
    link_xpath <- "descendant-or-self::e[0]"
    for (translator in c("html", "xhtml")) {
        expect_equal(css_to_xpath("e:any-link", translator = translator),
                     link_xpath)
        expect_equal(css_to_xpath("e:any-link", translator = translator),
                     css_to_xpath("e:link", translator = translator))
    }

    # A <link> is metadata rather than a hyperlink, so it is outside
    # ':link''s element set and prunes away just as 'e' does
    for (translator in c("html", "xhtml")) {
        expect_equal(css_to_xpath("link:link", translator = translator),
                     "descendant-or-self::link[0]")
        expect_equal(css_to_xpath("link:any-link", translator = translator),
                     "descendant-or-self::link[0]")
    }
    expect_equal(css_to_xpath(":link", translator = "html"),
                 paste0("descendant-or-self::*[local-name(.) = 'a' and ",
                        "(@href) or local-name(.) = 'area' and (@href)]"))

    # The generic translator has no link semantics: never matches
    expect_equal(css_to_xpath("e:any-link"),
                 "descendant-or-self::e[0]")
})

test_that("HTML pseudo-classes prune disjuncts the compound rules out", {
    # add_disjunction() (R/xpath.R) drops a form-state pseudo-class's
    # per-element disjuncts once the compound's element is known to be
    # something else - e.g. 'input:checked' no longer carries the
    # 'option' disjunct. Confirm both the shrunk XPath text and that the
    # pruned and unpruned forms select the same nodes
    skip_if_not_installed("xml2")

    fold <- paste0("translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                   "'abcdefghijklmnopqrstuvwxyz')")
    expect_equal(
        css_to_xpath("input:checked", prefix = "", translator = "html"),
        paste0("input[@checked and (", fold, " = 'checkbox' or ",
              fold, " = 'radio')]"))
    expect_equal(css_to_xpath("option:checked", prefix = "",
                              translator = "html"),
                 "option[@selected]")
    # 'p' is in neither disjunct: the predicate collapses to '0'
    expect_equal(css_to_xpath("p:checked", prefix = "", translator = "html"),
                 "p[0]")
    expect_equal(css_to_xpath("button:required", prefix = "",
                              translator = "html"),
                 "button[0]")

    doc <- xml2::read_xml(paste0(
        "<form>",
        "<input id='i1' type='checkbox' checked='checked'/>",
        "<option id='o1' selected='selected'>x</option>",
        "<option id='o2'>y</option>",
        "<p id='p1'>z</p>",
        "</form>"))
    get_ids <- function(css) {
        xml2::xml_attr(querySelectorAll(doc, css, translator = "html"), "id")
    }
    expect_equal(get_ids("input:checked"), "i1")
    expect_equal(get_ids("option:checked"), "o1")
    expect_equal(get_ids("p:checked"), character(0))
    # The pruned, single-element form still matches the same nodes as
    # the unpruned, element-less form
    expect_equal(get_ids(":checked"), c("i1", "o1"))
})

test_that("pseudo-class names spelled with underscores are unknown", {
    # Method dispatch maps '-' to '_', which must not let an
    # underscore spelling alias the hyphenated pseudo-class
    expect_error(css_to_xpath("a:first_child"), class = "selectr_translation_error")
    expect_error(css_to_xpath("a:first_child"),
                 "The pseudo-class :first_child is unknown", fixed = TRUE)
    expect_error(css_to_xpath("a:nth_child(2)"), class = "selectr_translation_error")
    expect_error(css_to_xpath("a:nth_child(2)"),
                 "The pseudo-class :nth_child() is unknown", fixed = TRUE)
    expect_error(css_to_xpath("a:nth-of_type(2)"),
                 "The pseudo-class :nth-of_type() is unknown", fixed = TRUE)
    expect_error(css_to_xpath("p:last_of_type"),
                 "The pseudo-class :last_of_type is unknown", fixed = TRUE)
    expect_error(css_to_xpath(":any_link", translator = "html"),
                 "The pseudo-class :any_link is unknown", fixed = TRUE)

    # The hyphenated spellings are unaffected
    expect_equal(css_to_xpath("a:first-child"),
                 "descendant-or-self::a[count(preceding-sibling::*) = 0]")
    expect_equal(css_to_xpath("a:nth-child(2)"),
                 "descendant-or-self::a[count(preceding-sibling::*) = 1]")
    expect_equal(css_to_xpath(":any-link", translator = "html"),
                 css_to_xpath(":link", translator = "html"))
})

test_that("a pseudo-element is rejected at translation time", {
    # parse() accepts a trailing pseudo-element (it is only invalid
    # mid-selector, e.g. ":before a"); css_to_xpath() has nothing to
    # translate it to, so it is rejected once translation begins
    expect_error(css_to_xpath("a::before"), class = "selectr_translation_error")
    expect_error(css_to_xpath("a::before"),
                 "^Pseudo-elements are not supported\\.$")
    expect_error(css_to_xpath("::after"),
                 "^Pseudo-elements are not supported\\.$")
    expect_error(css_to_xpath("a:before"),
                 "^Pseudo-elements are not supported\\.$")
})
