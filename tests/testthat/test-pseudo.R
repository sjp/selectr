context("pseudo")

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

    expect_that(parse_one("foo"),
                equals(list("Element[foo]", NULL)))
    expect_that(parse_one("*"),
                equals(list("Element[*]", NULL)))
    expect_that(parse_one(":empty"),
                equals(list("Pseudo[Element[*]:empty]", NULL)))

    # Special cases for CSS 2.1 pseudo-elements
    expect_that(parse_one(":BEfore"),
                equals(list("Element[*]", "before")))
    expect_that(parse_one(":aftER"),
                equals(list("Element[*]", "after")))
    expect_that(parse_one(":First-Line"),
                equals(list("Element[*]", "first-line")))
    expect_that(parse_one(":First-Letter"),
                equals(list("Element[*]", "first-letter")))

    expect_that(parse_one("::befoRE"),
                equals(list("Element[*]", "before")))
    expect_that(parse_one("::AFter"),
                equals(list("Element[*]", "after")))
    expect_that(parse_one("::firsT-linE"),
                equals(list("Element[*]", "first-line")))
    expect_that(parse_one("::firsT-letteR"),
                equals(list("Element[*]", "first-letter")))

    expect_that(parse_one("::Selection"),
                equals(list("Element[*]", "selection")))
    expect_that(parse_one("foo:after"),
                equals(list("Element[foo]", "after")))
    expect_that(parse_one("foo::selection"),
                equals(list("Element[foo]", "selection")))
    expect_that(parse_one("lorem#ipsum ~ a#b.c[href]:empty::selection"),
                equals(list("CombinedSelector[Hash[Element[lorem]#ipsum] ~ Pseudo[Attrib[Class[Hash[Element[a]#b].c][href]]:empty]]", "selection")))

    expect_that(parse_pseudo("foo:before, bar, baz:after"),
                equals(list(list("Element[foo]", "before"),
                            list("Element[bar]", NULL),
                            list("Element[baz]", "after"))))
})

test_that("runtime-state pseudo-classes translate as never matching", {
    # Dynamic state that a static document does not have is accepted a
    # whole family at a time (so e.g. ':focus' and ':focus-within'
    # behave alike) and matches nothing; see the never-match table on
    # GenericTranslator in R/xpath.R
    for (translator in c("generic", "html", "xhtml")) {
        expect_that(css_to_xpath("a:focus", translator = translator),
                    equals("descendant-or-self::a[0]"))
        expect_that(css_to_xpath("a:focus-within", translator = translator),
                    equals("descendant-or-self::a[0]"))
        expect_that(css_to_xpath("a:focus-visible", translator = translator),
                    equals("descendant-or-self::a[0]"))
        expect_that(css_to_xpath("a:target-within", translator = translator),
                    equals("descendant-or-self::a[0]"))
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
    required_xpath <- paste(
        "@required and",
        "((name(.) = 'input' and not(@type = 'hidden')) or",
        "name(.) = 'select' or",
        "name(.) = 'textarea')")
    optional_xpath <- paste(
        "not(@required) and",
        "((name(.) = 'input' and not(@type = 'hidden')) or",
        "name(.) = 'select' or",
        "name(.) = 'textarea')")
    for (translator in c("html", "xhtml")) {
        expect_that(css_to_xpath("input:required", translator = translator),
                    equals(paste0("descendant-or-self::input[",
                                  required_xpath, "]")))
        expect_that(css_to_xpath("input:optional", translator = translator),
                    equals(paste0("descendant-or-self::input[",
                                  optional_xpath, "]")))
    }
    expect_that(css_to_xpath("input:required"),
                equals("descendant-or-self::input[0]"))
    expect_that(css_to_xpath("input:optional"),
                equals("descendant-or-self::input[0]"))

    # The other form-state pseudo-classes have no exact static
    # translation and stay unknown
    expect_error(css_to_xpath("input:read-only", translator = "html"),
                 "The pseudo-class :read-only is unknown", fixed = TRUE)
    expect_error(css_to_xpath("input:read-write", translator = "html"),
                 "The pseudo-class :read-write is unknown", fixed = TRUE)
    expect_error(css_to_xpath("input:placeholder-shown", translator = "html"),
                 "The pseudo-class :placeholder-shown is unknown", fixed = TRUE)
    expect_error(css_to_xpath("input:default", translator = "html"),
                 "The pseudo-class :default is unknown", fixed = TRUE)
    expect_error(css_to_xpath("input:indeterminate", translator = "html"),
                 "The pseudo-class :indeterminate is unknown", fixed = TRUE)
})

test_that(":required and :optional match form elements correctly", {
    library(xml2)

    form <- read_xml(paste0(
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
        xml_attr(results, "id")
    }

    # Only form elements that can take @required and have it; a hidden
    # input cannot be required, and a div's @required is meaningless
    expect_that(get_ids("*:required"),
                equals(c("i1", "s1", "t1")))

    # The rest of the same element set; non-form elements (and hidden
    # inputs) are neither :required nor :optional
    expect_that(get_ids("*:optional"),
                equals(c("i2", "s2", "t2")))

    expect_that(get_ids("input:required"), equals("i1"))
    expect_that(get_ids("select:optional"), equals("s2"))
})

test_that(":empty keeps the Selectors 3 white space semantics", {
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

    library(XML)
    doc <- xmlRoot(xmlParse(doc_xml))
    ids <- sapply(querySelectorAll(doc, "p:empty"),
                  function(x) xmlGetAttr(x, "id"))
    expect_that(ids, equals(c("truly-empty", "comment")))

    library(xml2)
    doc2 <- read_xml(doc_xml)
    expect_that(xml_attr(querySelectorAll(doc2, "p:empty"), "id"),
                equals(c("truly-empty", "comment")))
})

test_that(":any-link matches the same elements as :link", {
    # ':any-link' is ':link or :visited'; the static-document
    # convention treats every link as unvisited, so the HTML
    # translators give it the :link condition verbatim (the design
    # shared with selectrs: internal consistency over the spec-exact
    # a/area element set, which would omit 'link')
    link_xpath <- paste0(
        "descendant-or-self::e[@href and ",
        "(name(.) = 'a' or name(.) = 'link' or name(.) = 'area')]")
    for (translator in c("html", "xhtml")) {
        expect_that(css_to_xpath("e:any-link", translator = translator),
                    equals(link_xpath))
        expect_that(css_to_xpath("e:any-link", translator = translator),
                    equals(css_to_xpath("e:link", translator = translator)))
    }

    # The generic translator has no link semantics: never matches
    expect_that(css_to_xpath("e:any-link"),
                equals("descendant-or-self::e[0]"))
})
