test_that("XPathExpr objects print correctly", {
    shw <- function(x) trimws(capture.output(x$show()))

    xp <- XPathExpr()
    expect_equal(xp$repr(), "XPathExpr[*]")
    expect_equal(shw(xp), "XPathExpr[*]")

    xp <- XPathExpr("//")
    expect_equal(xp$repr(), "XPathExpr[//*]")
    expect_equal(shw(xp), "XPathExpr[//*]")

    xp <- XPathExpr(element = "a")
    expect_equal(xp$repr(), "XPathExpr[a]")
    expect_equal(shw(xp), "XPathExpr[a]")

    xp <- XPathExpr("//a/", "b")
    expect_equal(xp$repr(), "XPathExpr[//a/b]")
    expect_equal(shw(xp), "XPathExpr[//a/b]")
})

# The XPath the :lang() translation uses to cut a tail of language
# subtags just after its first singleton subtag (RFC 4647 step 3.C):
# every letter and digit maps to 'a', so a singleton reads "-a-"
singleton_cut <- function(tail) {
    paste0("substring(", tail, ", 1, string-length(substring-before(",
           "concat(translate(concat('-', ", tail, "), ",
           "'abcdefghijklmnopqrstuvwxyz0123456789', ",
           "'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'), 'a-'), '-a-')) + 2)")
}

test_that("Generic translator validates language arguments", {
    translator <- GenericTranslator$new()
    expect_equal(translator$css_to_xpath("xml:lang(en)"), "descendant-or-self::xml[lang('en')]")
    # "en-nz" names two subtags, so RFC 4647 extended filtering applies,
    # which XPath's lang() cannot express: the nearest xml:lang is
    # matched the way the HTML translator matches lang
    xlc <- "translate(@xml:lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"
    expect_equal(translator$css_to_xpath("xml:lang(en-nz)"),
                 paste0("descendant-or-self::xml[ancestor-or-self::*[@xml:lang][1][",
                        "starts-with(concat(", xlc, ", '-'), 'en-') and ",
                        "contains(concat('-', ",
                        singleton_cut(paste0("substring-after(concat(", xlc,
                                             ", '-'), 'en-')")),
                        "), '-nz-')]]"))

    expect_error(translator$css_to_xpath("xml:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("xml:lang(1)"), "Expected string, ident, or \\* arguments.*")
    # The reported argument is the offending one, not the first
    expect_error(translator$css_to_xpath("xml:lang(en, 5)"),
                 "Expected string, ident, or \\* arguments for :lang\\(\\), got <NUMBER '5' at 14>")

    # Multiple languages with OR logic
    expect_equal(translator$css_to_xpath("xml:lang(en, fr)"), "descendant-or-self::xml[lang('en') or lang('fr')]")
    expect_equal(translator$css_to_xpath("xml:lang(en, de, fr)"), "descendant-or-self::xml[lang('en') or lang('de') or lang('fr')]")
})

test_that("HTML translator validates language arguments", {
    translator <- HTMLTranslator$new()
    lc <- "translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"
    expect_equal(translator$css_to_xpath("html:lang(en)"), "descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]")
    # "en-nz" names two subtags, so RFC 4647 extended filtering applies
    # (see the "extended-filtering exact ranges" test below) rather than
    # the single-subtag prefix test used for "en" above. The "nz" is
    # searched for only up to the first singleton after the "en".
    expect_equal(translator$css_to_xpath("html:lang(en-nz)"),
                 paste0("descendant-or-self::html[ancestor-or-self::*[@lang][1][",
                        "starts-with(concat(", lc, ", '-'), 'en-') and ",
                        "contains(concat('-', ",
                        singleton_cut(paste0("substring-after(concat(", lc,
                                             ", '-'), 'en-')")),
                        "), '-nz-')]]"))

    expect_error(translator$css_to_xpath("html:lang()"), "Expected at least one argument.*")
    expect_error(translator$css_to_xpath("html:lang(1)"), "Expected string, ident, or \\* arguments.*")

    # Multiple languages with OR logic
    expect_equal(translator$css_to_xpath("html:lang(en, fr)"),
                 "descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]]")
})

test_that("HTML translator lowercases attribute names but not values", {
    translator <- HTMLTranslator$new()

    # Attribute names in HTML are case-insensitive, but values are not
    expect_equal(translator$css_to_xpath('[Data-State="Active"]'),
                 "descendant-or-self::*[@data-state = 'Active']")
    expect_equal(translator$css_to_xpath('[data-state~="Active"]'),
                 paste0("descendant-or-self::*[",
                        "contains(concat(' ', ",
                        "normalize-space(@data-state), ' '), ",
                        "' Active ')]"))
    # Element names are still lowercased
    expect_equal(translator$css_to_xpath('DIV[data-state="Active"]'),
                 "descendant-or-self::div[@data-state = 'Active']")
})

test_that("the HTML case-insensitive attributes fold their values", {
    translator <- HTMLTranslator$new()
    lc <- function(expr)
        paste0("translate(", expr, ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
               "'abcdefghijklmnopqrstuvwxyz')")

    # HTML matches the values of the attributes in its
    # "Case-sensitivity of selectors" list ASCII case-insensitively,
    # so both sides of the comparison are folded
    expect_equal(translator$css_to_xpath("input[type=RADIO]"),
                 paste0("descendant-or-self::input[", lc("@type"),
                        " = 'radio']"))
    expect_equal(translator$css_to_xpath("a[REL=Tag]"),
                 paste0("descendant-or-self::a[", lc("@rel"), " = 'tag']"))
    # Every operator folds, and an operator that names the attribute
    # twice folds it twice
    expect_equal(translator$css_to_xpath('[lang|="en"]'),
                 paste0("descendant-or-self::*[", lc("@lang"),
                        " = 'en' or starts-with(", lc("@lang"), ", 'en-')]"))
    expect_equal(translator$css_to_xpath('[media*="Print"]'),
                 paste0("descendant-or-self::*[contains(", lc("@media"),
                        ", 'print')]"))

    # An explicit 's' opts back out; an empty value is left exact, as
    # it is for the 'i' flag
    expect_equal(translator$css_to_xpath("input[type=RADIO s]"),
                 "descendant-or-self::input[@type = 'RADIO']")
    expect_equal(translator$css_to_xpath('input[type=""]'),
                 "descendant-or-self::input[@type = '']")

    # The list is closed, and does not cover a namespaced attribute
    # even when its local name is on it
    expect_equal(translator$css_to_xpath('a[href^="HTTP"]'),
                 "descendant-or-self::a[starts-with(@href, 'HTTP')]")
    expect_equal(translator$css_to_xpath('[class="Foo"]'),
                 "descendant-or-self::*[@class = 'Foo']")
    expect_equal(translator$css_to_xpath('[data-type="Foo"]'),
                 "descendant-or-self::*[@data-type = 'Foo']")
    expect_equal(translator$css_to_xpath('[svg|type="Foo"]'),
                 "descendant-or-self::*[@svg:type = 'Foo']")
    # Under '*|' the rule still holds for the attribute in no
    # namespace, and only for that one
    expect_equal(translator$css_to_xpath('[*|type="Foo"]'),
                 paste0("descendant-or-self::*[@*[local-name() = 'type']",
                        "[namespace-uri() = '' and ", lc("."),
                        " = 'foo' or . = 'Foo']]"))

    # The rule is HTML's: neither the xhtml nor the generic translator
    # serves an HTML document
    expect_equal(HTMLTranslator$new(xhtml = TRUE)$css_to_xpath("input[type=RADIO]"),
                 "descendant-or-self::input[@type = 'RADIO']")
    expect_equal(GenericTranslator$new()$css_to_xpath("input[type=RADIO]"),
                 "descendant-or-self::input[@type = 'RADIO']")
})

test_that("the HTML translator folds names as an HTML parser does", {
    translator <- HTMLTranslator$new()

    # An HTML parser lowercases ASCII only, so a non-ASCII element or
    # attribute name reaches XPath as it was written
    expect_equal(translator$css_to_xpath("\\C4[\\C4=x]"),
                 paste0("descendant-or-self::*[name() = '\u00c4' and ",
                        "namespace-uri() = '' and ",
                        "attribute::*[name() = '\u00c4' and ",
                        "namespace-uri() = ''] = 'x']"))
    # U+041B CYRILLIC CAPITAL LETTER EL
    expect_equal(translator$css_to_xpath("\\41b"),
                 paste0("descendant-or-self::*[name() = '\u041b' and ",
                        "namespace-uri() = '']"))
})

test_that("ASCII folds cope with a name containing a noncharacter", {
    # R refuses to convert a noncharacter (U+FFFE, U+FFFF) to wide
    # characters, so tolower() and chartr() fail outright on any string
    # holding one - which is why every ASCII fold in the package goes
    # through ascii_lower() instead. Each selector below reaches one of
    # those folds, and each used to leave the package with an unclassed
    # "invalid input ... in 'utf8towcs'" rather than a translation or a
    # selectr condition
    translator <- HTMLTranslator$new()
    escapes <- c("\\FFFE", "\\FFFF")
    chars <- intToUtf8(c(0xFFFEL, 0xFFFFL), multiple = TRUE)

    for (i in seq_along(escapes)) {
        esc <- escapes[i]
        ch <- chars[i]
        # An element name, lowercased for an HTML document
        expect_equal(translator$css_to_xpath(esc, prefix = ""),
                     paste0("*[name() = '", ch, "' and namespace-uri() = '']"))
        # An attribute name, lowercased the same way
        expect_equal(translator$css_to_xpath(paste0("[", esc, "=x]"),
                                             prefix = ""),
                     paste0("*[attribute::*[name() = '", ch,
                            "' and namespace-uri() = ''] = 'x']"))
        # A :lang() range: no fold to reach - a noncharacter cannot
        # appear in an RFC 4647 language range - but the range still
        # has to be named in a selectr condition, not crash the
        # message-building
        expect_error(translator$css_to_xpath(paste0(":lang(", esc, ")")),
                     class = "selectr_translation_error")
        # A pseudo-class name, an attribute flag and an An+B argument
        # are all matched ASCII case-insensitively too; none of the
        # three is valid, and each must say so as a selectr condition
        expect_error(translator$css_to_xpath(paste0(":", esc)),
                     class = "selectr_translation_error")
        expect_error(translator$css_to_xpath(paste0("[a=b ", esc, "]")),
                     class = "selectr_parse_error")
        expect_error(translator$css_to_xpath(paste0(":nth-child(", esc, ")")),
                     class = "selectr_parse_error")
    }
})

test_that("Generic translator handles :lang() wildcards and comma lists", {
    translator <- GenericTranslator$new()

    # Simple languages still work
    expect_equal(translator$css_to_xpath("div:lang(en)"), "descendant-or-self::div[lang('en')]")

    # Wildcard * matches any element with a known (non-empty) language
    expect_equal(translator$css_to_xpath('div:lang(*)'),
                 paste0("descendant-or-self::div[ancestor-or-self::*",
                        "[@xml:lang][1][string-length(@xml:lang) > 0]]"))

    # Wildcard suffix like en-* for prefix matching; the trailing "-*" is
    # stripped because XPath's lang() already matches at '-' boundaries
    # (lang('en-') would match nothing)
    expect_equal(translator$css_to_xpath('div:lang(en-*)'), "descendant-or-self::div[lang('en')]")
    expect_equal(translator$css_to_xpath('div:lang(fr-*)'), "descendant-or-self::div[lang('fr')]")

    # Comma-separated lists with OR logic
    expect_equal(translator$css_to_xpath('div:lang(en, fr)'), "descendant-or-self::div[lang('en') or lang('fr')]")
    expect_equal(translator$css_to_xpath('div:lang(en, de, fr)'), "descendant-or-self::div[lang('en') or lang('de') or lang('fr')]")

    # Mixed wildcards and regular languages
    expect_equal(translator$css_to_xpath('div:lang(en-*, fr)'), "descendant-or-self::div[lang('en') or lang('fr')]")
    expect_equal(translator$css_to_xpath('div:lang(*, de)'),
                 paste0("descendant-or-self::div[ancestor-or-self::*",
                        "[@xml:lang][1][string-length(@xml:lang) > 0]",
                        " or lang('de')]"))
})

test_that("HTML translator handles :lang() wildcards and comma lists", {
    translator <- HTMLTranslator$new()

    # Wildcard * matches any element with a known (non-empty) language
    expect_equal(translator$css_to_xpath('div:lang(*)'),
                 paste0("descendant-or-self::div[ancestor-or-self::*",
                        "[@lang][1][string-length(@lang) > 0]]"))

    # Wildcard suffix for prefix matching
    expect_equal(translator$css_to_xpath('div:lang(en-*)'),
                 "descendant-or-self::div[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]")

    # Multiple values with OR logic
    expect_equal(translator$css_to_xpath('div:lang(en, fr)'),
                 "descendant-or-self::div[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')] or ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'fr-')]]")
})

test_that("Generic translator handles :dir() function", {
    translator <- GenericTranslator$new()

    # :dir() uses "never matches" pattern (requires runtime directionality detection)
    expect_equal(translator$css_to_xpath("div:dir(ltr)"), "descendant-or-self::div[0]")
    expect_equal(translator$css_to_xpath("div:dir(rtl)"), "descendant-or-self::div[0]")
    expect_equal(translator$css_to_xpath(":dir(ltr)"), "descendant-or-self::*[0]")
    # values other than ltr/rtl are not invalid, they just never match
    expect_equal(translator$css_to_xpath(":dir(foo)"), "descendant-or-self::*[0]")

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    # :dir() takes exactly one identifier (CSS Selectors Level 4)
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath('div:dir("ltr")'), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr rtl)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr, rtl)"), "Expected an argument.*")
    expect_error(translator$css_to_xpath("div:dir(*)"), "Expected an argument.*")
})

test_that("HTML translator handles :dir() function", {
    translator <- HTMLTranslator$new()

    # :dir() never matches with the HTML translators too - a
    # deliberate decision, not a missing override: resolved
    # directionality (dir=auto, bdi, form controls) is not static,
    # so no :lang()-style attribute-walk approximation is attempted
    expect_equal(translator$css_to_xpath("div:dir(ltr)"), "descendant-or-self::div[0]")
    expect_equal(translator$css_to_xpath("div:dir(rtl)"), "descendant-or-self::div[0]")
    expect_equal(translator$css_to_xpath(":dir(ltr)"), "descendant-or-self::*[0]")

    xhtml_translator <- HTMLTranslator$new(xhtml = TRUE)
    expect_equal(xhtml_translator$css_to_xpath("div:dir(ltr)"),
                 "descendant-or-self::div[0]")
    expect_equal(xhtml_translator$css_to_xpath("div:dir(rtl)"),
                 "descendant-or-self::div[0]")

    expect_error(translator$css_to_xpath("div:dir()"), "Expected at least one argument.*")
    # :dir() takes exactly one identifier (CSS Selectors Level 4)
    expect_error(translator$css_to_xpath("div:dir(1)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath('div:dir("ltr")'), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr rtl)"), "Expected a single ident argument.*")
    expect_error(translator$css_to_xpath("div:dir(ltr, rtl)"), "Expected an argument.*")
    expect_error(translator$css_to_xpath("div:dir(*)"), "Expected an argument.*")
})

test_that(":lang() and :dir() reject a lone '-' argument", {
    # A lone '-' is not a valid <ident> per css-syntax (an ident may
    # start with '-' only when followed by an ident-start code point
    # or a second '-')
    for (translator in list(GenericTranslator$new(), HTMLTranslator$new())) {
        expect_error(translator$css_to_xpath("e:lang(-)"),
                     "Expected string, ident, or \\* arguments.*")
        expect_error(translator$css_to_xpath("e:dir(-)"),
                     "Expected a single ident argument.*")
        expect_error(translator$css_to_xpath("e:lang(en, -)"),
                     "Expected string, ident, or \\* arguments.*")
        # '--x' and 'en--' are valid idents, so they get past the
        # argument check - and are then rejected as language ranges
        # (see the extended-language-range tests below)
        expect_error(translator$css_to_xpath("e:lang(--x)"),
                     "not a well-formed extended language range")
        expect_error(translator$css_to_xpath("e:lang(en--)"),
                     "not a well-formed extended language range")
        expect_error(translator$css_to_xpath("e:lang(en-*)"), NA)
    }
})

test_that(":lang() rejects an ill-formed extended language range", {
    # Selectors 4 section 7.2 requires each range to be an RFC 4647
    #   extended-language-range = (1*8ALPHA / "*") *("-" (1*8alphanum / "*"))
    # so an empty, over-long or non-alphanumeric subtag, and a '*' that
    # is not a whole subtag, are all ill-formed. The spec says such a
    # range matches nothing while leaving the selector valid; XPath has
    # no never-matching form that survives being combined with the rest
    # of the expression, so the range is rejected by name instead of
    # being normalised into a well-formed one - ":lang(en-)" used to
    # translate as ":lang(en)" and select every English element.
    ill_formed <- c("en-", '"en-"', "en--", "-en", "--x", "--", "en*",
                    "de-*--de", '"*-CH-"', "abcdefghi", "x1", '"en-abcdefghi"')
    for (translator in list(GenericTranslator$new(), HTMLTranslator$new(),
                            HTMLTranslator$new(xhtml = TRUE))) {
        for (range in ill_formed) {
            expect_error(translator$css_to_xpath(paste0("e:lang(", range, ")")),
                         "not a well-formed extended language range",
                         class = "selectr_translation_error")
            # and equally when it keeps company with a valid range
            expect_error(
                translator$css_to_xpath(paste0("e:lang(en, ", range, ")")),
                "not a well-formed extended language range")
        }
        # well-formed ranges of every shape stay accepted, including
        # the empty range, which Selectors 4 gives a meaning of its own
        for (range in c('""', "en", "EN", "en-gb", "en-GB-1996", "*",
                        "en-*", '"en-*"', "abcdefgh")) {
            expect_error(translator$css_to_xpath(paste0("e:lang(", range, ")")),
                         NA)
        }
    }
})

test_that(":lang() rejects an empty item of its comma-separated list", {
    # The argument is an <ident>#-style list, in which an empty item is
    # a grammar error - not a range that matches nothing, and not
    # something to drop silently (":lang(en, )" used to translate as
    # ":lang(en)")
    msg <- "Expected a language range for :lang\\(\\), got "
    for (translator in list(GenericTranslator$new(), HTMLTranslator$new(),
                            HTMLTranslator$new(xhtml = TRUE))) {
        # the token reported is the one found where the range should
        # be: the ',' that follows the missing item, or the ')' (or
        # EOF) that closes the list after a trailing comma
        expect_error(translator$css_to_xpath("e:lang(en, )"),
                     paste0(msg, "<DELIM '\\)' at 12>"),
                     class = "selectr_translation_error")
        expect_error(translator$css_to_xpath("e:lang(, en)"),
                     paste0(msg, "<DELIM ',' at 8>"))
        expect_error(translator$css_to_xpath("e:lang(en,, fr)"),
                     paste0(msg, "<DELIM ',' at 11>"))
        expect_error(translator$css_to_xpath("e:lang(en,"),
                     paste0(msg, "<EOF at 11>"))
        # a list of nothing but empty items is no more an argument list
        # than ":lang()" is, and keeps that parse error
        expect_error(translator$css_to_xpath("e:lang(,)"),
                     "Expected at least one argument",
                     class = "selectr_parse_error")
        # an *empty range*, by contrast, is a range like any other
        expect_error(translator$css_to_xpath('e:lang(en, "")'), NA)
    }
})

test_that("HTML translator handles :lang() extended-filtering wildcards", {
    # Per Selectors 4 section 14.1, :lang() ranges are matched with RFC
    # 4647 extended filtering, so a wildcard in non-trailing position
    # (*-CH, de-*-DE) is valid. The HTML translators approximate it from
    # the nearest lang-attributed ancestor.
    translator <- HTMLTranslator$new()
    lc <- "translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"

    # A leading wildcard "*-CH" matches any tag carrying a "ch" subtag
    # after the first: RFC 4647 step 2 pairs the '*' with the tag's
    # primary subtag, so that subtag is consumed rather than searched
    # (substring-after drops it before the containment test, which looks
    # no further than the first singleton). Both the unquoted (tokenized
    # as '*' + "-CH") and quoted spellings reassemble to the same range
    # and translation.
    expected_star_ch <- paste0(
        "descendant-or-self::*[ancestor-or-self::*[@lang][1]",
        "[contains(concat('-', ",
        singleton_cut(paste0("substring-after(concat(", lc, ", '-'), '-')")),
        "), '-ch-')]]")
    expect_equal(translator$css_to_xpath(":lang(*-CH)"), expected_star_ch)
    expect_equal(translator$css_to_xpath('div:lang("*-CH")'),
                 sub("self::\\*", "self::div", expected_star_ch))

    # An interior wildcard "de-*-DE": the tag must start with "de" and
    # carry a later "de" subtag, in that order (substring-after threads
    # the tail so "de-CH" alone does not match).
    expect_equal(translator$css_to_xpath(":lang(de-*-DE)"),
                 paste0("descendant-or-self::*[ancestor-or-self::*[@lang][1]",
                        "[starts-with(concat(", lc, ", '-'), 'de-') and ",
                        "contains(concat('-', ",
                        singleton_cut(paste0("substring-after(concat(", lc,
                                             ", '-'), 'de-')")),
                        "), '-de-')]]"))

    # A non-trailing wildcard in a comma list translates alongside its
    # neighbours without error
    expect_error(translator$css_to_xpath(":lang(en, *-CH)"), NA)

    # An empty subtag makes the range ill-formed, wherever it falls: it
    # is rejected rather than skipped, which would translate
    # ":lang(de-*--de)" as the different range ":lang(de-*-de)"
    expect_error(translator$css_to_xpath(":lang(de-*--de)"),
                 "not a well-formed extended language range")

    # An all-wildcard range constrains no subtag, so it matches any
    # element with a known language, exactly as ":lang(*)" does
    expect_equal(translator$css_to_xpath(':lang("*-*")'),
                 translator$css_to_xpath(":lang(*)"))
})

test_that("HTML :lang() extended wildcards match the right elements", {
    skip_if_not_installed("xml2")
    doc <- xml2::read_xml(paste0(
        "<html>",
        "<a lang='fr-CH'/>",       # ch subtag    -> :lang(*-CH)
        "<b lang='de-CH-1996'/>",  # ch subtag    -> :lang(*-CH)
        "<c lang='en-GB'/>",       # no ch        -> neither
        "<d lang='ch'/>",          # ch is the whole tag -> neither
        "<e lang='de-DE'/>",       # de...de, no ch -> :lang(de-*-DE) only
        "<f lang='de-CH-DE'/>",    # ch subtag and de...de -> both
        "<g lang='de-CH'/>",       # ch subtag, but no later de -> :lang(*-CH) only
        "<h lang='ch-DE'/>",       # ch is the primary subtag -> neither
        "</html>"))
    ids <- function(css) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(css, translator = "html"))
        paste(xml2::xml_name(nodes), collapse = ",")
    }
    # every element carrying a "ch" subtag after the primary one: the
    # leading wildcard is paired with the primary subtag (RFC 4647 step
    # 2), so lang="ch" and lang="ch-DE" are not matched by "*-CH"
    expect_equal(ids(":lang(*-CH)"), "a,b,f,g")
    # "de" first and a later "de" subtag, in order (de-CH alone excluded)
    expect_equal(ids(":lang(de-*-DE)"), "e,f")
    # case-insensitive: the wildcard subtag is matched in lower case
    expect_equal(ids(":lang(*-ch)"), "a,b,f,g")
})

test_that("HTML :lang() applies extended filtering to exact multi-subtag ranges", {
    skip_if_not_installed("xml2")
    # A range with no literal '*' but more than one subtag is still RFC
    # 4647 extended filtering, not a plain prefix test: any subtag may
    # be skipped between the ones named.
    doc <- xml2::read_xml('<a lang="de-Latn-DE">x</a>')
    xp <- css_to_xpath("*:lang(de-DE)", translator = "html")
    expect_equal(xml2::xml_name(xml2::xml_find_all(doc, xp)), "a")

    # A single subtag (with or without a trailing wildcard) is
    # unaffected: still a plain prefix test, so both translate to the
    # same "de-" prefix condition
    expect_equal(
        HTMLTranslator$new()$css_to_xpath("*:lang(de)"),
        HTMLTranslator$new()$css_to_xpath("*:lang(de-*)"))
})

test_that("HTML :lang() extended filtering stops at a singleton subtag", {
    # RFC 4647 step 3.C: a singleton subtag (one letter or digit, which
    # opens an extension or private-use section) reached before a range
    # subtag is found ends the match, so a "DE" after "-x-" is not a
    # region. A singleton after the match, or as the primary subtag,
    # changes nothing.
    tags <- c("de-DE", "de-x-DE", "de-Latn-x-DE", "de-a-foo-DE",
              "de-DE-x-foo", "de-Latn-DE", "de-1-DE", "x-foo", "i-klingon",
              "x-de", "de", "de-x-foo", "de-DE-x-foo-y-bar", "de-y-bar-x-foo",
              "de-x-a-foo")
    html <- paste0("<html><body>",
                   paste0(sprintf('<p id="%s" lang="%s">x</p>', tags, tags),
                          collapse = ""),
                   "</body></html>")
    expected <- list(
        "de-DE" = c("de-DE", "de-DE-x-foo", "de-Latn-DE",
                    "de-DE-x-foo-y-bar"),
        "de-Latn-DE" = "de-Latn-DE",
        "\\*-DE" = c("de-DE", "de-DE-x-foo", "de-Latn-DE", "x-de",
                     "de-DE-x-foo-y-bar"),
        "de-DE, fr-CA" = c("de-DE", "de-DE-x-foo", "de-Latn-DE",
                           "de-DE-x-foo-y-bar"),
        "de-*-DE" = c("de-DE", "de-DE-x-foo", "de-Latn-DE",
                      "de-DE-x-foo-y-bar"),
        # a singleton in the range matches the tag's first singleton
        # after the primary subtag, and the search then resumes up to
        # the next one
        "de-x-foo" = c("de-DE-x-foo", "de-x-foo", "de-DE-x-foo-y-bar"),
        "de-x-*-bar" = character(0),
        "de-y-bar" = "de-y-bar-x-foo",
        "de-y-foo" = character(0),
        # a singleton primary subtag is matched like any other
        "x-foo" = "x-foo",
        "i-klingon" = "i-klingon",
        "\\*-foo" = "x-foo",
        # a single-subtag range is a prefix test, blind to singletons
        "de" = c("de-DE", "de-x-DE", "de-Latn-x-DE", "de-a-foo-DE",
                 "de-DE-x-foo", "de-Latn-DE", "de-1-DE", "de", "de-x-foo",
                 "de-DE-x-foo-y-bar", "de-y-bar-x-foo", "de-x-a-foo"),
        "x" = c("x-foo", "x-de"))

    # queried through both xml2 and XML
    for (range in names(expected)) {
        css <- paste0("p:lang(", range, ")")
        if (requireNamespace("xml2", quietly = TRUE)) {
            doc <- xml2::read_html(html)
            ids <- xml2::xml_attr(querySelectorAll(doc, css), "id")
            expect_equal(ids, expected[[range]], label = css)
        }
        if (requireNamespace("XML", quietly = TRUE)) {
            doc <- XML::htmlParse(html, asText = TRUE)
            ids <- vapply(querySelectorAll(doc, css), XML::xmlGetAttr,
                          character(1), "id")
            expect_equal(ids, expected[[range]], label = css)
        }
    }
})

test_that("XHTML :lang() extended filtering stops at a singleton subtag", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")
    doc_text <- paste0(
        "<html xmlns='http://www.w3.org/1999/xhtml'>",
        "<p id='a' xml:lang='de-DE'/>",
        "<p id='b' xml:lang='de-x-DE'/>",
        "<p id='c' lang='de-Latn-x-DE'/>",
        "<p id='d' xml:lang='de-DE-x-foo' lang='de-x-DE'/>",
        "<p id='e' xml:lang='de-x-DE' lang='de-DE'/>",
        "</html>")
    ns <- c(h = "http://www.w3.org/1999/xhtml")
    xp <- css_to_xpath("h|p:lang(de-DE)", prefix = "//",
                       translator = "xhtml")
    ids <- xml2::xml_attr(
        xml2::xml_find_all(xml2::read_xml(doc_text), xp, ns), "id")
    expect_equal(ids, c("a", "d"))
    ids <- unlist(XML::xpathApply(XML::xmlParse(doc_text, asText = TRUE),
                                  xp, XML::xmlGetAttr, "id",
                                  namespaces = ns))
    expect_equal(ids, c("a", "d"))
})

test_that("HTML :lang() singleton cut is case-insensitive", {
    translator <- HTMLTranslator$new()
    # the tag is lowercased before the singleton search, so an
    # uppercase "X" ends it too, and the range is lowercased alike
    expect_equal(translator$css_to_xpath("p:lang(DE-DE)"),
                 translator$css_to_xpath("p:lang(de-de)"))
    skip_if_not_installed("xml2")
    doc <- xml2::read_html(paste0('<p id="a" lang="DE-X-DE">x</p>',
                                  '<p id="b" lang="DE-LATN-DE">y</p>'))
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "p:lang(de-DE)"),
                                "id"),
                 "b")
})

test_that("generic :lang() translation sits alongside the HTML ones", {
    generic <- GenericTranslator$new()
    html <- HTMLTranslator$new()
    xhtml <- HTMLTranslator$new(xhtml = TRUE)
    xhtml_lang <- paste0("concat(@xml:lang, substring(@lang, 1, ",
                         "string-length(@lang) * not(@xml:lang)))")
    as_generic <- function(xp) gsub("@lang", "@xml:lang", xp, fixed = TRUE)
    as_xhtml <- function(xp) {
        xp <- gsub("[@lang][1]", "[@xml:lang or @lang][1]", xp, fixed = TRUE)
        gsub("translate(@lang,", paste0("translate(", xhtml_lang, ","), xp,
             fixed = TRUE)
    }

    # A single-subtag range stays XPath's lang() in the generic
    # translator, alone or in a list
    expect_equal(generic$css_to_xpath("p:lang(en)"),
                 "descendant-or-self::p[lang('en')]")
    expect_equal(generic$css_to_xpath("p:lang(en, fr)"),
                 "descendant-or-self::p[lang('en') or lang('fr')]")

    # A range naming more than one subtag, a singleton, or a non-trailing
    # wildcard is matched as the HTML translators match it, with the
    # nearest xml:lang standing in for lang
    for (range in c("en-US", "EN-us", "de-x-foo", "x-foo", "*-CH",
                    "de-*-DE", "en-US-*")) {
        css <- paste0("p:lang(", range, ")")
        expect_equal(generic$css_to_xpath(css),
                     as_generic(html$css_to_xpath(css)), label = css)
        expect_equal(xhtml$css_to_xpath(css),
                     as_xhtml(html$css_to_xpath(css)), label = css)
    }

    # In a list, each range takes its own form
    expect_equal(generic$css_to_xpath("p:lang(en, fr-CA)"),
                 paste0("descendant-or-self::p[lang('en') or ",
                        sub("^descendant-or-self::p\\[(.*)\\]$", "\\1",
                            generic$css_to_xpath("p:lang(fr-CA)")),
                        "]"))
    for (css in c("p:lang(en)", "p:lang(en-US)", "p:lang(en, fr-CA)"))
        expect_equal(xhtml$css_to_xpath(css),
                     as_xhtml(html$css_to_xpath(css)), label = css)
})

test_that("generic :lang() applies extended filtering to xml:lang", {
    doc_text <- paste0(
        "<r>",
        "<p id='latn' xml:lang='en-Latn-US'/>",
        "<p id='us' xml:lang='en-US'/>",
        "<p id='upper' xml:lang='EN-LATN-us'/>",
        "<p id='gb' xml:lang='en-GB'/>",
        "<p id='en' xml:lang='en'/>",
        "<p id='us-x' xml:lang='en-x-US'/>",
        "<p id='us-priv' xml:lang='en-US-x-foo'/>",
        "<div xml:lang='en-Latn-US'>",
        "<p id='inherited'/>",
        "<p id='reset' xml:lang=''/>",
        "<div xml:lang=''><p id='reset-inherited'/></div>",
        "<p id='nearer' xml:lang='en-GB'/>",
        "</div>",
        "<p id='de' xml:lang='de-DE'/>",
        "<p id='de-x' xml:lang='de-x-DE'/>",
        "<p id='de-latn' xml:lang='de-Latn-DE'/>",
        "</r>")
    expected <- list(
        # subtags may be skipped, case aside, but not past a singleton
        "en-US" = c("latn", "us", "upper", "us-priv", "inherited"),
        "EN-us" = c("latn", "us", "upper", "us-priv", "inherited"),
        "de-DE" = c("de", "de-latn"),
        "en-US, de-DE" = c("latn", "us", "upper", "us-priv", "inherited",
                           "de", "de-latn"),
        "en, de-DE" = c("latn", "us", "upper", "gb", "en", "us-x",
                        "us-priv", "inherited", "nearer", "de", "de-latn"),
        "*-US" = c("latn", "us", "upper", "us-priv", "inherited"),
        "en-*-US" = c("latn", "us", "upper", "us-priv", "inherited"),
        "en-x-US" = "us-x")
    for (range in names(expected)) {
        css <- paste0("p:lang(", range, ")")
        if (requireNamespace("xml2", quietly = TRUE)) {
            doc <- xml2::read_xml(doc_text)
            ids <- xml2::xml_attr(querySelectorAll(doc, css), "id")
            expect_equal(ids, expected[[range]], label = css)
        }
        if (requireNamespace("XML", quietly = TRUE)) {
            doc <- XML::xmlParse(doc_text, asText = TRUE)
            ids <- vapply(querySelectorAll(doc, css), XML::xmlGetAttr,
                          character(1), name = "id")
            expect_equal(ids, expected[[range]], label = css)
        }
    }
})

test_that("generic and HTML :lang() agree on singleton subtags", {
    skip_if_not_installed("xml2")
    tags <- c("de-DE", "de-x-DE", "de-Latn-x-DE", "de-a-foo-DE",
              "de-DE-x-foo", "de-Latn-DE", "de-1-DE", "x-foo", "i-klingon",
              "x-de", "de", "de-x-foo", "de-DE-x-foo-y-bar", "de-y-bar-x-foo",
              "de-x-a-foo", "DE-X-de", "De-Latn-dE")
    html_doc <- xml2::read_html(paste0(
        "<html><body>",
        paste0(sprintf('<p id="%s" lang="%s">x</p>', tags, tags),
               collapse = ""),
        "</body></html>"))
    xml_doc <- xml2::read_xml(paste0(
        "<r>",
        paste0(sprintf('<p id="%s" xml:lang="%s"/>', tags, tags),
               collapse = ""),
        "</r>"))
    ids <- function(doc, css, translator)
        xml2::xml_attr(xml2::xml_find_all(
            doc, css_to_xpath(css, translator = translator)), "id")
    expect_equal(ids(xml_doc, "p:lang(de-DE)", "generic"),
                 c("de-DE", "de-DE-x-foo", "de-Latn-DE",
                   "de-DE-x-foo-y-bar", "De-Latn-dE"))
    for (range in c("de-DE", "de-Latn-DE", "\\*-DE", "de-*-DE", "de-x-foo",
                    "de-x-*-bar", "de-y-bar", "x-foo", "i-klingon",
                    "\\*-foo", "de", "x", "de-*", "de-DE-*", "DE-x-DE")) {
        css <- paste0("p:lang(", range, ")")
        expect_equal(ids(xml_doc, css, "generic"),
                     ids(html_doc, css, "html"), label = css)
    }
})

test_that(':lang("") matches elements with no tagged language', {
    skip_if_not_installed("xml2")
    # The document element itself carries no lang/xml:lang either, so
    # it counts as "not tagged" too, along with <a> (explicitly reset)
    # and <b> (never had one); <c> and its child <c1> both inherit a
    # real language and must not match.
    doc <- xml2::read_xml(paste0(
        '<r>',
        '<a lang="">untagged-by-reset</a>',
        '<b>never-tagged</b>',
        '<c lang="en"><c1/></c>',            # inherits a real language
        '</r>'))
    xp <- css_to_xpath('*:lang("")', translator = "html")
    expect_equal(xml2::xml_name(xml2::xml_find_all(doc, xp)), c("r", "a", "b"))

    generic <- xml2::read_xml(paste0(
        '<r>',
        '<a xml:lang="">untagged-by-reset</a>',
        '<b>never-tagged</b>',
        '<c xml:lang="en"><c1/></c>',
        '</r>'))
    xp2 <- css_to_xpath('*:lang("")')
    expect_equal(xml2::xml_name(xml2::xml_find_all(generic, xp2)), c("r", "a", "b"))
})

test_that("generic translator accepts :lang() non-trailing wildcards", {
    # With extended filtering done on the nearest xml:lang, a leading or
    # interior wildcard translates as it does for the HTML translators
    translator <- GenericTranslator$new()
    xlc <- "translate(@xml:lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"
    expected_star_ch <- paste0(
        "descendant-or-self::*[ancestor-or-self::*[@xml:lang][1]",
        "[contains(concat('-', ",
        singleton_cut(paste0("substring-after(concat(", xlc, ", '-'), '-')")),
        "), '-ch-')]]")
    expect_equal(translator$css_to_xpath(":lang(*-CH)"), expected_star_ch)
    expect_equal(translator$css_to_xpath(':lang("*-CH")'), expected_star_ch)
    expect_error(translator$css_to_xpath(":lang(de-*-DE)"), NA)
    expect_error(translator$css_to_xpath('div:lang("de-*-DE")'), NA)
    expect_error(translator$css_to_xpath(":lang(en, *-CH)"), NA)

    # Bare and trailing wildcards are unchanged
    expect_error(translator$css_to_xpath(":lang(*)"), NA)
    expect_equal(translator$css_to_xpath(":lang(en-*)"),
                 "descendant-or-self::*[lang('en')]")
    expect_equal(translator$css_to_xpath('div:lang("en-*")'),
                 "descendant-or-self::div[lang('en')]")
    expect_error(translator$css_to_xpath(":lang(en-*, fr)"), NA)
    expect_error(translator$css_to_xpath(":lang(*, de)"), NA)
})

test_that(":lang(*) only matches elements with a known language", {
    skip_if_not_installed("xml2")
    # The bare wildcard means "the language is known", not "always
    # true": an element with no language in its ancestry, or one whose
    # nearest declaration resets the language to unknown with an empty
    # value, must not match.
    generic <- xml2::read_xml(paste0(
        "<r>",
        "<a/>",                                    # no language at all
        "<b xml:lang='en'><b1/></b>",              # declared, inherited
        "<c xml:lang=''/>",                        # reset to unknown
        "<d xml:lang='en'><d1 xml:lang=''/></d>",  # d1's nearest resets
        "</r>"))
    expect_equal(
        xml2::xml_name(xml2::xml_find_all(generic, css_to_xpath(":lang(*)"))),
        c("b", "b1", "d"))

    html <- xml2::read_xml(paste0(
        "<html>",
        "<a/>",
        "<b lang='en'><b1/></b>",
        "<c lang=''/>",
        "<d lang='en'><d1 lang=''/></d>",          # d1's nearest resets
        "</html>"))
    expect_equal(
        xml2::xml_name(xml2::xml_find_all(
            html, css_to_xpath(":lang(*)", translator = "html"))),
        c("b", "b1", "d"))
})

test_that("HTMLTranslator rejects unknown construction arguments", {
    expect_error(HTMLTranslator$new(strict = TRUE), "unused argument")
    # (xhtm = TRUE would still construct via R's standard partial
    # argument matching of xhtml)
    expect_equal(HTMLTranslator$new(xhtm = TRUE)$xhtml, TRUE)
})

test_that("a translator subclass can add new pseudo-class handlers", {
    # Dispatch is dynamic, so a handler defined only on a subclass is
    # found without editing the base class
    BlinkTranslator <- translator_class("BlinkTranslator",
        inherit = GenericTranslator,
        public = list(
            xpath_blink_pseudo = function(xpath) {
                xpath$add_condition("@blink")
                xpath
            },
            xpath_nth_word_function = function(xpath, fn) {
                xpath$add_condition("@nth-word")
                xpath
            }))

    translator <- BlinkTranslator$new()
    expect_equal(translator$css_to_xpath("a:blink"),
                 "descendant-or-self::a[@blink]")
    expect_equal(translator$css_to_xpath("a:nth-word(2)"),
                 "descendant-or-self::a[@nth-word]")
    # Unknown names still produce the usual errors
    expect_error(translator$css_to_xpath("a:frobnicate"),
                 "The pseudo-class :frobnicate is unknown")
    expect_error(translator$css_to_xpath("a:frobnicate(2)"),
                 "The pseudo-class :frobnicate\\(\\) is unknown")
})

test_that("a translator subclass can override id_attribute", {
    XMLIdTranslator <- translator_class("XMLIdTranslator",
        inherit = GenericTranslator,
        public = list(id_attribute = "xml:id"))

    expect_equal(XMLIdTranslator$new()$css_to_xpath("#foo"),
                 "descendant-or-self::*[@xml:id = 'foo']")
    # The default is unchanged
    expect_equal(GenericTranslator$new()$css_to_xpath("#foo"),
                 "descendant-or-self::*[@id = 'foo']")
})

test_that("unimplemented methods throw errors", {
    translator <- GenericTranslator$new()

    expect_error(translator$css_to_xpath("*:nth-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:nth-last-of-type(2n)"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:first-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:last-of-type"), ".* is not implemented")
    expect_error(translator$css_to_xpath("*:only-of-type"), ".* is not implemented")

    # A namespaced wildcard is the universal selector too: counting
    # 'svg|*' siblings would group them by namespace rather than by
    # expanded name, so it errors instead of mistranslating
    expect_error(translator$css_to_xpath("svg|*:nth-of-type(2)"),
                 ".* is not implemented")
    expect_error(translator$css_to_xpath("svg|*:nth-last-of-type(2)"),
                 ".* is not implemented")
    expect_error(translator$css_to_xpath("svg|*:first-of-type"),
                 ".* is not implemented")
    expect_error(translator$css_to_xpath("svg|*:last-of-type"),
                 ".* is not implemented")
    expect_error(translator$css_to_xpath("svg|*:only-of-type"),
                 ".* is not implemented")
    # ... including where a combinator has folded the name test into a
    # predicate
    expect_error(translator$css_to_xpath("a + svg|*:first-of-type"),
                 ".* is not implemented")

    # A namespaced *name* is still counted by its own node test
    expect_equal(translator$css_to_xpath("svg|g:first-of-type"),
                 paste("descendant-or-self::svg:g",
                       "[not(preceding-sibling::svg:g[1])]",
                       sep = ""))
})

test_that("the of-type pseudo-classes refuse an element in any namespace", {
    # '*|e' names a local name but not a namespace, and siblings are of
    # the same type only when their expanded names match: no node test
    # counts them, so every of-type pseudo-class refuses it as it does
    # the universal selector
    of_type <- c("first-of-type", "last-of-type", "only-of-type",
                 "nth-of-type(2)", "nth-last-of-type(2)")
    labels <- c("first-of-type", "last-of-type", "only-of-type",
                "nth-of-type\\(\\)", "nth-last-of-type\\(\\)")
    translators <- list(GenericTranslator$new(), HTMLTranslator$new(),
                        HTMLTranslator$new(xhtml = TRUE))
    for (translator in translators) {
        for (i in seq_along(of_type)) {
            expect_error(translator$css_to_xpath(paste0("*|e:", of_type[i])),
                         paste0("^\\*:", labels[i], " is not implemented"),
                         class = "selectr_translation_error")
            expect_error(
                translator$css_to_xpath(paste0("a + *|e:", of_type[i])),
                paste0("^\\*:", labels[i], " is not implemented"),
                class = "selectr_translation_error")
        }
        # A named element, in no namespace or a given one, still counts
        # its siblings by its own node test
        expect_equal(translator$css_to_xpath("e:first-of-type"),
                     "descendant-or-self::e[not(preceding-sibling::e[1])]")
        expect_equal(translator$css_to_xpath("ns|e:first-of-type"),
                     paste0("descendant-or-self::ns:e",
                            "[not(preceding-sibling::ns:e[1])]"))
    }
})

test_that("'*|e' with an of-type pseudo-class is refused, not mismatched", {
    skip_if_not_installed("xml2")
    # 'a:e' and the 'e' in no namespace are each the first of their
    # own type; counting by local name alone would select only one
    doc <- xml2::read_xml('<root xmlns:a="A"><a:e id="1"/><e id="2"/></root>')
    expect_error(querySelectorAll(doc, "*|e:first-of-type"),
                 class = "selectr_translation_error")
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "e:first-of-type"),
                                "id"),
                 "2")
    expect_equal(xml2::xml_attr(
        querySelectorAll(doc, "a|e:first-of-type", ns = c(a = "A")), "id"),
        "1")
})
