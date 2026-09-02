test_that("XPathExpr objects print correctly", {
    shw <- function(x) trimws(capture.output(x$show()))

    xp <- XPathExpr$new()
    expect_equal(xp$repr(), "XPathExpr[*]")
    expect_equal(shw(xp), "XPathExpr[*]")

    xp <- XPathExpr$new("//")
    expect_equal(xp$repr(), "XPathExpr[//*]")
    expect_equal(shw(xp), "XPathExpr[//*]")

    xp <- XPathExpr$new(element = "a")
    expect_equal(xp$repr(), "XPathExpr[a]")
    expect_equal(shw(xp), "XPathExpr[a]")

    xp <- XPathExpr$new("//a/", "b")
    expect_equal(xp$repr(), "XPathExpr[//a/b]")
    expect_equal(shw(xp), "XPathExpr[//a/b]")
})

test_that("Generic translator validates language arguments", {
    translator <- GenericTranslator$new()
    expect_equal(translator$css_to_xpath("xml:lang(en)"), "descendant-or-self::xml[lang('en')]")
    expect_equal(translator$css_to_xpath("xml:lang(en-nz)"), "descendant-or-self::xml[lang('en-nz')]")

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
    expect_equal(translator$css_to_xpath("html:lang(en)"), "descendant-or-self::html[ancestor-or-self::*[@lang][1][starts-with(concat(translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), 'en-')]]")
    # "en-nz" names two subtags, so RFC 4647 extended filtering applies
    # (see the "extended-filtering exact ranges" test below) rather than
    # the single-subtag prefix test used for "en" above
    expect_equal(translator$css_to_xpath("html:lang(en-nz)"),
                 paste0("descendant-or-self::html[ancestor-or-self::*[@lang][1][",
                        "starts-with(concat('-', translate(@lang, ",
                        "'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), '-en-') and ",
                        "contains(substring-after(concat('-', translate(@lang, ",
                        "'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), '-'), '-en'), '-nz-')]]"))

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
        # valid idents starting or ending with '-' keep working
        expect_error(translator$css_to_xpath("e:lang(--x)"), NA)
        expect_error(translator$css_to_xpath("e:lang(en--)"), NA)
        expect_error(translator$css_to_xpath("e:lang(en-*)"), NA)
    }
})

test_that("HTML translator handles :lang() extended-filtering wildcards", {
    # Per Selectors 4 section 14.1, :lang() ranges are matched with RFC
    # 4647 extended filtering, so a wildcard in non-trailing position
    # (*-CH, de-*-DE) is valid. The HTML translators approximate it from
    # the nearest lang-attributed ancestor.
    translator <- HTMLTranslator$new()
    lc <- "translate(@lang, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"

    # A leading wildcard "*-CH" matches any tag carrying a "ch" subtag.
    # Both the unquoted (tokenized as '*' + "-CH") and quoted spellings
    # reassemble to the same range and translation.
    expected_star_ch <- sprintf(
        "descendant-or-self::*[ancestor-or-self::*[@lang][1][contains(concat('-', %s, '-'), '-ch-')]]",
        lc)
    expect_equal(translator$css_to_xpath(":lang(*-CH)"), expected_star_ch)
    expect_equal(translator$css_to_xpath('div:lang("*-CH")'),
                 sub("self::\\*", "self::div", expected_star_ch))

    # An interior wildcard "de-*-DE": the tag must start with "de" and
    # carry a later "de" subtag, in that order (substring-after threads
    # the tail so "de-CH" alone does not match).
    expect_equal(translator$css_to_xpath(":lang(de-*-DE)"),
                 sprintf(paste0("descendant-or-self::*[ancestor-or-self::*[@lang][1]",
                                "[starts-with(concat('-', %1$s, '-'), '-de-') and ",
                                "contains(substring-after(concat('-', %1$s, '-'), '-de'), '-de-')]]"),
                         lc))

    # A non-trailing wildcard in a comma list translates alongside its
    # neighbours without error
    expect_error(translator$css_to_xpath(":lang(en, *-CH)"), NA)

    # An empty subtag from an embedded double dash is simply skipped,
    # translating the same as if it were never there (R's strsplit()
    # already drops a *trailing* empty subtag, e.g. ":lang(*-)", so
    # this only matters for one embedded between two others)
    expect_equal(translator$css_to_xpath(":lang(de-*--de)"),
                 translator$css_to_xpath(":lang(de-*-de)"))
})

test_that("HTML :lang() extended wildcards match the right elements", {
    skip_if_not_installed("xml2")
    library(xml2)
    doc <- read_xml(paste0(
        "<html>",
        "<a lang='fr-CH'/>",       # ch subtag    -> :lang(*-CH)
        "<b lang='de-CH-1996'/>",  # ch subtag    -> :lang(*-CH)
        "<c lang='en-GB'/>",       # no ch        -> neither
        "<d lang='ch'/>",          # ch is the whole tag -> :lang(*-CH)
        "<e lang='de-DE'/>",       # de...de, no ch -> :lang(de-*-DE) only
        "<f lang='de-CH-DE'/>",    # ch subtag and de...de -> both
        "<g lang='de-CH'/>",       # ch subtag, but no later de -> :lang(*-CH) only
        "</html>"))
    ids <- function(css) {
        nodes <- xml_find_all(doc, css_to_xpath(css, translator = "html"))
        paste(xml_name(nodes), collapse = ",")
    }
    # every element carrying a "ch" subtag, in any position
    expect_equal(ids(":lang(*-CH)"), "a,b,d,f,g")
    # "de" first and a later "de" subtag, in order (de-CH alone excluded)
    expect_equal(ids(":lang(de-*-DE)"), "e,f")
    # case-insensitive: the wildcard subtag is matched in lower case
    expect_equal(ids(":lang(*-ch)"), "a,b,d,f,g")
})

test_that("HTML :lang() applies extended filtering to exact multi-subtag ranges", {
    skip_if_not_installed("xml2")
    library(xml2)
    # A range with no literal '*' but more than one subtag is still RFC
    # 4647 extended filtering, not a plain prefix test: any subtag may
    # be skipped between the ones named.
    doc <- read_xml('<a lang="de-Latn-DE">x</a>')
    xp <- css_to_xpath("*:lang(de-DE)", translator = "html")
    expect_equal(xml_name(xml_find_all(doc, xp)), "a")

    # A single subtag (with or without a trailing wildcard) is
    # unaffected: still a plain prefix test, so both translate to the
    # same "de-" prefix condition
    expect_equal(
        HTMLTranslator$new()$css_to_xpath("*:lang(de)"),
        HTMLTranslator$new()$css_to_xpath("*:lang(de-*)"))
})

test_that("generic translator's :lang() stays Selectors 3 prefix matching", {
    skip_if_not_installed("xml2")
    library(xml2)
    # Unlike the html/xhtml translators, the generic translator has no
    # lang-attribute to walk by hand, so a multi-subtag exact range
    # still does a plain |=-style prefix match: it does not skip
    # subtags the way RFC 4647 extended filtering requires.
    doc <- read_xml('<a xml:lang="de-Latn-DE">x</a>')
    xp <- css_to_xpath("*:lang(de-DE)")
    expect_length(xml_find_all(doc, xp), 0)
})

test_that(':lang("") matches elements with no tagged language', {
    skip_if_not_installed("xml2")
    library(xml2)
    # The document element itself carries no lang/xml:lang either, so
    # it counts as "not tagged" too, along with <a> (explicitly reset)
    # and <b> (never had one); <c> and its child <c1> both inherit a
    # real language and must not match.
    doc <- read_xml(paste0(
        '<r>',
        '<a lang="">untagged-by-reset</a>',
        '<b>never-tagged</b>',
        '<c lang="en"><c1/></c>',            # inherits a real language
        '</r>'))
    xp <- css_to_xpath('*:lang("")', translator = "html")
    expect_equal(xml_name(xml_find_all(doc, xp)), c("r", "a", "b"))

    generic <- read_xml(paste0(
        '<r>',
        '<a xml:lang="">untagged-by-reset</a>',
        '<b>never-tagged</b>',
        '<c xml:lang="en"><c1/></c>',
        '</r>'))
    xp2 <- css_to_xpath('*:lang("")')
    expect_equal(xml_name(xml_find_all(generic, xp2)), c("r", "a", "b"))
})

test_that("generic translator rejects :lang() non-trailing wildcards", {
    # XPath 1.0's lang() cannot express extended filtering, and the
    # generic translator has no lang attribute to walk, so an interior or
    # leading wildcard is rejected (quoted or not) rather than mismatched.
    translator <- GenericTranslator$new()
    msg <- "non-trailing position"
    expect_error(translator$css_to_xpath(":lang(*-CH)"), msg)
    expect_error(translator$css_to_xpath(':lang("*-CH")'), msg)
    expect_error(translator$css_to_xpath(":lang(de-*-DE)"), msg)
    expect_error(translator$css_to_xpath('div:lang("de-*-DE")'), msg)
    expect_error(translator$css_to_xpath(":lang(en, *-CH)"), msg)

    # Bare and trailing wildcards remain valid in the generic translator
    expect_error(translator$css_to_xpath(":lang(*)"), NA)
    expect_error(translator$css_to_xpath(":lang(en-*)"), NA)
    expect_error(translator$css_to_xpath('div:lang("en-*")'), NA)
    expect_error(translator$css_to_xpath(":lang(en-*, fr)"), NA)
    expect_error(translator$css_to_xpath(":lang(*, de)"), NA)
})

test_that(":lang(*) only matches elements with a known language", {
    skip_if_not_installed("xml2")
    library(xml2)
    # The bare wildcard means "the language is known", not "always
    # true": an element with no language in its ancestry, or one whose
    # nearest declaration resets the language to unknown with an empty
    # value, must not match.
    generic <- read_xml(paste0(
        "<r>",
        "<a/>",                                    # no language at all
        "<b xml:lang='en'><b1/></b>",              # declared, inherited
        "<c xml:lang=''/>",                        # reset to unknown
        "<d xml:lang='en'><d1 xml:lang=''/></d>",  # d1's nearest resets
        "</r>"))
    expect_equal(
        xml_name(xml_find_all(generic, css_to_xpath(":lang(*)"))),
        c("b", "b1", "d"))

    html <- read_xml(paste0(
        "<html>",
        "<a/>",
        "<b lang='en'><b1/></b>",
        "<c lang=''/>",
        "<d lang='en'><d1 lang=''/></d>",          # d1's nearest resets
        "</html>"))
    expect_equal(
        xml_name(xml_find_all(
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
    BlinkTranslator <- R6::R6Class("BlinkTranslator",
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
    XMLIdTranslator <- R6::R6Class("XMLIdTranslator",
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
                       "[count(preceding-sibling::svg:g) = 0]",
                       sep = ""))
})
