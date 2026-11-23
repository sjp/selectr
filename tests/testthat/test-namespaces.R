test_that("namespace selectors translate faithfully", {
    gt <- GenericTranslator$new()
    xpath <- function(css) {
        gt$css_to_xpath(css, prefix = "")
    }

    # '*|e' matches 'e' in any namespace, including none
    expect_equal(xpath("*|e"), "*[local-name() = 'e']")
    # '|e' matches 'e' in no namespace, which is what an unprefixed
    # XPath name test already means
    expect_equal(xpath("|e"), "e")
    # '|e' with a name unusable as an XPath name test must still pin the
    # null namespace: a bare name() test is also unprefixed for an
    # element in a default namespace.  'e' means the same thing, so it
    # gets the same pin
    expect_equal(xpath("|é"),
                 "*[name() = 'é' and namespace-uri() = '']")
    expect_equal(xpath("é"),
                 "*[name() = 'é' and namespace-uri() = '']")
    # A prefixed name unusable as an XPath name test keeps the prefix in
    # the node test, where the evaluator's namespace map still resolves
    # it, and compares the local part alone
    expect_equal(xpath("ns|é"), "ns:*[local-name() = 'é']")
    # The node test left on the path step is the namespaced wildcard,
    # but the compound still names a type, so the of-type pseudo-classes
    # count siblings by prefix and local name rather than refusing it as
    # they do for 'ns|*' itself
    expect_equal(xpath("ns|é:first-of-type"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "not(preceding-sibling::",
                        "ns:*[local-name() = 'é'][1])]"))
    expect_equal(xpath("ns|é:nth-of-type(2)"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "preceding-sibling::ns:*[local-name() = 'é'][1] and ",
                        "not(preceding-sibling::ns:*[local-name() = 'é'][2])]"))
    expect_equal(xpath("svg|di\\[v:last-of-type"),
                 paste0("svg:*[local-name() = 'di[v' and ",
                        "not(following-sibling::",
                        "svg:*[local-name() = 'di[v'][1])]"))
    expect_equal(xpath("ns|é:only-of-type"),
                 paste0("ns:*[local-name() = 'é' and ",
                        "not(preceding-sibling::",
                        "ns:*[local-name() = 'é'][1]) and ",
                        "not(following-sibling::",
                        "ns:*[local-name() = 'é'][1])]"))
    # 'ns|*' names no type and is still refused
    expect_error(xpath("ns|*:first-of-type"),
                 "\\*:first-of-type is not implemented")
    # '*|*' is equivalent to '*'
    expect_equal(xpath("*|*"), "*")
    # '|*' matches any element in no namespace
    expect_equal(xpath("|*"), "*[namespace-uri() = '']")
    # 'ns|e' defers prefix-to-URI binding to evaluation time
    expect_equal(xpath("ns|e"), "ns:e")

    # Attribute selectors
    expect_equal(xpath("[*|a]"), "*[@*[local-name() = 'a']]")
    expect_equal(xpath("[*|a='v']"),
                 "*[@*[local-name() = 'a'] = 'v']")
    # Unprefixed attribute names have no namespace, so '[|a]' is
    # equivalent to '[a]'
    expect_equal(xpath("[|a]"), "*[@a]")
    expect_equal(xpath("[|a='v']"), "*[@a = 'v']")
    expect_equal(xpath("[ns|a]"), "*[@ns:a]")
    # An attribute name unusable as an XPath name test is compared with
    # name(), which returns the qualified name, so it is pinned to no
    # namespace as an element name is
    expect_equal(xpath("[\\31 href]"),
                 "*[attribute::*[name() = '1href' and namespace-uri() = '']]")
    # An unsafe local name keeps the prefix in the node test, as on the
    # element path
    expect_equal(xpath("[ns|\\31 href]"),
                 "*[@ns:*[local-name() = '1href']]")
    expect_equal(xpath("[ns|\\31 href='v']"),
                 "*[@ns:*[local-name() = '1href'] = 'v']")

    # Composability
    expect_equal(xpath(":not(*|e)"), "*[not(local-name() = 'e')]")
    expect_equal(xpath("div > *|e"), "div/*[local-name() = 'e']")

    # On the right of '+' the name becomes a node test on the self
    # axis, which like the bare name test in a path step matches an
    # unprefixed name in the null namespace only; '|e' and 'e'
    # coincide there too
    expect_equal(xpath("e + f"),
                 "e/following-sibling::*[1][self::f]")
    expect_equal(xpath("e + |f"),
                 "e/following-sibling::*[1][self::f]")
    expect_equal(xpath("e + ns|f"),
                 "e/following-sibling::*[1][self::ns:f]")
    expect_equal(xpath("e + *|f"),
                 "e/following-sibling::*[1][local-name() = 'f']")

    # Inside pseudo-class arguments, prefixed names keep resolving
    # through the namespace map (a name test on the self axis or the
    # path step itself), rather than comparing against the document's
    # literal prefix with name()
    expect_equal(xpath(":is(ns|e)"), "*[self::ns:e]")
    expect_equal(xpath(":not(ns|e)"), "*[not(self::ns:e)]")
    expect_equal(xpath(":has(ns|e)"), "*[.//ns:e]")
    expect_equal(xpath(":has(> ns|e)"), "*[child::ns:e]")
    # Under '+' the position predicate [1] must precede the name test
    # ("the next sibling, if it is an ns:e"), so the name cannot stay
    # on the path step
    expect_equal(xpath(":has(+ ns|e)"),
                 "*[following-sibling::*[1][self::ns:e]]")

    # 'ns|*' is a node test too ('*' is a valid local part), not an
    # unsafe name: stringifying it as name() = 'ns:*' could never
    # match, as name() never returns a literal '*'
    expect_equal(xpath(":is(ns|*)"), "*[self::ns:*]")
    expect_equal(xpath(":not(ns|*)"), "*[not(self::ns:*)]")
    expect_equal(xpath(":has(ns|*)"), "*[.//ns:*]")
})

test_that("namespace selector specificity is correct", {
    spec <- function(css) specificity(parse(css)[[1]])

    # Universal selectors and namespace components contribute nothing
    expect_equal(spec("*|e"), c(0, 0, 1))
    expect_equal(spec("|e"), c(0, 0, 1))
    expect_equal(spec("*|*"), c(0, 0, 0))
    expect_equal(spec("|*"), c(0, 0, 0))
})

test_that("a prefix that is not an XPath name is rejected", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x, prefix = "")

    # A prefix is written into the node test as it stands, so it has to
    # be a name XPath can parse. Unlike a local name, it has no
    # fallback: comparing 'prefix:name' against name() would test how
    # the document spells the prefix, where every prefix the translator
    # emits is resolved by what the caller bound it to
    expect_error(css("\\31 ns|div"),
                 "The namespace prefix '1ns' is not an XPath name")
    expect_error(css("[\\31 ns|href]"),
                 "The namespace prefix '1ns' is not an XPath name")
    expect_error(css("ns\\:x|div"),
                 "The namespace prefix 'ns:x' is not an XPath name")
    expect_error(css("ns\\|a|b"),
                 "The namespace prefix 'ns\\|a' is not an XPath name")
    expect_error(css("\\D800|a"), "is not an XPath name")

    # The condition is an XML NCName, which is not restricted to ASCII:
    # a prefix XPath can name is passed through whatever its script
    expect_equal(css("äöü|a"), "äöü:a")
    expect_equal(css("[äöü|a]"), "*[@äöü:a]")
    # ... but only as XML 1.0 spelled it, which is what libxml2 (behind
    # both the XML and the xml2 package) parses: the Fifth Edition
    # widened the tables, and a prefix accepted here has to parse
    # everywhere
    expect_error(css("nsɂ|a"), "is not an XPath name")

    # A translation error, so it carries the usual fields. There is no
    # CSS that spells this construct, so 'feature' is the phrase the
    # message reads as rather than a fragment of the selector, and
    # there is no one position to point at either
    err <- tryCatch(css("\\31 ns|div"), selectr_translation_error = identity)
    expect_equal(err$feature,
                 "a namespace prefix that is not an XPath name (`1ns`)")
    expect_equal(err$selector, "\\31 ns|div")
    expect_null(err$pos)
})

test_that("an escaped '*' is a prefix named '*', not the any-namespace one", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x, prefix = "")

    # Selectors 4 6.2: an <ns-prefix> is '[ <ident-token> | "*" ]? "|"'.
    # Only the delimiter '*' is the any-namespace wildcard; an
    # identifier that decodes to the same character names a prefix
    # '*', and no @namespace rule can bind it because it is not an
    # NCName. So it takes the same route as any other prefix XPath
    # cannot name, rather than silently widening to every namespace
    for (sel in c("\\2a|a", "\\*|a", "\\2a |a", "\\2a|*")) {
        expect_error(css(sel),
                     "The namespace prefix '\\*' is not an XPath name",
                     info = sel)
    }
    # ... and an attribute prefix is read the same way
    for (sel in c("[\\2a|href]", "[\\*|href]", "[\\2a|href=\"x\"]")) {
        expect_error(css(sel),
                     "The namespace prefix '\\*' is not an XPath name",
                     info = sel)
    }

    # A translation error, so it carries the usual fields
    err <- tryCatch(css("\\2a|a"), selectr_translation_error = identity)
    expect_equal(err$feature,
                 "a namespace prefix that is not an XPath name (`*`)")
    expect_equal(err$selector, "\\2a|a")

    # The delimiter is untouched: these still mean any namespace
    expect_equal(css("*|a"), "*[local-name() = 'a']")
    expect_equal(css("*|*"), "*")
    expect_equal(css("[*|href]"), "*[@*[local-name() = 'href']]")
    # An escaped '*' elsewhere is an ordinary name, as before
    expect_equal(css("\\2a"), "*[name() = '*' and namespace-uri() = '']")
    expect_equal(css("ns|\\2a"), "ns:*[local-name() = '*']")
    # '*|\2a' is an element named '*' in any namespace. The local name
    # is told apart from the universal selector the same way the prefix
    # is: by which token it came from, not by the character it spells
    expect_equal(css("*|\\2a"), "*[local-name() = '*']")
})

test_that("malformed namespace selectors are rejected", {
    gt <- GenericTranslator$new()
    css <- function(x) gt$css_to_xpath(x)

    expect_error(css("e|"), "Expected ident or '\\*'")
    # 'a||b' is not a malformed namespace selector but the Selectors 4
    # column combinator, so it is rejected by name
    expect_error(css("a||b"), "The column combinator '\\|\\|' is not supported")
    expect_error(css("div .|x"), "Expected ident")
})

test_that("namespace selectors match correct elements", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r xmlns:svg="http://www.w3.org/2000/svg" a="x">',
        '<e>plain</e><svg:e svg:a="y">svg</svg:e></r>'))
    ns <- xml2::xml_ns(doc)
    matches <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_name(nodes, ns)
    }

    expect_equal(matches("*|e"), c("e", "svg:e"))
    expect_equal(matches("|e"), "e")
    expect_equal(matches("|*"), c("r", "e"))
    expect_equal(matches("*|*"), c("r", "e", "svg:e"))
    expect_equal(matches("svg|e"), "svg:e")
    expect_equal(matches("[*|a]"), c("r", "svg:e"))
    expect_equal(matches("[|a]"), "r")
})

test_that("'[*|attr]' value tests run against every matching attribute", {
    gt <- GenericTranslator$new()
    xpath <- function(css) gt$css_to_xpath(css, prefix = "")
    any_lang <- "@*[local-name() = 'lang']"
    lower <- function(x) {
        paste0("translate(", x, ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
               "'abcdefghijklmnopqrstuvwxyz')")
    }

    # A string function would see only the first attribute of the
    # node-set, so each test sits in a predicate on the attribute step
    expect_equal(xpath("[*|lang^=en]"),
                 paste0("*[", any_lang, "[starts-with(., 'en')]]"))
    expect_equal(xpath("[*|lang$=US]"),
                 paste0("*[", any_lang,
                        "[substring(., string-length(.) - 1) = 'US']]"))
    expect_equal(xpath("[*|lang*=n-U]"),
                 paste0("*[", any_lang, "[contains(., 'n-U')]]"))
    expect_equal(xpath("[*|lang~=en]"),
                 paste0("*[", any_lang, "[contains(concat(' ', ",
                        "normalize-space(.), ' '), ' en ')]]"))
    expect_equal(xpath("[*|lang|=en]"),
                 paste0("*[", any_lang,
                        "[. = 'en' or starts-with(., 'en-')]]"))
    expect_equal(xpath("[*|lang=EN i]"),
                 paste0("*[", any_lang, "[", lower("."), " = 'en']]"))
    expect_equal(xpath("[*|lang^=EN i]"),
                 paste0("*[", any_lang,
                        "[starts-with(", lower("."), ", 'en')]]"))
    # '=' already compares every node of a node-set, and existence
    # needs no value, so both keep the plain form
    expect_equal(xpath("[*|lang=en]"), paste0("*[", any_lang, " = 'en']"))
    expect_equal(xpath("[*|lang=en s]"), paste0("*[", any_lang, " = 'en']"))
    expect_equal(xpath("[*|lang]"), paste0("*[", any_lang, "]"))
    # A test that can never match stays a constant
    expect_equal(xpath("[*|lang^='']"), "*[0]")
    expect_equal(xpath("[*|lang~='a b']"), "*[0]")
    # The HTML translator's per-attribute case rule applies under '*|'
    # to the attribute in no namespace alone, and an explicit flag to
    # every attribute
    ht <- HTMLTranslator$new()
    expect_equal(ht$css_to_xpath("[*|type^=Foo]", prefix = ""),
                 paste0("*[@*[local-name() = 'type'][namespace-uri() = '' ",
                        "and starts-with(", lower("."), ", 'foo') ",
                        "or starts-with(., 'Foo')]]"))
    expect_equal(ht$css_to_xpath("[*|type^=Foo i]", prefix = ""),
                 paste0("*[@*[local-name() = 'type']",
                        "[starts-with(", lower("."), ", 'foo')]]"))
    # A namespaced or unprefixed attribute is a single node, and keeps
    # the direct form
    expect_equal(xpath("[lang^=en]"), "*[starts-with(@lang, 'en')]")
    expect_equal(xpath("[ns|lang^=en]"), "*[starts-with(@ns:lang, 'en')]")
})

# Each 'p' carries two 'lang' attributes, only one of which satisfies
# every selector in any_lang_selectors; 'q' has neither. Both orders
# are covered, so a test of the first attribute alone fails one of them
any_lang_markup <- paste0(
    '<r xmlns:x="urn:x">',
    '<p id="first" lang="en-US x" xml:lang="fr"/>',
    '<p id="second" xml:lang="fr" lang="en-US x"/>',
    '<p id="third" x:lang="en-US x" lang="fr"/>',
    '<p id="fourth" lang="fr" x:lang="en-US x"/>',
    '<q id="none" lang="fr" xml:lang="fr"/></r>')
any_lang_ids <- c("first", "second", "third", "fourth")
any_lang_selectors <- c(
    "[*|lang^=en]", "[*|lang$=x]", "[*|lang*=n-U]", "[*|lang~=x]",
    "[*|lang|=en]", "[*|lang='en-US x']", "[*|lang='EN-us X' i]",
    "[*|lang^=EN i]", "[*|lang|=EN i]", "[*|lang~=X i]",
    "[*|lang$='-us x' i]", "[*|lang*=N-u i]")

test_that("'[*|attr]' value tests match any attribute (xml2)", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(any_lang_markup)
    ids <- function(sel) xml2::xml_attr(querySelectorAll(doc, sel), "id")
    for (sel in any_lang_selectors)
        expect_equal(ids(sel), any_lang_ids, info = sel)
    expect_equal(ids("[*|lang^=EN]"), character(0))
})

test_that("'[*|attr]' value tests match any attribute (XML)", {
    skip_if_not_installed("XML")

    doc <- XML::xmlParse(any_lang_markup, asText = TRUE)
    ids <- function(sel) {
        vapply(querySelectorAll(doc, sel), XML::xmlGetAttr,
               character(1), "id")
    }
    for (sel in any_lang_selectors)
        expect_equal(ids(sel), any_lang_ids, info = sel)
    expect_equal(ids("[*|lang^=EN]"), character(0))
})

test_that("HTML '[*|attr]' folds the value of the no-namespace attribute", {
    ht <- HTMLTranslator$new()
    xpath <- function(css) ht$css_to_xpath(css, prefix = "")
    any_type <- "@*[local-name() = 'type']"
    lower <- function(x) {
        paste0("translate(", x, ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
               "'abcdefghijklmnopqrstuvwxyz')")
    }
    split <- function(folded, exact) {
        paste0("*[", any_type, "[namespace-uri() = '' and ", folded,
               " or ", exact, "]]")
    }

    # Every operator tests the attribute in no namespace folded and any
    # other attribute exactly
    expect_equal(xpath("[*|type=Foo]"),
                 split(paste0(lower("."), " = 'foo'"), ". = 'Foo'"))
    expect_equal(xpath("[*|type~=Foo]"),
                 split(paste0("contains(concat(' ', normalize-space(",
                              lower("."), "), ' '), ' foo ')"),
                       "contains(concat(' ', normalize-space(.), ' '), ' Foo ')"))
    expect_equal(xpath("[*|type|=Foo]"),
                 split(paste0("(", lower("."), " = 'foo' or starts-with(",
                              lower("."), ", 'foo-'))"),
                       ". = 'Foo' or starts-with(., 'Foo-')"))
    expect_equal(xpath("[*|type^=Foo]"),
                 split(paste0("starts-with(", lower("."), ", 'foo')"),
                       "starts-with(., 'Foo')"))
    expect_equal(xpath("[*|type$=Foo]"),
                 split(paste0("substring(", lower("."), ", string-length(",
                              lower("."), ") - 2) = 'foo'"),
                       "substring(., string-length(.) - 2) = 'Foo'"))
    expect_equal(xpath("[*|type*=Foo]"),
                 split(paste0("contains(", lower("."), ", 'foo')"),
                       "contains(., 'Foo')"))

    # Existence, an empty value and a test that can never match are as
    # before
    expect_equal(xpath("[*|type]"), paste0("*[", any_type, "]"))
    expect_equal(xpath("[*|type='']"), paste0("*[", any_type, " = '']"))
    expect_equal(xpath("[*|type^='']"), "*[0]")
    expect_equal(xpath("[*|type~='a b']"), "*[0]")

    # An explicit flag applies to every attribute, whatever its namespace
    expect_equal(xpath("[*|type=Foo i]"),
                 paste0("*[", any_type, "[", lower("."), " = 'foo']]"))
    expect_equal(xpath("[*|type=Foo s]"),
                 paste0("*[", any_type, " = 'Foo']"))
    expect_equal(xpath("[*|type^=Foo s]"),
                 paste0("*[", any_type, "[starts-with(., 'Foo')]]"))

    # An attribute off the case-insensitive list is left exact
    expect_equal(xpath("[*|class^=Foo]"),
                 "*[@*[local-name() = 'class'][starts-with(., 'Foo')]]")

    # A named namespace or none is unchanged
    expect_equal(xpath("[type=Foo]"), paste0("*[", lower("@type"), " = 'foo']"))
    expect_equal(xpath("[|type=Foo]"), paste0("*[", lower("@type"), " = 'foo']"))
    expect_equal(xpath("[ns|type=Foo]"), "*[@ns:type = 'Foo']")

    # Neither the xhtml nor the generic translator folds
    for (tr in list(HTMLTranslator$new(xhtml = TRUE),
                    GenericTranslator$new())) {
        expect_equal(tr$css_to_xpath("[*|type=Foo]", prefix = ""),
                     paste0("*[", any_type, " = 'Foo']"))
        expect_equal(tr$css_to_xpath("[*|type^=Foo]", prefix = ""),
                     paste0("*[", any_type, "[starts-with(., 'Foo')]]"))
    }
})

# Each 'p' has a 'type' in no namespace and an 'x:type', in either
# order; the one spelt in upper case matches the case-insensitive
# selectors below only when it has no namespace
any_type_markup <- paste0(
    '<r xmlns:x="urn:x">',
    '<p id="a" type="FOO-bar baz" x:type="zzz"/>',
    '<p id="b" x:type="zzz" type="FOO-bar baz"/>',
    '<p id="c" x:type="FOO-bar baz" type="zzz"/>',
    '<p id="d" type="zzz" x:type="FOO-bar baz"/>',
    '<p id="e" x:type="foo-bar baz" type="zzz"/>',
    '<p id="f" type="zzz" x:type="foo-bar baz"/>',
    '<q id="g" type="foo-bar baz"/></r>')
any_type_expected <- list(
    "[*|type='foo-bar baz']" = c("a", "b", "e", "f", "g"),
    "[*|type~=foo-bar]" = c("a", "b", "e", "f", "g"),
    "[*|type|=foo]" = c("a", "b", "e", "f", "g"),
    "[*|type^=foo]" = c("a", "b", "e", "f", "g"),
    "[*|type$=BAZ]" = c("a", "b", "g"),
    "[*|type*=o-B]" = c("a", "b", "g"),
    "[*|type='foo-bar baz' s]" = c("e", "f", "g"),
    "[*|type^=foo s]" = c("e", "f", "g"),
    "[*|type^=FOO i]" = c("a", "b", "c", "d", "e", "f", "g"),
    "[*|type]" = c("a", "b", "c", "d", "e", "f", "g"),
    "[type^=foo]" = c("a", "b", "g"),
    "[|type^=foo]" = c("a", "b", "g"),
    "[x|type^=foo]" = c("e", "f"))

test_that("HTML '[*|attr]' folds only the no-namespace value (xml2)", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(any_type_markup)
    for (sel in names(any_type_expected)) {
        xp <- css_to_xpath(sel, prefix = "//", translator = "html")
        nodes <- xml2::xml_find_all(doc, xp, c(x = "urn:x"))
        expect_equal(xml2::xml_attr(nodes, "id"), any_type_expected[[sel]],
                     info = sel)
    }
})

test_that("HTML '[*|attr]' folds only the no-namespace value (XML)", {
    skip_if_not_installed("XML")

    doc <- XML::xmlParse(any_type_markup, asText = TRUE)
    for (sel in names(any_type_expected)) {
        xp <- css_to_xpath(sel, prefix = "//", translator = "html")
        nodes <- XML::getNodeSet(doc, xp, c(x = "urn:x"))
        expect_equal(vapply(nodes, XML::xmlGetAttr, character(1), "id"),
                     any_type_expected[[sel]], info = sel)
    }
})

test_that("an of-type pseudo-class counts a prefixed unsafe name", {
    skip_if_not_installed("xml2")

    # 'é' cannot be an XPath name test, so the step is 'ns:*' with the
    # local name in a condition; the of-type pseudo-classes must still
    # count siblings of that type, not every element in the namespace
    doc <- xml2::read_xml(paste0(
        '<r xmlns:n="http://ns">',
        '<n:f id="f1"/><n:é id="e1"/><n:f id="f2"/><n:é id="e2"/></r>'))
    ns <- c(ns = "http://ns")
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_attr(nodes, "id")
    }

    expect_equal(ids("ns|é:first-of-type"), "e1")
    expect_equal(ids("ns|é:last-of-type"), "e2")
    expect_equal(ids("ns|é:nth-of-type(2)"), "e2")
    expect_equal(ids("ns|é:only-of-type"), character(0))
})

test_that("namespaced pseudo-class arguments match by URI, not prefix", {
    skip_if_not_installed("xml2")

    # the document binds the SVG namespace to 's', the query to 'svg':
    # matching must go through the namespace map (URI), not compare
    # qualified names as strings
    doc <- xml2::read_xml(paste0(
        '<r xmlns:s="http://www.w3.org/2000/svg">',
        '<s:g id="g1"/><b id="b1"/></r>'))
    ns <- c(svg = "http://www.w3.org/2000/svg")
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"), ns)
        xml2::xml_attr(nodes, "id")
    }

    expect_equal(ids("svg|g"), "g1")
    expect_equal(ids(":is(svg|g)"), "g1")
    expect_equal(ids(":not(svg|g)"), c(NA, "b1")) # r and b
    expect_equal(ids(":is(svg|*)"), "g1")
    expect_equal(ids(":not(svg|*)"), c(NA, "b1")) # r and b
    expect_equal(xml2::xml_name(xml2::xml_find_all(
                     doc, css_to_xpath(":has(svg|g)", prefix = "//"), ns)),
                 "r")
})

test_that("unprefixed names match no namespace wherever they appear", {
    skip_if_not_installed("xml2")

    # An unprefixed name is an XPath name test wherever it sits in the
    # selector, so it matches in no namespace only: an element in a
    # *default* namespace needs an explicit prefix, inside a functional
    # pseudo-class argument exactly as at the top level
    doc <- xml2::read_xml(paste0(
        '<r id="root"><p id="plain"/><span id="plain-sib"/>',
        '<x xmlns="http://d" id="wrapper">',
        '<p id="defaulted"/><span id="defaulted-sib"/></x></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"),
                                    c(d = "http://d"))
        xml2::xml_attr(nodes, "id")
    }

    # top level and pseudo-class argument agree
    expect_equal(ids("p"), "plain")
    expect_equal(ids(":is(p)"), "plain")
    expect_equal(ids(":where(p)"), "plain")
    expect_equal(ids("*:not(p)"),
                 c("root", "plain-sib", "wrapper",
                   "defaulted", "defaulted-sib"))

    # so do the combinators, on either side and in either direction
    expect_equal(ids("r > p"), "plain")
    expect_equal(ids(":is(r > p)"), "plain")
    expect_equal(ids("p + span"), "plain-sib")
    expect_equal(ids(":is(p + span)"), "plain-sib")
    expect_equal(ids("p ~ span"), "plain-sib")
    expect_equal(ids(":is(p ~ span)"), "plain-sib")
    expect_equal(ids(":has(p)"), "root")
    expect_equal(ids(":has(> p)"), "root")
    expect_equal(ids(":has(+ span)"), "plain")

    # a prefix bound to the default namespace reaches the rest,
    # again in both positions
    expect_equal(ids("d|p"), "defaulted")
    expect_equal(ids(":is(d|p)"), "defaulted")
    expect_equal(ids(":has(d|p)"), c("root", "wrapper"))
})

test_that("'+' matches type selectors by namespace like '~'", {
    skip_if_not_installed("xml2")

    # The element following the first p is in a default namespace; a
    # bare type selector must not match it under '+', just as it does
    # not under '~'
    doc <- xml2::read_xml(paste0(
        '<r><w><p/><span xmlns="http://d" id="defaulted"/></w>',
        '<v><p/><span id="plain"/></v></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_equal(ids("p + span"), "plain")
    expect_equal(ids("p ~ span"), "plain")
    # '|span' pins the null namespace explicitly: same elements
    expect_equal(ids("p + |span"), "plain")
    # the universal selector matches either way
    expect_equal(ids("p + *"), c("defaulted", "plain"))
})

test_that("'|e' with an unsafe name does not match a default namespace", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(paste0(
        '<r><é id="plain"/>',
        '<w><é xmlns="http://default" id="defaulted"/></w></r>'))
    ids <- function(sel) {
        nodes <- xml2::xml_find_all(doc, css_to_xpath(sel, prefix = "//"))
        xml2::xml_attr(nodes, "id")
    }

    expect_equal(ids("|é"), "plain")
    expect_equal(ids("|é:first-of-type"), "plain")
})

test_that("an unprefixed unsafe attribute name is pinned to no namespace", {
    translators <- list(GenericTranslator$new(), HTMLTranslator$new(),
                        HTMLTranslator$new(xhtml = TRUE))
    pinned <- "attribute::*[name() = 'a:b' and namespace-uri() = '']"
    folded <- paste0("translate(", pinned, ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                     "'abcdefghijklmnopqrstuvwxyz')")
    for (tr in translators) {
        xpath <- function(css) tr$css_to_xpath(css, prefix = "")
        expect_equal(xpath("[a\\:b]"), paste0("*[", pinned, "]"))
        expect_equal(xpath("[|a\\:b]"), paste0("*[", pinned, "]"))
        expect_equal(xpath("[a\\:b=x]"), paste0("*[", pinned, " = 'x']"))
        expect_equal(xpath("[a\\:b^=x]"),
                     paste0("*[starts-with(", pinned, ", 'x')]"))
        expect_equal(xpath("[a\\:b*=x]"),
                     paste0("*[contains(", pinned, ", 'x')]"))
        expect_equal(xpath("[a\\:b|=x]"),
                     paste0("*[", pinned, " = 'x' or starts-with(",
                            pinned, ", 'x-')]"))
        expect_equal(xpath("[a\\:b=X i]"), paste0("*[", folded, " = 'x']"))
        expect_equal(xpath("[a\\:b=X s]"), paste0("*[", pinned, " = 'X']"))
    }
    # The HTML translator lowercases the name first
    expect_equal(HTMLTranslator$new()$css_to_xpath("[A\\:B]", prefix = ""),
                 paste0("*[", pinned, "]"))
})

unsafe_attr_xml <- paste0(
    '<r xmlns:a="urn:a"><p id="prefixed" a:b="x"/>',
    '<p id="lang" xml:lang="en"/></r>')
unsafe_attr_html <- paste0(
    '<div><p id="literal" a:b="x" xml:lang="EN"></p>',
    '<p id="none"></p></div>')

test_that("an escaped colon does not match a namespaced attribute (xml2)", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(unsafe_attr_xml)
    ids <- function(doc, sel)
        xml2::xml_attr(querySelectorAll(doc, sel), "id")

    # 'a:b' here is 'b' in namespace 'urn:a', not an attribute in no
    # namespace named 'a:b'
    expect_equal(ids(doc, "[a\\:b]"), character(0))
    expect_equal(ids(doc, "[a\\:b=x]"), character(0))
    expect_equal(ids(doc, "[a|b]"), "prefixed")
    # likewise 'xml:lang', which is namespaced in an XML document
    expect_equal(ids(doc, "[xml\\:lang]"), character(0))
    expect_equal(ids(doc, "[xml|lang]"), "lang")

    # An HTML parser keeps such names as plain attribute names in no
    # namespace, so they are still matched
    html <- xml2::read_html(unsafe_attr_html)
    expect_equal(ids(html, "[a\\:b]"), "literal")
    expect_equal(ids(html, "[a\\:b=x]"), "literal")
    expect_equal(ids(html, "[xml\\:lang]"), "literal")
    expect_equal(ids(html, "[xml\\:lang|=en i]"), "literal")
})

test_that("an escaped colon does not match a namespaced attribute (XML)", {
    skip_if_not_installed("XML")

    ids <- function(doc, sel) {
        vapply(querySelectorAll(doc, sel), XML::xmlGetAttr,
               character(1), "id")
    }

    doc <- XML::xmlParse(unsafe_attr_xml, asText = TRUE)
    expect_equal(ids(doc, "[a\\:b]"), character(0))
    expect_equal(ids(doc, "[a\\:b=x]"), character(0))
    expect_equal(ids(doc, "[a|b]"), "prefixed")
    expect_equal(ids(doc, "[xml\\:lang]"), character(0))
    expect_equal(ids(doc, "[xml|lang]"), "lang")

    html <- XML::htmlParse(unsafe_attr_html, asText = TRUE)
    expect_equal(ids(html, "[a\\:b]"), "literal")
    expect_equal(ids(html, "[a\\:b=x]"), "literal")
    expect_equal(ids(html, "[xml\\:lang]"), "literal")
    expect_equal(ids(html, "[xml\\:lang|=en i]"), "literal")
})

test_that("a zero-length 'ns' means no namespace map", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    xmldoc <- '<r><b id="one"/><w><b id="two"/></w></r>'
    ids <- function(doc, ns)
        as.character(unlist(lapply(querySelectorAll(doc, "b", ns = ns),
                                   function(node) {
            if (inherits(node, "xml_node"))
                xml2::xml_attr(node, "id")
            else
                XML::xmlGetAttr(node, "id")
        })))

    # xml2 defaults to the document's own namespace map, built by
    # walking the whole document; character(0) says there is nothing to
    # look up, and matches the same nodes in an un-namespaced document
    doc <- xml2::read_xml(xmldoc)
    expect_equal(ids(doc, character(0)), c("one", "two"))
    expect_equal(ids(doc, list()), c("one", "two"))
    expect_equal(ids(doc, NULL), c("one", "two"))
    # and from a node or a node set, not just the document
    root <- xml2::xml_root(doc)
    expect_equal(ids(root, character(0)), c("one", "two"))
    expect_equal(ids(querySelectorAll(doc, "r, w"), character(0)),
                 c("one", "two"))
    expect_equal(xml2::xml_attr(querySelector(doc, "b", ns = character(0)), "id"),
                 "one")

    # the XML package takes it the same way
    xdoc <- XML::xmlParse(xmldoc)
    expect_equal(ids(xdoc, character(0)), c("one", "two"))
    expect_equal(ids(XML::getNodeSet(xdoc, "//w"), character(0)), "two")

    # the namespaced functions still require a namespace to filter to
    expect_error(querySelectorAllNS(doc, "b", character(0)),
                 "A namespace must be provided.")
    expect_error(querySelectorNS(doc, "b", list()),
                 "A namespace must be provided.")
})

test_that("namespace prefixes that are not valid XML names are rejected", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    doc <- xml2::read_xml('<a xmlns:s="urn:s"><s:b/></a>')
    xdoc <- XML::xmlParse('<a xmlns:s="urn:s"><s:b/></a>')

    # The condition is is_ncname(), the same test the translator applies
    # to a prefix written in the selector, so the reach of the two stops
    # in the same place. "ª" and "nsɂ" are Unicode letters outside
    # XML 1.0's Appendix B tables: libxml2 refuses them, and catching
    # them here makes that an argument error rather than an "Invalid
    # expression" over an XPath the caller never wrote
    bad_names <- c("s p", "s:p", "1s", "ª", "nsɂ")
    for (bad in bad_names) {
        ns <- setNames("urn:s", bad)
        expect_error(querySelectorAllNS(doc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorNS(doc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorAllNS(xdoc, "s|b", ns),
                     class = "selectr_argument_error")
        expect_error(querySelectorNS(xdoc, "s|b", ns),
                     class = "selectr_argument_error")
    }
})

test_that("a namespace prefix may be any XML name, not just an ASCII one", {
    skip_if_not_installed("xml2")
    skip_if_not_installed("XML")

    xmldoc <- '<a xmlns:s="urn:s"><s:b id="one"/><b id="two"/></a>'
    doc <- xml2::read_xml(xmldoc)
    xdoc <- XML::xmlParse(xmldoc)
    ns <- setNames("urn:s", "é")

    # A prefix the parser accepts ('äöü|a' above) is accepted as a
    # binding too, so the two entry points meet: the prefix is spliced
    # into the node test and into the descendant-or-self filter, and
    # libxml2 parses both
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "é|b", ns = ns), "id"),
                 "one")
    expect_equal(xml2::xml_attr(querySelectorAllNS(doc, "é|b", ns), "id"),
                 "one")
    expect_equal(xml2::xml_attr(querySelectorNS(doc, "é|b", ns), "id"), "one")
    ids <- function(nodes) vapply(nodes, XML::xmlGetAttr, character(1), "id")
    expect_equal(ids(querySelectorAll(xdoc, "é|b", ns = ns)), "one")
    expect_equal(ids(querySelectorAllNS(xdoc, "é|b", ns)), "one")
    expect_equal(XML::xmlGetAttr(querySelectorNS(xdoc, "é|b", ns), "id"), "one")

    # The binding is by URI, so the document's own prefix is irrelevant
    expect_equal(xml2::xml_attr(querySelectorAll(doc, "é|*", ns = ns), "id"),
                 "one")
})

test_that("a reversed combinator step names its element when it can", {
    gt <- GenericTranslator$new()
    xpath <- function(css) gt$css_to_xpath(css, prefix = "")

    # A name the path step would use as its node test is the node test
    # of the reversed axis step too, prefixed or not, as is a
    # namespaced wildcard - with the rest of the compound as a predicate
    expect_equal(xpath(":is(span ~ p)"), "*[self::p and preceding-sibling::span]")
    expect_equal(xpath(":is(|span ~ p)"), "*[self::p and preceding-sibling::span]")
    expect_equal(xpath(":is(a|span ~ p)"),
                 "*[self::p and preceding-sibling::a:span]")
    expect_equal(xpath(":is(a|* > p)"), "*[self::p and parent::a:*]")
    expect_equal(xpath(":is(a|div.x p)"),
                 paste0("*[self::p and ancestor::a:div[contains(concat(' ', ",
                        "normalize-space(@class), ' '), ' x ')]]"))
    expect_equal(xpath(":is(a|div > a|p ~ p)"),
                 "*[self::p and preceding-sibling::a:p[parent::a:div]]")
    # A prefixed name that needs quoting keeps its prefix in the node
    # test and compares the local part, as it does in a path step
    expect_equal(xpath(":is(a|é ~ p)"),
                 "*[self::p and preceding-sibling::a:*[local-name() = 'é']]")

    # A compound with no name the node test can carry stays a predicate
    # on '*': the universal selector, any namespace, no namespace, and
    # an unprefixed name that needs quoting
    expect_equal(xpath(":is(* ~ p)"), "*[self::p and preceding-sibling::*]")
    expect_equal(xpath(":is(*.x ~ p)"),
                 paste0("*[self::p and preceding-sibling::*[contains(concat(",
                        "' ', normalize-space(@class), ' '), ' x ')]]"))
    expect_equal(xpath(":is(*|span ~ p)"),
                 "*[self::p and preceding-sibling::*[local-name() = 'span']]")
    expect_equal(xpath(":is(|* > p)"),
                 "*[self::p and parent::*[namespace-uri() = '']]")
    expect_equal(xpath(":is(é p)"),
                 paste0("*[self::p and ancestor::*[name() = 'é' and ",
                        "namespace-uri() = '']]"))
    # Under '+' the position predicate comes first, so the name moves
    # into a predicate after it
    expect_equal(xpath(":is(span + p)"),
                 "*[self::p and preceding-sibling::*[1][self::span]]")
    expect_equal(xpath(":is(a|span + p)"),
                 "*[self::p and preceding-sibling::*[1][self::a:span]]")
    expect_equal(xpath(":is(a|é + p)"),
                 paste0("*[self::p and preceding-sibling::*[1]",
                        "[local-name() = 'é' and self::a:*]]"))
})

# Elements in no namespace, in a default namespace and under two
# prefixes, with an unsafe name and nested chains, so that a node test
# matching any more or fewer elements than the 'self::' test it
# replaces would change some selector's result
reversed_step_markup <- paste0(
    '<root xmlns:a="urn:a" xmlns:b="urn:b">',
    '<div id="d1" class="x"><span id="s1"/><p id="p1"/><a:span id="as1"/>',
    '<p id="p2"/><b:span id="bs1"/><p id="p3"><em id="e1"><p id="p4"/>',
    '</em></p><é id="acc1"/><a:é id="aacc1"/><p id="p5"/></div>',
    '<section id="sec" xmlns="urn:a"><span id="ds1"/><p id="dp1"/>',
    '<div id="dd"><p id="dp2"><span id="ds2"/></p></div></section>',
    '<ul id="ul1"><li id="li1" class="x"/><li id="li2"/><span id="s2"/>',
    '<li id="li3"><ul id="ul2"><li id="li4"/></ul></li></ul>',
    '<a:div id="ad"><a:p id="ap1"/><a:span id="as2"/><a:p id="ap2">',
    '<p id="p6"/></a:p><span id="s3"/><a:p id="ap3"/></a:div></root>')
reversed_step_ns <- c(a = "urn:a", b = "urn:b")
# Each selector with the XPath its argument test had when the
# left-hand compound was always written as a predicate on '*'
reversed_step_cases <- list(
    ":is(span ~ p)" = "//*[self::p and preceding-sibling::*[self::span]]",
    ":is(div > p)" = "//*[self::p and parent::*[self::div]]",
    ":is(div p)" = "//*[self::p and ancestor::*[self::div]]",
    ":not(div *)" = "//*[not(ancestor::*[self::div])]",
    ":is(a|span ~ p)" = "//*[self::p and preceding-sibling::*[self::a:span]]",
    ":is(a|* ~ p)" = "//*[self::p and preceding-sibling::*[self::a:*]]",
    ":is(b|span ~ *)" = "//*[preceding-sibling::*[self::b:span]]",
    ":is(a|é ~ p)" = paste0("//*[self::p and preceding-sibling::*",
                            "[local-name() = 'é' and self::a:*]]"),
    ":is(a|div a|p p)" = paste0("//*[self::p and ancestor::*[self::a:p and ",
                                "ancestor::*[self::a:div]]]"),
    ":is(a|div > a|p ~ a|p)" = paste0("//*[self::a:p and preceding-sibling::*",
                                      "[self::a:p and parent::*[self::a:div]]]"),
    ":is(div > span ~ p)" = paste0("//*[self::p and preceding-sibling::*",
                                   "[self::span and parent::*[self::div]]]"),
    ":is(ul > li.x ~ li)" = paste0("//*[self::li and preceding-sibling::*[",
                                   "contains(concat(' ', normalize-space(@class),",
                                   " ' '), ' x ') and self::li and ",
                                   "parent::*[self::ul]]]"),
    ":is(p:nth-of-type(2) ~ p)" = paste0("//*[self::p and preceding-sibling::*[",
                                         "preceding-sibling::p[1] and not(",
                                         "preceding-sibling::p[2]) and self::p]]"),
    ":is(ul > li ~ li li)" = paste0("//*[self::li and ancestor::*[self::li and ",
                                    "preceding-sibling::*[self::li and ",
                                    "parent::*[self::ul]]]]"))

test_that("a named reversed combinator step matches as before (xml2)", {
    skip_if_not_installed("xml2")

    doc <- xml2::read_xml(reversed_step_markup)
    ids <- function(xp) {
        xml2::xml_attr(xml2::xml_find_all(doc, xp, reversed_step_ns), "id")
    }
    for (sel in names(reversed_step_cases)) {
        new <- css_to_xpath(sel, prefix = "//")
        expect_false(grepl("::*[self::", new, fixed = TRUE), info = sel)
        expect_equal(ids(new), ids(reversed_step_cases[[sel]]), info = sel)
        expect_gt(length(ids(new)), 0)
    }
})

test_that("a named reversed combinator step matches as before (XML)", {
    skip_if_not_installed("XML")

    doc <- XML::xmlParse(reversed_step_markup, asText = TRUE)
    ids <- function(xp) {
        unlist(XML::xpathApply(doc, xp, XML::xmlGetAttr, "id",
                               namespaces = reversed_step_ns))
    }
    for (sel in names(reversed_step_cases)) {
        new <- css_to_xpath(sel, prefix = "//")
        expect_equal(ids(new), ids(reversed_step_cases[[sel]]), info = sel)
        expect_gt(length(ids(new)), 0)
    }
})
