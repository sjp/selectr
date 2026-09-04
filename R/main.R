# A bad R-level argument, e.g. the wrong type, length, or an NA where
# one is not allowed. See selectr_abort() in errors.R.
argument_stop <- function(...) {
    selectr_abort(paste0(...), "selectr_argument_error")
}

# Key identifying one (selector, prefix, translator) translation. The
# selector is length-prefixed so that no combination of selector and
# prefix values can collide.
xpath_cache_key <- function(selector, prefix, translator) {
    paste0(translator, "\r", nchar(selector), "\r", selector, "\r", prefix)
}

# The three translators hold no per-call state (see GenericTranslator
# and HTMLTranslator in xpath.R: neither ever assigns to a 'self$'
# field outside 'initialize'), so one instance of each is created
# lazily and reused rather than allocating a fresh R6 object -- and
# its inheritance chain of fields -- on every css_to_xpath() call.
# This is purely an internal reuse of otherwise-stateless objects and
# is not a cache of translation results.
.selectr_translators <- new.env(parent = emptyenv())

get_translator <- function(trans) {
    obj <- .selectr_translators[[trans]]
    if (is.null(obj)) {
        obj <- if (trans == "html") {
            HTMLTranslator$new()
        } else if (trans == "xhtml") {
            HTMLTranslator$new(xhtml = TRUE)
        } else {
            GenericTranslator$new()
        }
        assign(trans, obj, envir = .selectr_translators)
    }
    obj
}

css_to_xpath <- function(selector, prefix = "descendant-or-self::", translator = "generic") {
    if (missing(selector) || is.null(selector))
        argument_stop("A valid selector (character vector) must be provided.")

    if (!is.character(selector))
        argument_stop("The 'selector' argument must be a character vector")
    if (!is.character(prefix))
        argument_stop("The 'prefix' argument must be a character vector")
    if (!is.character(translator))
        argument_stop("The 'translator' argument must be a character vector")

    if (anyNA(selector))
        argument_stop("NA values are not allowed in the 'selector' argument")
    if (anyNA(prefix))
        argument_stop("NA values are not allowed in the 'prefix' argument")
    if (anyNA(translator))
        argument_stop("NA values are not allowed in the 'translator' argument")

    # Bytes that are invalid in their declared (or native) encoding
    # would otherwise reach nchar() in tokenize() and raise a base R
    # "invalid multibyte string" error, breaking the selectr_error
    # contract. enc2utf8() also transcodes valid non-UTF-8 input (e.g.
    # a latin1-marked string) so it passes validUTF8() untouched.
    if (!all(validUTF8(enc2utf8(selector))))
        argument_stop("The 'selector' argument contains invalid or non-convertible bytes")
    if (!all(validUTF8(enc2utf8(prefix))))
        argument_stop("The 'prefix' argument contains invalid or non-convertible bytes")

    zeroLengthArgs <- character(0)
    if (!length(selector))
        zeroLengthArgs <- c(zeroLengthArgs, "selector")
    if (!length(prefix))
        zeroLengthArgs <- c(zeroLengthArgs, "prefix")
    if (!length(translator))
        zeroLengthArgs <- c(zeroLengthArgs, "translator")

    if (length(zeroLengthArgs)) {
        plural <- if (length(zeroLengthArgs) > 1) "s" else ""
        argument_stop("Zero length character vector found for the following argument",
             plural,
             ": ",
             paste0(zeroLengthArgs, collapse = ", "))
    }

    # match.arg() does unambiguous-prefix matching (e.g. "x" ->
    # "xhtml"), which is worth keeping; only its error wording is
    # replaced, since "'arg' should be one of ..." names the wrong
    # (internal) argument and doesn't say what the caller passed.
    validTranslators <- c("generic", "html", "xhtml")
    translator <- ascii_lower(translator)
    badTranslators <- character(0)
    translator <- vapply(translator, function(tran) {
        tryCatch(
            match.arg(tran, validTranslators),
            error = function(e) {
                badTranslators <<- c(badTranslators, tran)
                NA_character_
            }
        )
    }, character(1), USE.NAMES = FALSE)
    if (length(badTranslators))
        argument_stop("'translator' must be one of \"",
             paste0(validTranslators, collapse = "\", \""),
             "\", not \"", paste0(unique(badTranslators), collapse = "\", \""), "\"")

    # Only length-1 arguments are broadcast: recycling a vector whose
    # length is a fraction of another's would silently turn a mistyped
    # call into a plausible-looking result.
    argLengths <- c(selector = length(selector),
                    prefix = length(prefix),
                    translator = length(translator))
    maxArgLength <- max(argLengths)
    badArgs <- names(argLengths)[argLengths != 1L & argLengths != maxArgLength]

    if (length(badArgs)) {
        plural <- if (length(badArgs) > 1) "s" else ""
        argument_stop("Arguments must have length 1 or a common length (",
             maxArgLength,
             "), which the following argument",
             plural,
             " do not: ",
             paste0(badArgs, " (length ", argLengths[badArgs], ")",
                    collapse = ", "))
    }

    selector <- rep(selector, length.out = maxArgLength)
    prefix <- rep(prefix, length.out = maxArgLength)
    translator <- rep(translator, length.out = maxArgLength)

    # Translate each distinct (selector, prefix, translator) triple
    # only once per call, e.g. c("#a", "#b", "#a") parses twice. The
    # cache is local to this call so it cannot grow across calls.
    cache <- new.env(parent = emptyenv())
    results <- character(maxArgLength)
    for (i in seq_len(maxArgLength)) {
        sel <- selector[i]
        pref <- prefix[i]
        trans <- translator[i]

        # R limits a symbol - and therefore an environment key - to
        # 10000 bytes, so an unusually long selector is translated
        # uncached rather than failing to be looked up
        key <- xpath_cache_key(sel, pref, trans)
        cacheable <- nchar(key, type = "bytes") < 10000L
        cached <- if (cacheable) cache[[key]] else NULL
        if (is.null(cached)) {
            cached <- get_translator(trans)$css_to_xpath(sel, pref)
            if (cacheable)
                cache[[key]] <- cached
        }
        results[i] <- cached
    }

    results
}

querySelector <- function(doc, selector, ns = NULL, ...) {
    UseMethod("querySelector", doc)
}

querySelectorAll <- function(doc, selector, ns = NULL, ...) {
    UseMethod("querySelectorAll", doc)
}

querySelectorNS <- function(doc, selector, ns,
                            prefix = "descendant-or-self::", ...) {
    UseMethod("querySelectorNS", doc)
}

querySelectorAllNS <- function(doc, selector, ns,
                               prefix = "descendant-or-self::", ...) {
    UseMethod("querySelectorAllNS", doc)
}

querySelector.default <- function(doc, selector, ns = NULL, ...) {
    argument_stop("The object given to querySelector() is not an 'XML' or 'xml2' document or node.")
}

querySelectorAll.default <- function(doc, selector, ns = NULL, ...) {
    argument_stop("The object given to querySelectorAll() is not an 'XML' or 'xml2' document or node.")
}

querySelectorNS.default <- function(doc, selector, ns,
                                    prefix = "descendant-or-self::", ...) {
    argument_stop("The object given to querySelectorNS() is not an 'XML' or 'xml2' document or node.")
}

querySelectorAllNS.default <- function(doc, selector, ns,
                                    prefix = "descendant-or-self::", ...) {
    argument_stop("The object given to querySelectorAllNS() is not an 'XML' or 'xml2' document or node.")
}

# XML::xmlTreeParse() and XML::htmlTreeParse() return a tree of R lists
# rather than a libxml2 document, and XPath can only be evaluated over
# the latter. Such a tree is an 'XML' document, so the default method's
# message would send the reader looking for the wrong problem; point at
# the parsers that give a searchable document instead.
rLevelTreeStop <- function(fname) {
    argument_stop("The object given to ", fname, "() is an R-level 'XML' ",
                  "tree, which cannot be searched with XPath. Re-parse the ",
                  "document with XML::xmlParse() or XML::htmlParse() ",
                  "(equivalently, with useInternalNodes = TRUE).")
}

querySelector.XMLDocumentContent <-
querySelector.XMLDocument <-
querySelector.XMLNode <- function(doc, selector, ns = NULL, ...) {
    rLevelTreeStop("querySelector")
}

querySelectorAll.XMLDocumentContent <-
querySelectorAll.XMLDocument <-
querySelectorAll.XMLNode <- function(doc, selector, ns = NULL, ...) {
    rLevelTreeStop("querySelectorAll")
}

querySelectorNS.XMLDocumentContent <-
querySelectorNS.XMLDocument <-
querySelectorNS.XMLNode <- function(doc, selector, ns,
                                    prefix = "descendant-or-self::", ...) {
    rLevelTreeStop("querySelectorNS")
}

querySelectorAllNS.XMLDocumentContent <- # nolint: object_length_linter.
querySelectorAllNS.XMLDocument <-
querySelectorAllNS.XMLNode <- function(doc, selector, ns,
                                       prefix = "descendant-or-self::", ...) {
    rLevelTreeStop("querySelectorAllNS")
}

# The first step shared by the XML methods below: validate the
# selector, settle on the translator for the document and translate,
# and put the namespace object into the form XML::getNodeSet() takes.
xmlQuery <- function(doc, selector, ns, translator, ...) {
    validateSelector(selector)
    translator <- xmlTranslator(translator, doc)
    list(xpath = css_to_xpath(selector, translator = translator, ...),
         ns = if (is.null(ns)) NULL else formatNS(ns))
}

# XML::getNodeSet() derives a default set of namespaces from the
# document, so "no namespaces given" has to be an absent argument
# rather than a NULL one.
xmlMatches <- function(node, xpath, ns) {
    if (is.null(ns))
        XML::getNodeSet(node, xpath)
    else
        XML::getNodeSet(node, xpath, ns)
}

# The first node the expression matches, or NULL. A positional
# predicate applies to a node set in document order, so parenthesising
# the whole expression and taking [1] picks out the same node as the
# first of the full result -- without the XML package wrapping each of
# the other matches in an R object on the way to discarding it.
xmlFirstMatch <- function(node, xpath, ns) {
    results <- xmlMatches(node, paste0("(", xpath, ")[1]"), ns)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelector.XMLInternalNode <- function(doc, selector, ns = NULL,
                                          translator = NULL, ...) {
    query <- xmlQuery(doc, selector, ns, translator, ...)
    xmlFirstMatch(doc, query$xpath, query$ns)
}

querySelector.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    querySelector(XML::xmlRoot(doc), selector, ns, ...)
}

# Each node of the set is queried in turn and the first match ends the
# search, which is the node querySelectorAll() would return first.
querySelector.XMLNodeSet <- function(doc, selector, ns = NULL,
                                     translator = NULL, ...) {
    query <- xmlQuery(doc, selector, ns, translator, ...)
    for (i in seq_along(doc)) {
        result <- xmlFirstMatch(doc[[i]], query$xpath, query$ns)
        if (!is.null(result))
            return(result)
    }
    NULL
}

querySelectorAll.XMLInternalNode <- function(doc, selector, ns = NULL,
                                             translator = NULL, ...) {
    query <- xmlQuery(doc, selector, ns, translator, ...)
    xmlMatches(doc, query$xpath, query$ns)
}

querySelectorAll.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    querySelectorAll(XML::xmlRoot(doc), selector, ns, ...)
}

querySelectorAll.XMLNodeSet <- function(doc, selector, ns = NULL,
                                        translator = NULL, ...) {
    query <- xmlQuery(doc, selector, ns, translator, ...)
    results <- lapply(doc, xmlMatches, query$xpath, query$ns)
    results <- unlist(results, recursive = FALSE)
    if (is.null(results))
        results <- list()
    # A node matched from more than one node in the set is returned
    # once, at the position it was first matched. This mirrors what
    # xml2 does when given a nodeset.
    structure(unique(results), class = "XMLNodeSet")
}

querySelectorNS.XMLNodeSet          <-
querySelectorNS.XMLInternalNode     <-
querySelectorNS.XMLInternalDocument <- function(doc, selector, ns,
                                                prefix = "descendant-or-self::", ...) {
    ns_dispatch(querySelector, doc, selector, ns, prefix, ...)
}

querySelectorAllNS.XMLNodeSet          <-
querySelectorAllNS.XMLInternalNode     <-
querySelectorAllNS.XMLInternalDocument <- function(doc, selector, ns,
                                                   prefix = "descendant-or-self::", ...) {
    ns_dispatch(querySelectorAll, doc, selector, ns, prefix, ...)
}

# The xml2 counterpart of xmlQuery(). xml2 wants the namespaces as an
# argument to every query, and takes the document's own when the
# caller named none.
xml2Query <- function(doc, selector, ns, translator, ...) {
    validateSelector(selector)
    translator <- xml2Translator(translator, doc)
    list(xpath = css_to_xpath(selector, translator = translator, ...),
         ns = if (is.null(ns)) xml2::xml_ns(doc) else formatNS(ns))
}

# xml2::xml_find_first() stops at the first match, which is the
# shortcut the XML methods above get from a parenthesised expression
# and a [1] predicate; neither has to find every match to return one.
querySelector.xml_node <- function(doc, selector, ns = NULL,
                                   translator = NULL, ...) {
    query <- xml2Query(doc, selector, ns, translator, ...)
    result <- xml2::xml_find_first(doc, query$xpath, query$ns)
    if (length(result))
        result
    else
        NULL
}

querySelectorAll.xml_node <- function(doc, selector, ns = NULL,
                                      translator = NULL, ...) {
    query <- xml2Query(doc, selector, ns, translator, ...)
    xml2::xml_find_all(doc, query$xpath, query$ns)
}

# As for XMLNodeSet above, the nodes are queried in turn and the first
# match ends the search.
querySelector.xml_nodeset <- function(doc, selector, ns = NULL,
                                      translator = NULL, ...) {
    query <- xml2Query(doc, selector, ns, translator, ...)
    for (i in seq_along(doc)) {
        result <- xml2::xml_find_first(doc[[i]], query$xpath, query$ns)
        if (length(result))
            return(result)
    }
    NULL
}

querySelectorAll.xml_nodeset <- function(doc, selector, ns = NULL,
                                         translator = NULL, ...) {
    query <- xml2Query(doc, selector, ns, translator, ...)
    # xml2 evaluates the expression from each node in turn, so a
    # relative selector (e.g. ":scope > a") applies per node, and a
    # node matched more than once is returned only once.
    xml2::xml_find_all(doc, query$xpath, query$ns)
}

querySelector.xml_missing <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    NULL
}

querySelectorAll.xml_missing <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    emptyNodeSet()
}

querySelectorNS.xml_missing <-
querySelectorNS.xml_nodeset <-
querySelectorNS.xml_node <- function(doc, selector, ns,
                                     prefix = "descendant-or-self::", ...) {
    ns_dispatch(querySelector, doc, selector, ns, prefix, ...)
}

querySelectorAllNS.xml_missing <-
querySelectorAllNS.xml_nodeset <-
querySelectorAllNS.xml_node <- function(doc, selector, ns,
                                        prefix = "descendant-or-self::", ...) {
    ns_dispatch(querySelectorAll, doc, selector, ns, prefix, ...)
}

# The translator for a query on the xml2 object 'doc' that did not
# name one. Users scraping HTML almost always want the "html"
# translator, so a document parsed as HTML gets it; everything else
# keeps the "generic" (XML) translator that css_to_xpath() defaults to.
#
# Both packages use the same classes for the nodes of HTML and XML
# content, so the kind of document cannot be found by dispatch and is
# asked of libxml2 instead. The document node of a document read by
# xml2::read_html() reports its type as "html_document", which is true
# whether the query starts from the document, one of its nodes or a
# node set. A node set may be empty, and so have no document to ask,
# in which case the query is generic.
xml2Translator <- function(translator, doc) {
    if (!is.null(translator))
        return(translator)
    type <- tryCatch(xml2::xml_type(xml2::xml_parent(xml2::xml_root(doc))),
                     error = function(e) NA_character_)
    if (identical(type, "html_document"))
        "html"
    else
        "generic"
}

# The XML package counterpart of xml2Translator(). Every node knows
# its owning document, reached as the "/" node here, and the XML
# package classes that document node as an XMLHTMLDocumentNode when
# the document was parsed by XML::htmlParse(). A node that belongs to
# no document (one built by XML::newXMLNode(), say) has nothing to
# ask, as does an empty node set, and both stay generic.
xmlTranslator <- function(translator, doc) {
    if (!is.null(translator))
        return(translator)
    if (inherits(doc, "XMLNodeSet")) {
        if (!length(doc))
            return("generic")
        doc <- doc[[1]]
    }
    docNode <- tryCatch(XML::getNodeSet(doc, "/")[[1]],
                        error = function(e) NULL)
    if (inherits(docNode, "XMLHTMLDocumentNode"))
        "html"
    else
        "generic"
}

# xml2 does not export a constructor for an empty nodeset, but this
# is the structure it uses for one.
emptyNodeSet <- function() {
    structure(list(), class = "xml_nodeset")
}

validateSelector <- function(selector) {
    if (missing(selector) || !is.character(selector) ||
        length(selector) != 1 || is.na(selector))
        argument_stop("A valid selector (single character string) must be provided.")
}

# Takes a named vector or list and gives a named vector back
formatNS <- function(ns) {
    if (is.null(ns))
        return(NULL)
    if (!is.list(ns) && !is.character(ns))
        argument_stop("A namespace object must be either a named list or a named character vector.")
    # A zero-length namespace object means "no namespaces"
    if (!length(ns))
        return(character())
    nsNames <- names(ns)
    if (is.null(nsNames) || anyNA(nsNames) || !all(nzchar(nsNames)))
        argument_stop("The namespace object must be a named list or character vector; every element needs a non-empty name.")
    badNames <- nsNames[!vapply(nsNames, is_safe_name, logical(1))]
    if (length(badNames))
        argument_stop("Namespace prefixes must be valid XML names (e.g. 'svg', not '",
                       badNames[1], "').")
    if (is.list(ns) && any(lengths(ns) != 1))
        argument_stop("Each element in the namespace object must be a single character string.")
    ns <- unlist(ns)
    if (!is.character(ns))
        argument_stop("The values in the namespace object must be a character vector.")
    if (anyNA(ns) || !all(nzchar(ns)))
        argument_stop("The values in the namespace object must be non-missing, non-empty strings.")
    names(ns) <- nsNames
    ns
}

# Shared body of the four querySelectorNS()/querySelectorAllNS()
# methods: validate that a namespace was supplied, then delegate to
# 'query_fun' (querySelector() or querySelectorAll()) with 'prefix'
# expanded to scope the query to namespaced descendants.
ns_dispatch <- function(query_fun, doc, selector, ns,
                        prefix = "descendant-or-self::", ...) {
    validateSelector(selector)
    if (missing(ns) || !length(ns))
        argument_stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    query_fun(doc, selector, ns, prefix = prefix, ...)
}

# The namespace filter is relative to the queried node, so that a query
# starting from a node searches that node's subtree rather than the whole
# document.
formatNSPrefix <- function(ns, prefix) {
    filters <- paste0("descendant-or-self::", names(ns), ":*", collapse = "|")
    prefix <- paste0("(", filters, ")/", prefix)
    prefix
}
