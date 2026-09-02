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
    translator <- tolower(translator)
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

querySelector.XMLNodeSet          <-
querySelector.XMLInternalNode     <-
querySelector.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    # The selector is validated by the querySelectorAll() method below
    results <- querySelectorAll(doc, selector, ns, ...)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelectorAll.XMLInternalNode <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    xpath <- css_to_xpath(selector, ...)
    if (!is.null(ns)) {
        ns <- formatNS(ns)
        XML::getNodeSet(doc, xpath, ns)
    } else {
        XML::getNodeSet(doc, xpath)
    }
}

querySelectorAll.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    doc <- XML::xmlRoot(doc)
    querySelectorAll(doc, selector, ns, ...)
}

# XML::htmlParse() gives a document the "HTMLInternalDocument" class,
# so an HTML document is recognised by dispatch. The nodes of such a
# document are plain XMLInternalNodes, indistinguishable from those of
# an XML document, so a query starting from a node (or a node set) is
# not recognised and keeps the generic translator.
#
# Only querySelectorAll() needs a method here: querySelector() and the
# two namespaced functions call the generic on the document itself, so
# they arrive back here.
querySelectorAll.HTMLInternalDocument <- function(doc, selector, ns = NULL,
                                                  translator = "html", ...) {
    validateSelector(selector)
    doc <- XML::xmlRoot(doc)
    querySelectorAll(doc, selector, ns, translator = translator, ...)
}

querySelectorAll.XMLNodeSet <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    xpath <- css_to_xpath(selector, ...)
    if (!is.null(ns))
        ns <- formatNS(ns)
    results <- lapply(doc, function(node) {
        if (is.null(ns))
            XML::getNodeSet(node, xpath)
        else
            XML::getNodeSet(node, xpath, ns)
    })
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

# Unlike querySelector.XMLInternalNode/Document above, this does not
# delegate to querySelectorAll(): xml2::xml_find_first() is cheaper
# than xml2::xml_find_all()[[1]] (it can stop at the first match), so
# the query is translated and run directly instead. The XML package
# has no first-match equivalent, so its methods have no such shortcut
# available and materialise the full node set regardless.
querySelector.xml_node <- function(doc, selector, ns = NULL,
                                   translator = NULL, ...) {
    validateSelector(selector)
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    else
        ns <- formatNS(ns)
    translator <- defaultTranslator(translator, doc)
    xpath <- css_to_xpath(selector, translator = translator, ...)
    result <- xml2::xml_find_first(doc, xpath, ns)
    if (length(result))
        result
    else
        NULL
}

querySelectorAll.xml_node <- function(doc, selector, ns = NULL,
                                      translator = NULL, ...) {
    validateSelector(selector)
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    else
        ns <- formatNS(ns)
    translator <- defaultTranslator(translator, doc)
    xml2::xml_find_all(doc, css_to_xpath(selector, translator = translator, ...), ns)
}

querySelector.xml_nodeset <- function(doc, selector, ns = NULL, ...) {
    # The selector is validated by the querySelectorAll() method below
    results <- querySelectorAll(doc, selector, ns, ...)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelectorAll.xml_nodeset <- function(doc, selector, ns = NULL,
                                         translator = NULL, ...) {
    validateSelector(selector)
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    else
        ns <- formatNS(ns)
    translator <- defaultTranslator(translator, doc)
    xpath <- css_to_xpath(selector, translator = translator, ...)
    # xml2 evaluates the expression from each node in turn, so a
    # relative selector (e.g. ":scope > a") applies per node, and a
    # node matched more than once is returned only once.
    xml2::xml_find_all(doc, xpath, ns)
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
# xml2 uses the same classes for HTML and XML content, so unlike the
# XML package the kind of document cannot be found by dispatch and is
# instead asked of libxml2 here. The document node of a document read
# by xml2::read_html() reports its type as "html_document", which is
# true for its nodes and node sets too. A node set may be empty, and
# so have no document to ask, in which case the query is generic.
defaultTranslator <- function(translator, doc) {
    if (!is.null(translator))
        return(translator)
    type <- tryCatch(xml2::xml_type(xml2::xml_parent(xml2::xml_root(doc))),
                     error = function(e) NA_character_)
    if (identical(type, "html_document"))
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
