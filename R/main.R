# Key identifying one (selector, prefix, translator) translation. The
# selector is length-prefixed so that no combination of selector and
# prefix values can collide.
xpath_cache_key <- function(selector, prefix, translator) {
    paste0(translator, "\r", nchar(selector), "\r", selector, "\r", prefix)
}

css_to_xpath <- function(selector, prefix = "descendant-or-self::", translator = "generic") {
    if (missing(selector) || is.null(selector))
        stop("A valid selector (character vector) must be provided.")

    if (!is.character(selector))
        stop("The 'selector' argument must be a character vector")
    if (!is.character(prefix))
        stop("The 'prefix' argument must be a character vector")
    if (!is.character(translator))
        stop("The 'translator' argument must be a character vector")

    if (anyNA(selector))
        stop("NA values are not allowed in the 'selector' argument")
    if (anyNA(prefix))
        stop("NA values are not allowed in the 'prefix' argument")
    if (anyNA(translator))
        stop("NA values are not allowed in the 'translator' argument")

    zeroLengthArgs <- character(0)
    if (!length(selector))
        zeroLengthArgs <- c(zeroLengthArgs, "selector")
    if (!length(prefix))
        zeroLengthArgs <- c(zeroLengthArgs, "prefix")
    if (!length(translator))
        zeroLengthArgs <- c(zeroLengthArgs, "translator")

    if (length(zeroLengthArgs)) {
        plural <- if (length(zeroLengthArgs) > 1) "s" else ""
        stop("Zero length character vector found for the following argument",
             plural,
             ": ",
             paste0(zeroLengthArgs, collapse = ", "))
    }

    translator <- sapply(translator, function(tran) {
        match.arg(tolower(tran), c("generic", "html", "xhtml"))
    })

    maxArgLength <- max(length(selector), length(prefix), length(translator))
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
            tran <- if (trans == "html") {
                HTMLTranslator$new()
            } else if (trans == "xhtml") {
                HTMLTranslator$new(xhtml = TRUE)
            } else {
                GenericTranslator$new()
            }

            cached <- tran$css_to_xpath(sel, pref)
            if (cacheable)
                cache[[key]] <- cached
        }
        results[i] <- cached
    }

    as.character(results)
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
    stop("The object given to querySelector() is not an 'XML' or 'xml2' document or node.")
}

querySelectorAll.default <- function(doc, selector, ns = NULL, ...) {
    stop("The object given to querySelectorAll() is not an 'XML' or 'xml2' document or node.")
}

querySelectorNS.default <- function(doc, selector, ns,
                                    prefix = "descendant-or-self::", ...) {
    stop("The object given to querySelectorNS() is not an 'XML' or 'xml2' document or node.")
}

querySelectorAllNS.default <- function(doc, selector, ns,
                                    prefix = "descendant-or-self::", ...) {
    stop("The object given to querySelectorAllNS() is not an 'XML' or 'xml2' document or node.")
}

querySelector.XMLNodeSet          <-
querySelector.XMLInternalNode     <-
querySelector.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
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
    validateSelector(selector)
    if (missing(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.XMLNodeSet          <-
querySelectorAllNS.XMLInternalNode     <-
querySelectorAllNS.XMLInternalDocument <- function(doc, selector, ns,
                                                   prefix = "descendant-or-self::", ...) {
    validateSelector(selector)
    if (missing(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
}

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
    validateSelector(selector)
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
    validateSelector(selector)
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.xml_missing <-
querySelectorAllNS.xml_nodeset <-
querySelectorAllNS.xml_node <- function(doc, selector, ns,
                                        prefix = "descendant-or-self::", ...) {
    validateSelector(selector)
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
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
        stop("A valid selector (single character string) must be provided.")
}

# Takes a named vector or list and gives a named vector back
formatNS <- function(ns) {
    if (is.null(ns))
        return(NULL)
    if (!is.list(ns) && !is.character(ns))
        stop("A namespace object must be either a named list or a named character vector.")
    nsNames <- names(ns)
    if (is.null(nsNames) || anyNA(nsNames) || !all(nzchar(nsNames)))
        stop("The namespace object either missing some or all names for each element in its collection.")
    if (is.list(ns) && any(lengths(ns) != 1))
        stop("Each element in the namespace object must be a single character string.")
    ns <- unlist(ns)
    if (!is.character(ns))
        stop("The values in the namespace object must be a character vector.")
    names(ns) <- nsNames
    ns
}

formatNSPrefix <- function(ns, prefix) {
    filters <- paste0("//", names(ns), ":*", collapse = "|")
    prefix <- paste0("(", filters, ")/", prefix)
    prefix
}
