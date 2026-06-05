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

        key <- xpath_cache_key(sel, pref, trans)
        cached <- cache[[key]]
        if (is.null(cached)) {
            tran <- if (trans == "html") {
                HTMLTranslator$new()
            } else if (trans == "xhtml") {
                HTMLTranslator$new(xhtml = TRUE)
            } else {
                GenericTranslator$new()
            }

            cached <- tran$css_to_xpath(sel, pref)
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

querySelector.xml_node <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    else
        ns <- formatNS(ns)
    xpath <- css_to_xpath(selector, ...)
    result <- xml2::xml_find_first(doc, xpath, ns)
    if (length(result))
        result
    else
        NULL
}

querySelectorAll.xml_node <- function(doc, selector, ns = NULL, ...) {
    validateSelector(selector)
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    else
        ns <- formatNS(ns)
    xpath <- css_to_xpath(selector, ...)
    xml2::xml_find_all(doc, xpath, ns)
}

querySelectorNS.xml_node <- function(doc, selector, ns,
                                     prefix = "descendant-or-self::", ...) {
    validateSelector(selector)
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.xml_node <- function(doc, selector, ns,
                                        prefix = "descendant-or-self::", ...) {
    validateSelector(selector)
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
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
