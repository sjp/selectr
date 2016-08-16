css_to_xpath <- function(selector, prefix = "descendant-or-self::", translator = "generic") {
    results <- Map(function(selector, prefix, translator) {
        tran <- if (translator == "html") {
            HTMLTranslator$new()
        } else if (translator == "xhtml") {
            HTMLTranslator$new(xhtml = TRUE)
        } else {
            GenericTranslator$new()
        }
        tran$css_to_xpath(selector, prefix = prefix)
    }, selector, prefix, translator)
    as.character(results)
}

querySelector <- function(doc, selector, ns = NULL, ...) {
    tryLoadNamespaces()
    UseMethod("querySelector", doc)
}

querySelectorAll <- function(doc, selector, ns = NULL, ...) {
    tryLoadNamespaces()
    UseMethod("querySelectorAll", doc)
}

querySelectorNS <- function(doc, selector, ns,
                            prefix = "descendant-or-self::", ...) {
    tryLoadNamespaces()
    UseMethod("querySelectorNS", doc)
}

querySelectorAllNS <- function(doc, selector, ns,
                               prefix = "descendant-or-self::", ...) {
    tryLoadNamespaces()
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
    results <- querySelectorAll(doc, selector, ns, ...)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelectorAll.XMLInternalNode <- function(doc, selector, ns = NULL, ...) {
    if (!requireNamespace("XML"))
        stop("The 'XML' package is required to query this object.")
    xpath <- css_to_xpath(selector, ...)
    if (! is.null(ns)) {
        XML::getNodeSet(doc, xpath, ns)
    } else {
        XML::getNodeSet(doc, xpath)
    }
}

querySelectorAll.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    if (!requireNamespace("XML"))
        stop("The 'XML' package is required to query this object.")
    doc <- XML::xmlRoot(doc)
    querySelectorAll(doc, selector, ns, ...)
}

querySelectorNS.XMLInternalNode     <-
querySelectorNS.XMLInternalDocument <- function(doc, selector, ns,
                                                prefix = "descendant-or-self::", ...) {
    if (missing(ns) || ! length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.XMLInternalNode     <-
querySelectorAllNS.XMLInternalDocument <- function(doc, selector, ns,
                                                   prefix = "descendant-or-self::", ...) {
    if (missing(ns) || ! length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
}

querySelector.xml_node <- function(doc, selector, ns = NULL, ...) {
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    xpath <- css_to_xpath(selector, ...)
    result <- xml2::xml_find_first(doc, xpath, ns)
    if (length(result))
        result
    else
        NULL
}

querySelectorAll.xml_node <- function(doc, selector, ns = NULL, ...) {
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    xpath <- css_to_xpath(selector, ...)
    xml2::xml_find_all(doc, xpath, ns)
}

querySelectorNS.xml_node <- function(doc, selector, ns,
                                     prefix = "descendant-or-self::", ...) {
    if (missing(ns) || ! length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.xml_node <- function(doc, selector, ns,
                                        prefix = "descendant-or-self::", ...) {
    if (missing(ns) || ! length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
}

# Takes a named vector or list and gives a named vector back
formatNS <- function(ns) {
    nsNames <- names(ns)
    ns <- unlist(ns)
    names(ns) <- nsNames
    ns
}

formatNSPrefix <- function(ns, prefix) {
    filters <- paste0(sprintf("//%s:*", names(ns)), collapse = "|")
    prefix <- paste0("(", filters, ")/", prefix)
    prefix
}
