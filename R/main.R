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
    results <- querySelectorAll(doc, selector, ns, ...)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelectorAll <- function(doc, selector, ns = NULL, ...) {
    if (inherits(doc, "XMLInternalDocument"))
        doc <- xmlRoot(doc)
    xpath <- css_to_xpath(selector, ...)
    if (! is.null(ns)) {
        getNodeSet(doc, xpath, ns)
    } else {
        getNodeSet(doc, xpath)
    }
}

querySelectorNS <- function(doc, selector, ns,
                            prefix = "descendant-or-self::", ...) {
    if (missing(ns) || ! length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS <- function(doc, selector, ns,
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
