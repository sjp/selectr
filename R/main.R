css_to_xpath <- function(selector, prefix = "descendant-or-self::", translator = "generic") {
    ns <- length(selector)
    np <- length(prefix)
    nt <- length(translator)
    n <- max(ns, np, nt)
    selector <- rep(selector, length.out = n)
    prefix <- rep(prefix, length.out = n)
    translator <- rep(translator, length.out = n)
    results <- character(n)
    for (i in 1:n) {
        tran <-
            if (translator[i] == "html") {
                HTMLTranslator$new()
            } else if (translator[i] == "xhtml") {
                HTMLTranslator$new(xhtml = TRUE)
            } else {
                GenericTranslator$new()
            }
        results[i] <- tran$css_to_xpath(selector[i], prefix = prefix[i])
    }
    results
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
