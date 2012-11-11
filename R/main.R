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

querySelectorNS <- function(doc, selector, ns = NULL,
                            prefix = "descendant-or-self::", ...) {
    if (! length(ns) || length(ns) > 1)
        stop("A single namespace must be provided.")
    # Handle both names vectors and lists
    nsNames <- names(ns)
    ns <- unlist(ns)
    names(ns) <- nsNames
    # Call qs
    querySelector(doc, selector, ns,
                  prefix = sprintf(paste0("//%s:*/", prefix), names(ns)), ...)
}

querySelectorAllNS <- function(doc, selector, ns = NULL,
                               prefix = "descendant-or-self::", ...) {
    if (! length(ns) || length(ns) > 1)
        stop("A single namespace must be provided.")
    # Handle both names vectors and lists
    nsNames <- names(ns)
    ns <- unlist(ns)
    names(ns) <- nsNames
    # Call qs
    querySelectorAll(doc, selector, ns,
                     prefix = sprintf(paste0("//%s:*/", prefix), names(ns)), ...)
}
