css_to_xpath <- function(selector, prefix = "descendant-or-self::", translator = "generic") {
    if (missing(selector) || is.null(selector))
        stop("A valid selector (character vector) must be provided.")
        
    if (!is.character(selector))
        stop("The 'selector' argument must be a character vector")
    if (!is.character(prefix))
        stop("The 'prefix' argument must be a character vector")
    if (!is.character(prefix))
        stop("The 'translator' argument must be a character vector")
            
    if (anyNA(selector)) {
        warning("NA values were found in the 'selector' argument, they have been removed")
        selector <- selector[!is.na(selector)]
    }

    if (anyNA(prefix)) {
        warning("NA values were found in the 'prefix' argument, they have been removed")
        prefix <- prefix[!is.na(prefix)]
    }
    
    if (anyNA(translator)) {
        warning("NA values were found in the 'translator' argument, they have been removed")
        translator <- translator[!is.na(translator)]
    }
    
    zeroLengthArgs <- character(0)
    if (!length(selector))
        zeroLengthArgs <- c(zeroLengthArgs, "selector")
    if (!length(prefix))
        zeroLengthArgs <- c(zeroLengthArgs, "prefix")
    if (!length(translator))
        zeroLengthArgs <- c(zeroLengthArgs, "translator")
    
    if (length(zeroLengthArgs)) {
        plural <- if (length(zeroLengthArgs) > 1) "s" else ""
        stop(sprintf("Zero length character vector found for the following argument%s: %s",
            plural, paste0(zeroLengthArgs, collapse = ",")))       
    }

    maxArgLength <- max(length(selector), length(prefix), length(translator))
    selector <- rep(selector, length.out = maxArgLength)
    prefix <- rep(prefix, length.out = maxArgLength)
    translator <- rep(translator, length.out = maxArgLength)

    results <- character(maxArgLength)
    for (i in seq_len(maxArgLength)) {
        sel <- selector[i]
        pref <- prefix[i]
        trans <- translator[i]
        
        tran <- if (trans == "html") {
            HTMLTranslator$new()
        } else if (trans == "xhtml") {
            HTMLTranslator$new(xhtml = TRUE)
        } else {
            GenericTranslator$new()
        }
        
        results[i] <- tran$css_to_xpath(sel, pref)
    }

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
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    results <- querySelectorAll(doc, selector, ns, ...)
    if (length(results))
        results[[1]]
    else
        NULL
}

querySelectorAll.XMLInternalNode <- function(doc, selector, ns = NULL, ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    xpath <- css_to_xpath(selector, ...)
    if (!is.null(ns)) {
        ns <- formatNS(ns)
        XML::getNodeSet(doc, xpath, ns)
    } else {
        XML::getNodeSet(doc, xpath)
    }
}

querySelectorAll.XMLInternalDocument <- function(doc, selector, ns = NULL, ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    doc <- XML::xmlRoot(doc)
    querySelectorAll(doc, selector, ns, ...)
}

querySelectorNS.XMLInternalNode     <-
querySelectorNS.XMLInternalDocument <- function(doc, selector, ns,
                                                prefix = "descendant-or-self::", ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")                                                
    if (missing(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.XMLInternalNode     <-
querySelectorAllNS.XMLInternalDocument <- function(doc, selector, ns,
                                                   prefix = "descendant-or-self::", ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    if (missing(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
}

querySelector.xml_node <- function(doc, selector, ns = NULL, ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    validateNS(ns)
    xpath <- css_to_xpath(selector, ...)
    result <- xml2::xml_find_first(doc, xpath, ns)
    if (length(result))
        result
    else
        NULL
}

querySelectorAll.xml_node <- function(doc, selector, ns = NULL, ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    if (is.null(ns))
        ns <- xml2::xml_ns(doc)
    validateNS(ns)
    xpath <- css_to_xpath(selector, ...)
    xml2::xml_find_all(doc, xpath, ns)
}

querySelectorNS.xml_node <- function(doc, selector, ns,
                                     prefix = "descendant-or-self::", ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelector(doc, selector, ns, prefix = prefix, ...)
}

querySelectorAllNS.xml_node <- function(doc, selector, ns,
                                        prefix = "descendant-or-self::", ...) {
    if (missing(selector))
        stop("A valid selector (character vector) must be provided.")
    if (missing(ns) || is.null(ns) || !length(ns))
        stop("A namespace must be provided.")
    ns <- formatNS(ns)
    prefix <- formatNSPrefix(ns, prefix)
    querySelectorAll(doc, selector, ns, prefix = prefix, ...)
}

# Takes a named vector or list and gives a named vector back
formatNS <- function(ns) {
    if (is.null(ns))
        return(NULL)
    if (!is.list(ns) && !is.character(ns))
        stop("A namespace object must be either a named list or a named character vector.")
    nsNames <- names(ns)
    if (is.null(nsNames) || anyNA(nsNames))
        stop("The namespace object either missing some or all names for each element in its collection.")
    ns <- unlist(ns)
    if (!is.character(ns))
        stop("The values in the namespace object must be a character vector.")
    names(ns) <- nsNames
    ns
}

formatNSPrefix <- function(ns, prefix) {
    filters <- paste0(sprintf("//%s:*", names(ns)), collapse = "|")
    prefix <- paste0("(", filters, ")/", prefix)
    prefix
}

# Checks whether a vector is a valid character vector for namespaces
validateNS <- function(ns) {
    if (!is.character(ns))
        stop("A namespace object must be comprised of characters")
    nsNames <- names(ns)
    if (is.null(nsNames) || anyNA(nsNames))
        stop("The namespace object either missing some or all names for each element in its collection.")
}
