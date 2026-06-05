escape <- paste0("\\\\([0-9a-fA-F]{1,6})(\r\n|[ \n\r\t\f])?",
                 "|\\\\[^\n\r\f0-9a-fA-F]")
nonascii <- "[^\1-\177]"

TokenMacros <- list(escape = escape,
                    string_escape = paste0("\\\\(?:\n|\r\n|\r|\f)|", escape),
                    nonascii = nonascii,
                    nmchar = paste0("([_a-z0-9-]|", escape, "|", nonascii, ")"),
                    nmstart = paste0("[_a-z]|", escape, "|", nonascii))

Selector <- R6Class("Selector",
    public = list(
        parsed_tree = NULL,
        pseudo_element = NULL,
        initialize = function(tree, pseudo_element = NULL) {
            self$parsed_tree <- tree
            if (!is.null(pseudo_element))
                self$pseudo_element <- tolower(pseudo_element)
        },
        repr = function() {
            pseudo_el <-
                if (is.null(self$pseudo_element)) ""
                else paste0("::", self$pseudo_element)
            paste0(self$parsed_tree$repr(), pseudo_el)
        },
        specificity = function() {
            specs <- self$parsed_tree$specificity()
            if (!is.null(self$pseudo_element))
                specs[3] <- specs[3] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

ClassSelector <- R6Class("ClassSelector",
    public = list(
        selector = NULL,
        class_name = NULL,
        initialize = function(selector, class_name) {
            self$selector <- selector
            self$class_name <- class_name
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ".",
                self$class_name,
                "]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Function <- R6Class("Function",
    public = list(
        selector = NULL,
        name = NULL,
        arguments = NULL,
        selector_list = NULL,
        initialize = function(selector, name, arguments, selector_list = NULL) {
            self$selector <- selector
            self$name <- tolower(name)
            self$arguments <- arguments
            self$selector_list <- selector_list
        },
        repr = function() {
            token_values <- lapply(self$arguments,
                function(token) paste0("'", token$value, "'"))
            token_values <- paste0(unlist(token_values), collapse = ", ")
            token_values <- paste0("[", token_values, "]")
            selector_list_repr <- ""
            if (!is.null(self$selector_list)) {
                selector_list_repr <- paste0(
                    " of ",
                    paste0(sapply(self$selector_list, function(s) s$repr()), collapse = ", ")
                )
            }
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":",
                self$name,
                "(",
                token_values,
                selector_list_repr,
                ")]")
        },
        argument_types = function() {
            token_types <- lapply(self$arguments, function(token) token$type)
            unlist(token_types)
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Pseudo <- R6Class("Pseudo",
    public = list(
        selector = NULL,
        ident = NULL,
        initialize = function(selector, ident) {
            self$selector <- selector
            self$ident <- tolower(ident)
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":",
                self$ident,
                "]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Negation <- R6Class("Negation",
    public = list(
        selector = NULL,
        selector_list = NULL,
        initialize = function(selector, selector_list) {
            self$selector <- selector
            self$selector_list <- selector_list
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":not(",
                paste0(
                    sapply(self$selector_list, function(s) s$repr()),
                    collapse = ", "
                ),
                ")]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            # according to CSS Selectors Level 4, :not() takes the specificity of
            # its most specific argument
            sub_specs <- sapply(self$selector_list, function(s) s$specificity())
            # sapply returns a matrix with each column being a selector's specificity
            if (is.matrix(sub_specs)) {
                # get rows as selectors
                sub_specs <- t(sub_specs)
                if (nrow(sub_specs) > 1) {
                    # sort by specificity (id, class, element) descending
                    sub_specs <- sub_specs[order(-sub_specs[, 1], -sub_specs[, 2], -sub_specs[, 3]), , drop = FALSE]
                }
                specs + sub_specs[1, ]
            } else {
                # single value case
                specs + sub_specs
            }
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Matching <- R6Class("Matching",
    public = list(
        selector = NULL,
        selector_list = NULL,
        initialize = function(selector, selector_list) {
            self$selector <- selector
            self$selector_list <- selector_list
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":is(",
                paste0(
                    sapply(self$selector_list, function(s) s$repr()),
                    collapse = ", "
                ),
                ")]"
            )
        },
        specificity = function() {
            # :is() takes the specificity of its most specific argument,
            # added to the base selector
            base_specs <- self$selector$specificity()
            sub_specs <- sapply(self$selector_list, function(s) s$specificity())
            # sapply returns a matrix with each column being a selector's specificity
            sub_specs <- t(sub_specs)
            if (nrow(sub_specs) > 1) {
                # sort by specificity (id, class, element) descending
                sub_specs <- sub_specs[order(-sub_specs[, 1], -sub_specs[, 2], -sub_specs[, 3]), , drop = FALSE]
            }
            base_specs + sub_specs[1, ]
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Where <- R6Class("Where",
    public = list(
        selector = NULL,
        selector_list = NULL,
        initialize = function(selector, selector_list) {
            self$selector <- selector
            self$selector_list <- selector_list
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":where(",
                paste0(
                    sapply(self$selector_list, function(s) s$repr()),
                    collapse = ", "
                ),
                ")]"
            )
        },
        specificity = function() {
            # :where() always has zero specificity
            self$selector$specificity()
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

# A :has() argument with an explicit leading combinator (selectors-4
# <relative-selector>): wraps the parsed selector alongside its combinator.
# Arguments with the omitted (implied descendant) combinator are stored
# unwrapped in Has$selector_list.
RelativeSelector <- R6Class("RelativeSelector",
    public = list(
        combinator = NULL,
        selector = NULL,
        initialize = function(combinator, selector) {
            self$combinator <- combinator
            self$selector <- selector
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$combinator,
                " ",
                self$selector$repr(),
                "]"
            )
        },
        specificity = function() {
            # The leading combinator contributes no specificity
            self$selector$specificity()
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Has <- R6Class("Has",
    public = list(
        selector = NULL,
        selector_list = NULL,
        initialize = function(selector, selector_list) {
            self$selector <- selector
            self$selector_list <- selector_list
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":has(",
                paste0(
                    sapply(self$selector_list, function(s) s$repr()),
                    collapse = ", "
                ),
                ")]"
            )
        },
        specificity = function() {
            # :has() takes the specificity of its most specific argument,
            # added to the base selector
            base_specs <- self$selector$specificity()
            sub_specs <- sapply(self$selector_list, function(s) s$specificity())
            # sapply returns a matrix with each column being a selector's specificity
            sub_specs <- t(sub_specs)
            if (nrow(sub_specs) > 1) {
                # sort by specificity (id, class, element) descending
                sub_specs <- sub_specs[order(-sub_specs[, 1], -sub_specs[, 2], -sub_specs[, 3]), , drop = FALSE]
            }
            base_specs + sub_specs[1, ]
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Attrib <- R6Class("Attrib",
    public = list(
        selector = NULL,
        namespace = NULL,
        attrib = NULL,
        operator = NULL,
        value = NULL,
        flag = NULL,
        initialize = function(selector, namespace, attrib, operator, value,
                              flag = NULL) {
            self$selector <- selector
            self$namespace <- namespace
            self$attrib <- attrib
            self$operator <- operator
            self$value <- value
            self$flag <- flag
        },
        repr = function() {
            attr <-
                if (!is.null(self$namespace))
                    paste0(self$namespace, "|", self$attrib)
                else
                    self$attrib
            if (self$operator == "exists")
                paste0(
                    first_class_name(self),
                    "[",
                    self$selector$repr(),
                    "[",
                    attr,
                    "]]")
            else
                paste0(
                    first_class_name(self),
                    "[",
                    self$selector$repr(),
                    "[",
                    attr,
                    " ",
                    self$operator,
                    " '",
                    self$value,
                    "'",
                    if (!is.null(self$flag)) paste0(" ", self$flag) else "",
                    "]]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Element <- R6Class("Element",
    public = list(
        namespace = NULL,
        element = NULL,
        initialize = function(namespace = NULL, element = NULL) {
            self$namespace <- namespace
            self$element <- element
        },
        repr = function() {
            el <-
                if (!is.null(self$element)) self$element
                else "*"
            if (!is.null(self$namespace))
                el <- paste0(self$namespace, "|", el)
            paste0(first_class_name(self), "[", el, "]")
        },
        specificity = function() {
            if (!is.null(self$element)) c(0, 0, 1)
            else rep(0, 3)
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Hash <- R6Class("Hash",
    public = list(
        selector = NULL,
        id = NULL,
        initialize = function(selector, id) {
            self$selector <- selector
            self$id <- id
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                "#",
                self$id,
                "]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[1] <- specs[1] + 1
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

CombinedSelector <- R6Class("CombinedSelector",
    public = list(
        selector = NULL,
        combinator = NULL,
        subselector = NULL,
        initialize = function(selector, combinator, subselector) {
            if (is.null(selector))
                stop("'selector' cannot be NULL")
            self$selector <- selector
            self$combinator <- combinator
            self$subselector <- subselector
        },
        repr = function() {
            comb <-
                if (self$combinator == " ") "<followed>"
                else self$combinator
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                " ",
                comb,
                " ",
                self$subselector$repr(),
                "]")
        },
        specificity = function() {
            specs <- self$selector$specificity()
            sub_specs <- self$subselector$specificity()
            specs + sub_specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

#### Parser

# Fast paths for the most common simple selectors, skipping
# tokenization. INVARIANT: each regex must accept only selectors that
# the full tokenize()/parse_selector_group() pipeline would parse to
# the same result; anything else falls through to the full parser.
# The name patterns are therefore conservative ASCII subsets of the
# tokenizer's identifier grammar (match_ident, sans escapes and
# non-ASCII) and hash grammar (match_hash).
fast_ident <- "[a-zA-Z][a-zA-Z0-9_-]*"
fast_name <- "[a-zA-Z0-9_-]+"

# foo
el_re <- paste0("^[ \t\r\n\f]*(", fast_ident, ")[ \t\r\n\f]*$")

# foo#bar or #bar
id_re <- paste0("^[ \t\r\n\f]*(", fast_ident, ")?",
                "#(", fast_name, ")[ \t\r\n\f]*$")

# foo.bar or .bar
class_re <- paste0("^[ \t\r\n\f]*(", fast_ident, ")?",
                   "\\.(", fast_ident, ")[ \t\r\n\f]*$")

parse <- function(css) {
    # regmatches() represents an unmatched optional group as "", which
    # cannot be confused with a present element name since fast_ident
    # never matches an empty string
    el_match <- regmatches(css, regexec(el_re, css))[[1]]
    if (length(el_match))
        return(list(Selector$new(Element$new(element = el_match[2]))))
    id_match <- regmatches(css, regexec(id_re, css))[[1]]
    if (length(id_match))
        return(list(Selector$new(
                        Hash$new(
                            Element$new(
                                element =
                                    if (nzchar(id_match[2])) id_match[2]
                                    else NULL),
                            id_match[3]))))
    class_match <- regmatches(css, regexec(class_re, css))[[1]]
    if (length(class_match))
        return(list(Selector$new(
                        ClassSelector$new(
                            Element$new(
                                element =
                                    if (nzchar(class_match[2])) class_match[2]
                                    else NULL),
                            class_match[3]))))
    stream <- TokenStream$new(tokenize(css))
    stream$source_text <- css
    parse_selector_group(stream)
}

parse_selector_group <- function(stream) {
    stream$skip_whitespace()
    i <- 1
    results <- list()
    while (TRUE) {
        parsed_selector <- parse_selector(stream)
        results[[i]] <- Selector$new(parsed_selector$result,
                                     parsed_selector$pseudo_element)
        i <- i + 1
        if (token_equality(stream$peek(), "DELIM", ",")) {
            stream$nxt()
            stream$skip_whitespace()
        } else {
            break
        }
    }
    results
}

token_equality <- function(token, t, val) {
    if (token$type != t)
        return(FALSE)
    # val or the token value can be NULL (e.g. for EOF tokens); they
    # are only equal when both are
    if (is.null(val) || is.null(token$value))
        return(is.null(val) && is.null(token$value))
    isTRUE(token$value == val)
}

parse_selector <- function(stream) {
    results <- parse_simple_selector(stream)
    result <- results$result
    pseudo_element <- results$pseudo_element

    while (TRUE) {
        stream$skip_whitespace()
        peek <- stream$peek()
        if (token_equality(peek, "EOF", NULL) ||
            token_equality(peek, "DELIM", ",")) {
            break
        }
        if (!is.null(pseudo_element) && nzchar(pseudo_element)) {
          stop("Got pseudo-element ::",
               pseudo_element,
               " not at the end of a selector")
        }
        if (token_is_delim(peek, c("+", ">", "~"))) {
            # A combinator
            combinator <- stream$nxt()$value
            stream$skip_whitespace()
        } else {
            # By exclusion, the last parse_simple_selector() ended
            # at peek == ' '
            combinator <- " "
        }
        stuff <- parse_simple_selector(stream)
        pseudo_element <- stuff$pseudo_element
        result <- CombinedSelector$new(result, combinator, stuff$result)
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_simple_selector <- function(stream, inside_arguments = FALSE,
                                  inside_has = FALSE) {
    stream$skip_whitespace()
    selector_start <- length(stream$used)
    peek <- stream$peek()
    if (peek$type == "IDENT" || token_equality(peek, "DELIM", "*") ||
        token_equality(peek, "DELIM", "|")) {
        if (peek$type == "IDENT") {
            namespace <- stream$nxt()$value
        } else if (token_equality(peek, "DELIM", "*")) {
            stream$nxt()
            # '*|e': any namespace, including none
            namespace <- "*"
        } else {
            # Leading '|', i.e. '|e' or '|*': explicitly no namespace
            namespace <- ""
        }
        if (token_equality(stream$peek(), "DELIM", "|")) {
            stream$nxt()
            # A second '|' makes this the Selectors 4 column
            # combinator ('a || b' and namespaceless '||b' arrive
            # here alike): column membership depends on table-layout
            # arithmetic (colspan/rowspan carry-over) that XPath 1.0
            # cannot express, so name the construct instead of
            # falling through to a stray-token error
            if (token_equality(stream$peek(), "DELIM", "|"))
                stop("The column combinator '||' is not supported")
            element <- stream$next_ident_or_star()
        } else {
            element <- if (identical(namespace, "*")) NULL else namespace
            namespace <- NULL
        }
    } else {
        element <- namespace <- NULL
    }
    result <- Element$new(namespace, element)
    pseudo_element <- NULL
    while (TRUE) {
        peek <- stream$peek()
        if (any(peek$type == c("S", "EOF")) ||
            token_is_delim(peek, c(",", "+", ">", "~")) ||
            (inside_arguments && token_equality(peek, "DELIM", ")"))) {
            break
        }
        if (!is.null(pseudo_element)) {
            stop("Got pseudo-element ::",
                 pseudo_element,
                 " not at the end of a selector")
        }
        if (peek$type == "HASH") {
            result <- Hash$new(result, stream$nxt()$value)
        } else if (token_equality(peek, "DELIM", ".")) {
            stream$nxt()
            result <- ClassSelector$new(result, stream$next_ident())
        } else if (token_equality(peek, "DELIM", "[")) {
            stream$nxt()
            result <- parse_attrib(result, stream)
        } else if (token_equality(peek, "DELIM", ":") ||
                   token_equality(peek, "DELIM", "::")) {
            if (token_equality(peek, "DELIM", "::")) {
                stream$nxt()
                pseudo_element <- stream$next_ident()
                next
            } else {
                stream$nxt()
            }
            ident <- stream$next_ident()
            if (tolower(ident) %in% c(
                "first-line", "first-letter", "before", "after")) {
                # Special case: CSS 2.1 pseudo-elements can have a single ':'
                # Any new pseudo-element must have two.
                pseudo_element <- ident
                next
            }
            if (!token_equality(stream$peek(), "DELIM", "(")) {
                result <- Pseudo$new(result, ident)
                next
            }
            stream$nxt()
            stream$skip_whitespace()
            if (tolower(ident) == "not") {
                # Selectors Level 4 places no nesting restriction on
                # :not(), so :not(:not(a)), :is(:not(a)), etc. are valid.
                selectors <- parse_simple_selector_arguments(stream, "not",
                                                             inside_has = inside_has)
                result <- Negation$new(result, selectors)
            } else if (any(tolower(ident) == c("matches", "is"))) {
                selectors <- parse_simple_selector_arguments(stream, tolower(ident),
                                                             inside_has = inside_has)
                result <- Matching$new(result, selectors)
            } else if (tolower(ident) == "where") {
                selectors <- parse_simple_selector_arguments(stream, "where",
                                                             inside_has = inside_has)
                result <- Where$new(result, selectors)
            } else if (tolower(ident) == "has") {
                # The :has() argument grammar excludes :has() at any
                # depth (selectors-4): "nesting :has() is not allowed"
                if (inside_has) {
                    stop("Got nested :has()")
                }
                selectors <- parse_simple_selector_arguments(stream, "has",
                                                             inside_has = TRUE,
                                                             relative = TRUE)
                result <- Has$new(result, selectors)
            } else {
                arguments <- list()
                selector_list <- NULL
                i <- 1

                # Parse the function arguments (e.g., "2n+1" for nth-child)
                # :lang() can accept a comma-separated list; :dir() takes
                # exactly one identifier (CSS Selectors Level 4)
                allow_commas <- tolower(ident) == "lang"

                while (TRUE) {
                    nt <- stream$nxt()
                    if (nt$type %in% c("IDENT", "STRING", "NUMBER") ||
                        (token_equality(nt, "DELIM", "+") ||
                         token_equality(nt, "DELIM", "-"))) {
                        arguments[[i]] <- nt
                        i <- i + 1

                        # Check if this is the 'of' keyword for nth-child/nth-last-child
                        if (nt$type == "IDENT" && tolower(nt$value) == "of" &&
                            any(tolower(ident) == c("nth-child", "nth-last-child"))) {
                            # Remove 'of' from arguments - it's a keyword, not an argument
                            arguments <- arguments[-length(arguments)]

                            # Parse the selector list that follows 'of'
                            stream$skip_whitespace()
                            selector_list <- parse_simple_selector_arguments(stream, ident,
                                                                             inside_has = inside_has)
                            break
                        }
                    } else if (token_equality(nt, "DELIM", "*") && allow_commas) {
                        # For :lang(), allow * as a wildcard
                        arguments[[i]] <- nt
                        i <- i + 1
                    } else if (nt$type == "S") {
                        # Keep whitespace tokens for the An+B (nth-*)
                        # functions so parse_series() can validate
                        # whitespace placement; other functions simply
                        # skip whitespace.
                        if (startsWith(tolower(ident), "nth-")) {
                            arguments[[i]] <- nt
                            i <- i + 1
                        }
                        next
                    } else if (token_equality(nt, "DELIM", ",") && allow_commas) {
                        # For :lang(), commas separate multiple values
                        stream$skip_whitespace()
                        next
                    } else if (token_equality(nt, "DELIM", ")") ||
                               nt$type == "EOF") {
                        # EOF auto-closes the function (css-syntax):
                        # ':lang(fr' means ':lang(fr)'
                        break
                    } else {
                        stop("Expected an argument, got ", token_repr(nt))
                    }
                }

                if (length(arguments) == 0) {
                    stop("Expected at least one argument, got ", token_repr(nt))
                }

                result <- Function$new(result, ident, arguments, selector_list)
            }
        } else {
            stop("Expected selector, got ", token_repr(stream$peek()))
        }
    }
    if (length(stream$used) == selector_start) {
        stop("Expected selector, got ", token_repr(stream$peek()))
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_simple_selector_arguments <- function(stream, function_name = NULL, # nolint: object_length_linter.
                                            inside_has = FALSE,
                                            relative = FALSE) {
    index <- 1
    arguments <- list()

    check_no_pseudo_element <- function(pseudo_element) {
        if (!is.null(pseudo_element)) {
            if (!is.null(function_name)) {
                stop("Got pseudo-element ::", pseudo_element,
                     " inside :", function_name,
                     "() at ", stream$peek()$pos)
            } else {
                stop("Got pseudo-element ::", pseudo_element, " inside function")
            }
        }
    }

    while (TRUE) {
        combinator <- NULL
        if (relative) {
            # :has() takes a <relative-selector-list> (selectors-4
            # section 17): each argument may begin with an explicit
            # combinator; the omitted combinator means descendant
            stream$skip_whitespace()
            peek <- stream$peek()
            if (token_is_delim(peek, c(">", "~", "+"))) {
                combinator <- stream$nxt()$value
            }
        }
        results <- parse_simple_selector(stream, inside_arguments = TRUE,
                                         inside_has = inside_has)
        result <- results$result
        check_no_pseudo_element(results$pseudo_element)

        # Arguments are complex selectors (selectors-4): consume any
        # combinator chain following the compound, as parse_selector()
        # does at the top level
        while (TRUE) {
            peek <- stream$peek()
            if (peek$type == "S") {
                stream$skip_whitespace()
                peek <- stream$peek()
                if (token_is_delim(peek, c(")", ","))) {
                    break
                }
                if (token_is_delim(peek, c("+", ">", "~"))) {
                    chain_combinator <- stream$nxt()$value
                } else {
                    # The whitespace was a descendant combinator
                    chain_combinator <- " "
                }
            } else if (token_is_delim(peek, c("+", ">", "~"))) {
                chain_combinator <- stream$nxt()$value
            } else {
                # ')', ',' or EOF: leave for the argument-list logic below
                break
            }
            stuff <- parse_simple_selector(stream, inside_arguments = TRUE,
                                           inside_has = inside_has)
            check_no_pseudo_element(stuff$pseudo_element)
            result <- CombinedSelector$new(result, chain_combinator,
                                           stuff$result)
        }

        if (!is.null(combinator)) {
            result <- RelativeSelector$new(combinator, result)
        }
        arguments[[index]] <- result
        index <- index + 1

        stream$skip_whitespace()
        nt <- stream$nxt()

        if (token_equality(nt, "DELIM", ")") || nt$type == "EOF") {
            # EOF auto-closes the function (css-syntax):
            # ':is(a' means ':is(a)'
            break
        } else if (token_equality(nt, "DELIM", ",")) {
            stream$skip_whitespace()
            # Check if there's actually a selector after the comma
            peek <- stream$peek()
            if (token_equality(peek, "DELIM", ")")) {
                # Trailing comma before closing paren
                stop("Expected ')', got ", token_repr(nt))
            }
            # Continue to parse next selector
        } else {
            stop("Expected an argument, got ", token_repr(nt))
        }
    }

    arguments
}

parse_attrib <- function(selector, stream) {
    stream$skip_whitespace()
    if (token_equality(stream$peek(), "DELIM", "|")) {
        # '[|attr]': explicitly no namespace, equivalent to '[attr]'
        # because unprefixed attribute names have no namespace
        stream$nxt()
        attrib <- stream$next_ident()
        namespace <- op <- NULL
    } else {
        attrib <- stream$next_ident_or_star()
        if (is.null(attrib) && !token_equality(stream$peek(), "DELIM", "|"))
            stop("Expected '|', got ", token_repr(stream$peek()))
        if (token_equality(stream$peek(), "DELIM", "|")) {
            stream$nxt()
            # next_ident_or_star() returns NULL for '*', i.e. '[*|attr]'
            namespace <- if (is.null(attrib)) "*" else attrib
            attrib <- stream$next_ident()
            op <- NULL
        } else if (token_equality(stream$peek(), "DELIM", "|=")) {
            namespace <- NULL
            stream$nxt()
            op <- "|="
        } else {
            namespace <- op <- NULL
        }
    }
    if (is.null(op)) {
        stream$skip_whitespace()
        nt <- stream$nxt()
        # EOF auto-closes the block (css-syntax), here and below:
        # '[rel' means '[rel]'. Anything else before the ']' is still
        # an error
        if (token_equality(nt, "DELIM", "]") || nt$type == "EOF") {
            return(Attrib$new(selector, namespace, attrib, "exists", NULL))
        } else if (token_equality(nt, "DELIM", "=")) {
            op <- "="
        } else if (token_is_delim(nt, c("^=", "$=", "*=", "~=", "|="))) {
            op <- nt$value
        } else {
            stop("Operator expected, got ", token_repr(nt))
        }
    }
    stream$skip_whitespace()
    value <- stream$nxt()
    if (!value$type %in% c("IDENT", "STRING")) {
        stop("Expected string or ident, got ", token_repr(value))
    }
    stream$skip_whitespace()
    nt <- stream$nxt()
    # CSS Selectors Level 4 allows an optional case-sensitivity flag
    # before the closing bracket, e.g. '[attr="value" i]'
    flag <- NULL
    if (nt$type == "IDENT" && tolower(nt$value) %in% c("i", "s")) {
        flag <- tolower(nt$value)
        stream$skip_whitespace()
        nt <- stream$nxt()
    }
    if (!token_equality(nt, "DELIM", "]") && nt$type != "EOF") {
        stop("Expected ']', got ", token_repr(nt))
    }
    Attrib$new(selector, namespace, attrib, op, value$value, flag)
}

str_int <- function(s) {
    # An+B takes <integer> values only (css-syntax-3), so reject
    # anything as.integer() would otherwise coerce through double
    # and truncate, e.g. "2.5" -> 2L or "2e1" -> 20L.
    if (!grepl("^[+-]?[0-9]+$", s))
        return(NA_integer_)
    suppressWarnings(as.integer(s))
}

parse_series <- function(tokens) {
    for (token in tokens) {
        if (token$type == "STRING")
            stop("String tokens not allowed in series.")
    }
    s <- paste0(sapply(tokens, function(x) x$value), collapse = "")
    # The An+B microsyntax is ASCII case-insensitive (css-syntax-3),
    # e.g. "2N", "ODD", "EVEN". chartr() rather than tolower() so the
    # mapping is locale-independent; "nodev" covers every letter that
    # can appear in a valid series.
    s <- chartr("NODEV", "nodev", s)
    # Validate against the An+B grammar (css-syntax-3 section 6):
    # whitespace is permitted only around the +/- sign that separates
    # the B value (e.g. "2n + 1"), never inside or between the other
    # components ("3 7", "2 n", "- n" are all invalid).
    anb <- paste0("^[ \t\r\n\f]*",
                  "(odd|even|[+-]?[0-9]+|",
                  "[+-]?[0-9]*n([ \t\r\n\f]*[+-][ \t\r\n\f]*[0-9]+)?)",
                  "[ \t\r\n\f]*$")
    if (!grepl(anb, s))
        return(NULL)
    s <- gsub("[ \t\r\n\f]+", "", s)
    if (s == "odd")
        return(2:1)
    else if (s == "even")
        return(c(2, 0))
    else if (s == "n")
        return(1:0)
    n_pos <- regexpr("n", s, fixed = TRUE)
    if (n_pos == -1L) {
        result <- str_int(s)
        if (is.na(result)) {
            return(NULL)
        } else {
            return(c(0, result))
        }
    }
    # Split at the first 'n' only
    a <- trimws(substring(s, 1, n_pos - 1))
    b <- trimws(substring(s, n_pos + 1))

    intb <- str_int(b)
    if (!nzchar(a) && is.na(intb))
        return(NULL)

    if (!nzchar(a))
        a <- 1
    else if (a == "-" || a == "+")
        a <- str_int(paste0(a, "1"))
    else
        a <- str_int(a)
    if (!nzchar(b))
        b <- 0
    else
        b <- str_int(b)
    c(a, b)
}

# Tokens are created in bulk by tokenize() and used as plain records,
# so they are ordinary lists rather than R6 objects (an environment and
# class attribute per token is significant overhead at that volume).
Token <- function(type = "", value = NULL, pos = 1) {
    list(type = type, value = value, pos = pos)
}

EOFToken <- function(pos = 1) {
    list(type = "EOF", value = NULL, pos = pos)
}

token_repr <- function(token) {
    if (token$type == "EOF")
        paste0("<EOF at ", token$pos, ">")
    else
        paste0("<", token$type, " '", token$value, "' at ", token$pos, ">")
}

token_is_delim <- function(token, values) {
    token$type == "DELIM" && token$value %in% values
}

compile_ <- function(pattern) {
    function(x) {
        m <- regexpr(pattern, x, perl = TRUE)
        if (m == -1L)
            c(NA_integer_, NA_integer_)
        else
            c(m, m + attr(m, "match.length") - 1L)
    }
}

delims_2ch <- c("~=", "|=", "^=", "$=", "*=", "::")
delims_1ch <- c(">", "+", "~", ",", ".", "*", "=", "[", "]", "(", ")", "|", ":", "#")
delim_escapes <- paste0("\\", delims_1ch, collapse = "|")
match_whitespace <- compile_("^[ \t\r\n\f]+")
match_number <- compile_("^[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)")
# The escape alternative covers both unicode escapes (e.g. '\31 ') and
# simple escapes of any non-hex character, which includes all delimiters
match_hash <- compile_(paste0("^#([_a-zA-Z0-9-]|", nonascii, "|", escape, ")+"))
match_ident <- compile_(paste0("^([_a-zA-Z0-9-]|", nonascii, "|", escape, ")+"))
# String content: any character except a newline, backslash, or the
# quote character, or an escape sequence. Anchored so the match end
# gives the content length; the closing quote must follow immediately.
match_string_by_quote <- list("'" = compile_(paste0("^([^\n\r\f\\\\']|", TokenMacros$string_escape, ")*")),
                              '"' = compile_(paste0('^([^\n\r\f\\\\"]|', TokenMacros$string_escape, ")*")))

# Decode a token's escape sequences in one left-to-right pass
# (css-syntax-3 "consume an escaped code point"): each backslash
# consumes either 1-6 hex digits plus one optional whitespace (a
# unicode escape, e.g. '\31 ' is U+0031, i.e. '1'), an escaped newline
# (a line continuation, strings only), or exactly one literal
# character. The single non-overlapping global match claims each
# sequence's characters, so the text consumed by one escape (e.g. the
# tail of an escaped backslash '\\') is never re-read as the start of
# another, as sequential substitution passes would do.
decode_escapes <- function(x, newlines = FALSE) {
    pattern <- paste0("\\\\[0-9a-fA-F]{1,6}(?:\r\n|[ \n\r\t\f])?",
                      if (newlines) "|\\\\(?:\r\n|[\n\r\f])",
                      "|\\\\.")
    m <- gregexpr(pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(esc) {
        if (length(esc) == 0) {
            return(esc)
        }
        is_hex <- grepl("^\\\\[0-9a-fA-F]", esc)
        out <- substring(esc, 2)              # simple escape: the character
        out[grepl("^[\n\r\f]", out)] <- ""    # line continuation: nothing
        if (any(is_hex)) {
            hex <- sub("(?:\r\n|[ \n\r\t\f])$", "", out[is_hex], perl = TRUE)
            out[is_hex] <- intToUtf8(strtoi(hex, base = 16L), multiple = TRUE)
        }
        out
    })
    x
}

tokenize <- function(s) {
    pos <- 1
    i <- 1
    len_s <- nchar(s)
    results <- list()
    while (pos <= len_s) {
        ss <- substring(s, pos, len_s)
        match <- match_whitespace(ss)
        if (!anyNA(match) && match[1] == 1) {
            results[[i]] <- Token("S", " ", pos)
            match_end <- match[2]
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_number(ss)
        if (!anyNA(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            results[[i]] <- Token("NUMBER", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_ident(ss)
        if (!anyNA(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            value <- decode_escapes(value)
            results[[i]] <- Token("IDENT", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_hash(ss)
        if (!anyNA(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            value <- decode_escapes(value)
            hash_id <- substring(value, 2)
            results[[i]] <- Token("HASH", hash_id, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        # Testing presence of a two char delim at the current position
        two_ch <- substring(s, pos, pos + 1)
        if (two_ch %in% delims_2ch) {
            results[[i]] <- Token("DELIM", two_ch, pos)
            pos <- pos + 2
            i <- i + 1
            next
        }

        # Testing presence of a single char delim at the current position
        ch <- substring(s, pos, pos)
        if (ch %in% delims_1ch) {
            results[[i]] <- Token("DELIM", ch, pos)
            pos <- pos + 1
            i <- i + 1
            next
        }
        if (ch %in% c("'", '"')) {
            # Match the string content after the opening quote; the
            # closing quote must follow immediately
            match <- match_string_by_quote[[ch]](substring(s, pos + 1, len_s))
            content_end <- if (anyNA(match)) 0 else match[2]
            end_quote <- pos + 1 + content_end
            # A string still open at EOF (content consumed to the end
            # of the input) is auto-closed with the consumed value, as
            # css-syntax requires; only a string stopped short of EOF
            # (by a raw newline or a lone trailing backslash) is an
            # error
            if (end_quote <= len_s &&
                substring(s, end_quote, end_quote) != ch) {
                stop("Unclosed string at ", pos)
            }
            value <- substring(s, pos + 1, pos + content_end)
            value <- decode_escapes(value, newlines = TRUE)
            results[[i]] <- Token("STRING", value, pos)
            # An auto-closed string has no closing quote to step over,
            # so the EOF token keeps its position just past the input
            pos <- min(end_quote, len_s) + 1
            i <- i + 1
            next
        }
        # Remove comments
        if (two_ch == "/*") {
            rel_pos <- regexpr("*/", ss, fixed = TRUE)
            pos <-
                if (rel_pos == -1L) {
                    len_s + 1
                } else {
                    pos + rel_pos + 1
                }
            next
        }
        # Every successful match ends in 'next', so reaching here means
        # the character cannot start any token
        stop("Unexpected character '",
             ch,
             "' found at position ",
             pos)
    }
    results[[i]] <- EOFToken(pos)
    results
}

TokenStream <- R6Class("TokenStream",
    public = list(
        pos = 1,
        tokens = NULL,
        ntokens = 0,
        used = list(),
        source_text = NULL,
        peeked = list(),
        peeking = FALSE,
        initialize = function(tokens, source_text = NULL) {
            self$tokens <- tokens
            self$ntokens <- length(tokens)
            self$source_text <- source_text
        },
        nxt = function() {
            if (self$peeking) {
                self$peeking <- FALSE
                self$used[[self$pos]] <- self$peeked
                self$peeked
            } else {
                nt <- self$next_token()
                self$used[[self$pos]] <- nt
                nt
            }
        },
        next_token = function() {
            if (self$pos > self$ntokens) {
                # The trailing EOF token is sticky: consuming it
                # (e.g. when it auto-closes a construct) must not run
                # past the token list, as the caller will peek again
                self$tokens[[self$ntokens]]
            } else {
                nt <- self$tokens[[self$pos]]
                self$pos <- self$pos + 1
                nt
            }
        },
        peek = function() {
            if (!self$peeking) {
                self$peeked <- self$next_token()
                self$peeking <- TRUE
            }
            self$peeked
        },
        next_ident = function() {
            nt <- self$nxt()
            if (nt$type != "IDENT")
                stop("Expected ident, got ", token_repr(nt))
            nt$value
        },
        next_ident_or_star = function() {
            nt <- self$nxt()
            if (nt$type == "IDENT")
                nt$value
            else if (token_equality(nt, "DELIM", "*"))
                NULL
            else
                stop("Expected ident or '*', got ", token_repr(nt))
        },
        skip_whitespace = function() {
            peek <- self$peek()
            if (peek$type == "S")
                self$nxt()
        }
    )
)
