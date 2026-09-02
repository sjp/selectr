escape <- paste0("\\\\([0-9a-fA-F]{1,6})(\r\n|[ \n\r\t\f])?",
                 "|\\\\[^\n\r\f0-9a-fA-F]",
                 # css-syntax-3: a backslash immediately followed by EOF is
                 # still a valid escape (only a following newline is not);
                 # \z (not $) so a backslash before a final trailing
                 # newline isn't mistaken for one at true end of input
                 "|\\\\\\z")
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
            # added to the base selector; an empty argument list
            # contributes nothing
            base_specs <- self$selector$specificity()
            if (length(self$selector_list) == 0)
                return(base_specs)
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
            spine <- combinator_spine(self)
            out <- spine$leftmost$repr()
            for (node in spine$nodes) {
                comb <-
                    if (node$combinator == " ") "<followed>"
                    else node$combinator
                out <- paste0(first_class_name(node), "[", out, " ", comb,
                              " ", node$subselector$repr(), "]")
            }
            out
        },
        specificity = function() {
            spine <- combinator_spine(self)
            specs <- spine$leftmost$specificity()
            for (node in spine$nodes)
                specs <- specs + node$subselector$specificity()
            specs
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

#### Parser

# The parser builds a left-deep CombinedSelector tree, so 'a > b > c'
# is ((a > b) > c). Split such a tree into the compound selector at the
# far left and the CombinedSelector nodes above it, ordered
# left-to-right, so that callers can fold the chain in a loop. Walking
# the spine recursively instead would cost several R frames per
# combinator, and long chains would exhaust R's expression nesting
# limit (options(expressions=)) rather than translate.
combinator_spine <- function(selector) {
    spine <- list()
    while (first_class_name(selector) == "CombinedSelector") {
        spine[[length(spine) + 1L]] <- selector
        selector <- selector$selector
    }
    list(leftmost = selector, nodes = rev(spine))
}

# Fast paths for the most common simple selectors, skipping
# tokenization. INVARIANT: each regex must accept only selectors that
# the full tokenize()/parse_selector_group() pipeline would parse to
# the same result; anything else falls through to the full parser.
# The name patterns are therefore conservative ASCII subsets of the
# tokenizer's identifier grammar (match_ident, sans escapes and
# non-ASCII) and hash grammar (match_hash).
fast_ident <- "[a-zA-Z][a-zA-Z0-9_-]*"
# An ID must be identifier-shaped, so it cannot start with a digit nor
# with '-' followed by a digit (see match_ident_start)
fast_id <- "[a-zA-Z_][a-zA-Z0-9_-]*|-[a-zA-Z_-][a-zA-Z0-9_-]*"

# foo
el_re <- paste0("^[ \t\r\n\f]*(", fast_ident, ")[ \t\r\n\f]*$")

# foo#bar or #bar
id_re <- paste0("^[ \t\r\n\f]*(", fast_ident, ")?",
                "#(", fast_id, ")[ \t\r\n\f]*$")

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
    tryCatch(
        {
            stream <- TokenStream$new(tokenize(css))
            stream$source_text <- css
            parse_selector_group(stream)
        },
        selectr_parse_error = function(e) {
            # Re-signal at the parse() boundary so the message gains the
            # source-pointer gutter, but keep the condition class and the
            # machine-readable fields so callers can handle it structurally.
            stop(structure(
                class = c("selectr_parse_error", "selectr_error",
                          "error", "condition"),
                list(message = format_parse_error(conditionMessage(e),
                                                  css, e$pos),
                     call = NULL,
                     pos = e$pos,
                     selector = css)
            ))
        }
    )
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
          parse_stop("Got pseudo-element ::",
                     pseudo_element,
                     " not at the end of a selector",
                     pos = peek$pos)
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
    selector_start <- stream$consumed
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
                parse_stop("The column combinator '||' is not supported",
                           pos = stream$peek()$pos)
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
            parse_stop("Got pseudo-element ::",
                       pseudo_element,
                       " not at the end of a selector",
                       pos = peek$pos)
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
                # :is()/:matches() take a <forgiving-selector-list>, so
                # an empty argument list is valid (it matches nothing)
                selectors <- parse_simple_selector_arguments(stream, tolower(ident),
                                                             inside_has = inside_has,
                                                             forgiving = TRUE)
                result <- Matching$new(result, selectors)
            } else if (tolower(ident) == "where") {
                selectors <- parse_simple_selector_arguments(stream, "where",
                                                             inside_has = inside_has,
                                                             forgiving = TRUE)
                result <- Where$new(result, selectors)
            } else if (tolower(ident) == "has") {
                # The :has() argument grammar excludes :has() at any
                # depth (selectors-4): "nesting :has() is not allowed"
                if (inside_has) {
                    parse_stop("Got nested :has()", pos = stream$peek()$pos)
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
                # has_arg/ws_since_arg track, within the current
                # comma-delimited :lang() value, whether an argument
                # token has been seen and whether whitespace followed
                # it. A wildcard range can span several adjacent
                # tokens with no whitespace between them (e.g. the
                # '*', '-CH' of "*-CH", or the "de-", '*', '-DE' of
                # "de-*-DE") -- those must NOT require a comma. Only
                # whitespace standing in for a comma is rejected.
                has_arg <- FALSE
                ws_since_arg <- FALSE

                while (TRUE) {
                    nt <- stream$nxt()
                    if (nt$type %in% c("IDENT", "STRING", "NUMBER") ||
                        (token_equality(nt, "DELIM", "+") ||
                         token_equality(nt, "DELIM", "-"))) {
                        if (allow_commas && has_arg && ws_since_arg) {
                            parse_stop("Expected ',' or ')', got ",
                                       token_repr(nt), pos = nt$pos)
                        }
                        arguments[[i]] <- nt
                        i <- i + 1
                        has_arg <- TRUE
                        ws_since_arg <- FALSE

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
                        if (has_arg && ws_since_arg) {
                            parse_stop("Expected ',' or ')', got ",
                                       token_repr(nt), pos = nt$pos)
                        }
                        arguments[[i]] <- nt
                        i <- i + 1
                        has_arg <- TRUE
                        ws_since_arg <- FALSE
                    } else if (nt$type == "S") {
                        # Keep whitespace tokens for the An+B (nth-*)
                        # functions so parse_series() can validate
                        # whitespace placement; other functions simply
                        # skip whitespace.
                        if (startsWith(tolower(ident), "nth-")) {
                            arguments[[i]] <- nt
                            i <- i + 1
                        }
                        ws_since_arg <- TRUE
                        next
                    } else if (token_equality(nt, "DELIM", ",") && allow_commas) {
                        # For :lang(), commas separate multiple values
                        stream$skip_whitespace()
                        has_arg <- FALSE
                        ws_since_arg <- FALSE
                        next
                    } else if (token_equality(nt, "DELIM", ")") ||
                               nt$type == "EOF") {
                        # EOF auto-closes the function (css-syntax):
                        # ':lang(fr' means ':lang(fr)'
                        break
                    } else {
                        parse_stop("Expected an argument, got ",
                                   token_repr(nt), pos = nt$pos)
                    }
                }

                if (length(arguments) == 0) {
                    parse_stop("Expected at least one argument, got ",
                               token_repr(nt), pos = nt$pos)
                }

                if (tolower(ident) %in% anb_function_names)
                    validate_series(arguments, ident)

                result <- Function$new(result, ident, arguments, selector_list)
            }
        } else {
            parse_stop("Expected selector, got ", token_repr(stream$peek()),
                       pos = stream$peek()$pos)
        }
    }
    if (stream$consumed == selector_start) {
        parse_stop("Expected selector, got ", token_repr(stream$peek()),
                   pos = stream$peek()$pos)
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_simple_selector_arguments <- function(stream, function_name = NULL, # nolint: object_length_linter.
                                            inside_has = FALSE,
                                            relative = FALSE,
                                            forgiving = FALSE) {
    index <- 1
    arguments <- list()

    if (forgiving) {
        # A <forgiving-selector-list> (:is(), :where()) may be empty:
        # ':is()' is valid and matches nothing. EOF auto-closes the
        # function as elsewhere, so ':is(' is the same as ':is()'
        peek <- stream$peek()
        if (token_equality(peek, "DELIM", ")")) {
            stream$nxt()
            return(arguments)
        } else if (peek$type == "EOF") {
            return(arguments)
        }
    }

    check_no_pseudo_element <- function(pseudo_element) {
        if (!is.null(pseudo_element)) {
            if (!is.null(function_name)) {
                parse_stop("Got pseudo-element ::", pseudo_element,
                           " inside :", function_name,
                           "() at ", stream$peek()$pos,
                           pos = stream$peek()$pos)
            } else {
                parse_stop("Got pseudo-element ::", pseudo_element,
                           " inside function",
                           pos = stream$peek()$pos)
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
                # Trailing comma: the ',' promised another selector, so
                # point at the ')' that arrived instead of it
                parse_stop("Expected selector after ',', got ",
                           token_repr(peek), pos = peek$pos)
            }
            # Continue to parse next selector
        } else {
            parse_stop("Expected an argument, got ", token_repr(nt), pos = nt$pos)
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
            parse_stop("Expected '|', got ", token_repr(stream$peek()),
                       pos = stream$peek()$pos)
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
            parse_stop("Operator expected, got ", token_repr(nt), pos = nt$pos)
        }
    }
    stream$skip_whitespace()
    value <- stream$nxt()
    if (!value$type %in% c("IDENT", "STRING")) {
        parse_stop("Expected string or ident, got ", token_repr(value),
                   pos = value$pos)
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
        parse_stop("Expected ']', got ", token_repr(nt), pos = nt$pos)
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

# The An+B grammar (css-syntax-3 section 6): whitespace is permitted
# only around the +/- sign that separates the B value (e.g. "2n + 1"),
# never inside or between the other components ("3 7", "2 n", "- n" are
# all invalid).
anb_re <- paste0("^[ \t\r\n\f]*",
                 "(odd|even|[+-]?[0-9]+|",
                 "[+-]?[0-9]*n([ \t\r\n\f]*[+-][ \t\r\n\f]*[0-9]+)?)",
                 "[ \t\r\n\f]*$")

# The text an nth-*() argument list spells, as written by the user.
series_source <- function(tokens) {
    paste0(sapply(tokens, function(x) x$value), collapse = "")
}

# The same text, case-folded for matching: the An+B microsyntax is ASCII
# case-insensitive (css-syntax-3), e.g. "2N", "ODD", "EVEN". chartr()
# rather than tolower() so the mapping is locale-independent; "nodev"
# covers every letter that can appear in a valid series.
series_text <- function(tokens) {
    chartr("NODEV", "nodev", series_source(tokens))
}

# The nth-*() pseudo-classes whose argument is an An+B series. :nth-col()
# and :nth-last-col() are deliberately absent: they are unsupported
# altogether, and the translator says so by name.
anb_function_names <- c("nth-child", "nth-last-child",
                        "nth-of-type", "nth-last-of-type")

# Reject an invalid An+B argument at parse time, where the tokens still
# carry the source positions the caret gutter needs and the pseudo-class
# the user wrote is still known. By translation time the series has been
# flattened to a string and every nth-*() looks alike.
validate_series <- function(tokens, function_name) {
    invalid <- function(..., pos) {
        # Lower-cased to match the translator's ":name() is unknown":
        # pseudo-class names are ASCII case-insensitive
        parse_stop("Invalid An+B expression in :", tolower(function_name),
                   "(): ", ..., pos = pos)
    }
    for (token in tokens) {
        if (token$type == "STRING")
            invalid("a quoted string is not allowed", pos = token$pos)
        # 'of <selector-list>' is consumed by the parser for
        # :nth-child()/:nth-last-child(); anywhere else it lands here as
        # part of the series
        if (token$type == "IDENT" && identical(tolower(token$value), "of"))
            invalid("'of' is only allowed in :nth-child() and ",
                    ":nth-last-child()", pos = token$pos)
    }
    series <- trimws(series_source(tokens))
    pos <- if (length(tokens)) tokens[[1]]$pos else NULL
    if (!grepl(anb_re, series_text(tokens)))
        invalid("'", series, "'", pos = pos)
    # The grammar has matched, so the only way parse_series() can still
    # fail is an A or B too large for an R integer
    ab <- parse_series(tokens)
    if (is.null(ab) || anyNA(ab))
        invalid("'", series, "' is out of the supported integer range",
                pos = pos)
    invisible(ab)
}

parse_series <- function(tokens) {
    for (token in tokens) {
        if (token$type == "STRING")
            stop("String tokens not allowed in series.")
    }
    s <- series_text(tokens)
    if (!grepl(anb_re, s))
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

# A parse failure that carries a 1-based source position so the parse()
# boundary can annotate the message with a caret. pos may be NULL when no
# meaningful source position is available.
parse_stop <- function(..., pos = NULL) {
    cond <- structure(
        class = c("selectr_parse_error", "selectr_error",
                  "error", "condition"),
        list(message = paste0(...), call = sys.call(-1), pos = pos)
    )
    stop(cond)
}

# Append a source-pointer gutter block to `message`, pointing a caret at
# character `pos` (1-based) within `css`. Returns `message` unchanged when
# `pos` or `css` is NULL, or when `css` contains a newline (multi-line
# selectors are vanishingly rare, but alignment would be wrong).
#
# The padding is built from the source text itself rather than a run of
# `pos - 1` spaces: a tab is echoed as a tab so the terminal applies the
# same tab stops to both the source line and the caret line, and any
# other character is replaced by spaces sized to its display width (via
# `nchar(type = "width")`) so double-width CJK/emoji characters still
# push the caret to the right column.
format_parse_error <- function(message, css, pos) {
    if (is.null(pos) || is.null(css) || grepl("[\r\n]", css))
        return(message)
    prefix <- substr(css, 1L, max(pos - 1L, 0L))
    chars <- strsplit(prefix, "", fixed = TRUE)[[1]]
    padding <- vapply(chars, function(ch) {
        if (identical(ch, "\t"))
            return("\t")
        width <- nchar(ch, type = "width")
        strrep(" ", if (is.na(width)) 1L else max(width, 1L))
    }, character(1))
    caret <- paste0(paste(padding, collapse = ""), "^")
    paste0(message, "\n  |\n  | ", css, "\n  | ", caret)
}

# Explain why `name` (the text after '#') is not identifier-shaped, and
# spell the intended id where there is one. The only ways match_hash can
# match a non-ident name are a leading digit, a '-' before a digit, and a
# lone '-', so the two branches below are exhaustive.
hash_ident_hint <- function(name) {
    m <- regmatches(name, regexec("^(-?)([0-9])(.*)$", name))[[1]]
    if (!length(m))
        return("an ID cannot be '-' alone")
    paste0("an identifier cannot start with a digit. Escape it: '#",
           m[2], "\\3", m[3], " ", m[4], "'")
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
# css-syntax-3 "would start an identifier": a name-start code point, or a
# leading '-' followed by a name-start code point, another '-' or an
# escape. Only a hash whose name starts an identifier is a hash of type
# "id", i.e. an ID selector; '#1' is not one.
match_ident_start <- compile_(paste0("^(--|-?([_a-zA-Z]|", nonascii,
                                     "|(?:", escape, ")))"))
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
    # Every alternative below starts with a backslash, so a token
    # without one -- the overwhelming majority -- needs no work. The
    # test is far cheaper than the gregexpr()/regmatches() pass it
    # skips, which the tokenizer would otherwise run on every ident.
    if (!any(grepl("\\", x, fixed = TRUE)))
        return(x)
    pattern <- paste0("\\\\[0-9a-fA-F]{1,6}(?:\r\n|[ \n\r\t\f])?",
                      if (newlines) "|\\\\(?:\r\n|[\n\r\f])",
                      "|\\\\.",
                      # a trailing backslash at true EOF (see `escape`
                      # above): css-syntax-3 decodes it to U+FFFD for an
                      # ident/hash, but drops it (does nothing) inside a
                      # string -- the same distinction `newlines` already
                      # marks
                      "|\\\\\\z")
    m <- gregexpr(pattern, x, perl = TRUE)
    regmatches(x, m) <- lapply(regmatches(x, m), function(esc) {
        if (length(esc) == 0) {
            return(esc)
        }
        is_hex <- grepl("^\\\\[0-9a-fA-F]", esc)
        is_eof <- esc == "\\"
        out <- substring(esc, 2)              # simple escape: the character
        out[grepl("^[\n\r\f]", out)] <- ""    # line continuation: nothing
        if (any(is_hex)) {
            hex <- sub("(?:\r\n|[ \n\r\t\f])$", "", out[is_hex], perl = TRUE)
            # css-syntax-3: a null, surrogate or out-of-range code point
            # decodes to U+FFFD. intToUtf8() would give NA (or "" for 0)
            # for these, so replace them before decoding.
            cp <- strtoi(hex, base = 16L)
            bad <- is.na(cp) | cp == 0L |
                (cp >= 0xD800L & cp <= 0xDFFFL) | cp > 0x10FFFFL
            cp[bad] <- 0xFFFDL
            out[is_hex] <- intToUtf8(cp, multiple = TRUE)
        }
        out[is_eof] <- if (newlines) "" else intToUtf8(0xFFFDL)
        out
    })
    x
}

# Anchored matchers only see the text they are handed, so slicing off the
# whole remaining input at every position (`substring(s, pos, len_s)`)
# copies O(n^2) characters over a long selector. Match against a bounded
# window instead, widening it only when the window might be cutting a
# token in half.
token_window <- 64L

# Run `matcher` (an anchored matcher built by compile_()) at `pos`,
# returning its match bounds relative to `pos` exactly as if it had been
# handed the whole remaining input.
#
# A match is known to be untruncated once it ends at least two characters
# short of the window: every matcher consumes greedily one character (or
# one escape sequence) at a time, so a match cut off by the window's end
# reaches that end -- except in a number, where a window ending just
# after the '.' of '1.5' loses the fractional alternative and falls back
# to the shorter integer one, stopping one character short. Both cases
# are covered by the slack.
#
# A failure to match is genuine as soon as the window is three characters
# wide: every matcher decides on the character at `pos`, apart from a
# number, which may need a sign and a '.' before its first digit.
match_window <- function(matcher, s, pos, len_s) {
    width <- token_window
    repeat {
        last <- min(pos + width - 1L, len_s)
        m <- matcher(substring(s, pos, last))
        if (last >= len_s ||
            (if (anyNA(m)) width >= 3L else m[2] < last - pos))
            return(m)
        width <- width * 2L
    }
}

tokenize <- function(s) {
    pos <- 1
    i <- 1
    len_s <- nchar(s)
    # Every token consumes at least one character, so this is an upper
    # bound (plus the trailing EOF); growing the list one element at a
    # time would copy it on each append
    results <- vector("list", len_s + 1L)
    while (pos <= len_s) {
        match <- match_window(match_whitespace, s, pos, len_s)
        if (!anyNA(match) && match[1] == 1) {
            results[[i]] <- Token("S", " ", pos)
            match_end <- match[2]
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_window(match_number, s, pos, len_s)
        if (!anyNA(match) && match[1] == 1) {
            match_end <- max(match[1], match[2])
            value <- substring(s, pos, pos + match_end - 1)
            results[[i]] <- Token("NUMBER", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_window(match_ident, s, pos, len_s)
        if (!anyNA(match) && match[1] == 1) {
            match_end <- max(match[1], match[2])
            value <- substring(s, pos, pos + match_end - 1)
            value <- decode_escapes(value)
            results[[i]] <- Token("IDENT", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_window(match_hash, s, pos, len_s)
        if (!anyNA(match) && match[1] == 1) {
            match_end <- max(match[1], match[2])
            value <- substring(s, pos, pos + match_end - 1)
            # The check is on the source text, not the decoded name,
            # so that an escaped digit ('#\31 ' spells the id '1') stays
            # legal while the bare digit ('#1') does not.
            if (anyNA(match_ident_start(substring(value, 2))))
                parse_stop("Invalid ID selector '", value, "' at position ",
                           pos, "; ", hash_ident_hint(substring(value, 2)),
                           pos = pos)
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
            match <- match_window(match_string_by_quote[[ch]], s, pos + 1, len_s)
            content_end <- if (anyNA(match)) 0 else match[2]
            end_quote <- pos + 1 + content_end
            # A string still open at EOF (content consumed to the end
            # of the input, including a lone trailing backslash -- see
            # `escape`) is auto-closed with the consumed value, as
            # css-syntax requires; only a string stopped short of EOF
            # by a raw newline is an error
            if (end_quote <= len_s &&
                substring(s, end_quote, end_quote) != ch) {
                parse_stop("Unclosed string at ", pos, pos = pos)
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
            # Widening windows again: an unterminated '/*' at the start
            # of a long selector would otherwise copy the whole tail
            width <- token_window
            repeat {
                last <- min(pos + width - 1L, len_s)
                # as.integer() strips regexpr()'s match.length and
                # friends, which would otherwise ride along on `pos`
                # and end up attached to every later token's position
                rel_pos <- as.integer(regexpr("*/", substring(s, pos, last),
                                              fixed = TRUE))
                if (rel_pos != -1L || last >= len_s)
                    break
                width <- width * 2L
            }
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
        parse_stop("Unexpected character '",
                   ch,
                   "' found at position ",
                   pos,
                   pos = pos)
    }
    results[[i]] <- EOFToken(pos)
    length(results) <- i
    results
}

TokenStream <- R6Class("TokenStream",
    public = list(
        pos = 1,
        tokens = NULL,
        ntokens = 0,
        # Index of the token most recently returned by nxt(). Positions
        # are consumed in order, so this doubles as a count of consumed
        # tokens: parse_simple_selector() only compares it against an
        # earlier reading to ask whether anything was consumed in
        # between. The sticky EOF token (see next_token()) leaves pos --
        # and so this -- alone once it has been consumed the first time.
        consumed = 0,
        source_text = NULL,
        peeked = list(),
        peeking = FALSE,
        initialize = function(tokens, source_text = NULL) {
            self$tokens <- tokens
            self$ntokens <- length(tokens)
            self$source_text <- source_text
        },
        nxt = function() {
            nt <- if (self$peeking) {
                self$peeking <- FALSE
                self$peeked
            } else {
                self$next_token()
            }
            self$consumed <- self$pos
            nt
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
                parse_stop("Expected ident, got ", token_repr(nt), pos = nt$pos)
            nt$value
        },
        next_ident_or_star = function() {
            nt <- self$nxt()
            if (nt$type == "IDENT")
                nt$value
            else if (token_equality(nt, "DELIM", "*"))
                NULL
            else
                parse_stop("Expected ident or '*', got ", token_repr(nt), pos = nt$pos)
        },
        skip_whitespace = function() {
            peek <- self$peek()
            if (peek$type == "S")
                self$nxt()
        }
    )
)
