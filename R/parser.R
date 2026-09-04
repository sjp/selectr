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
                    nonascii = nonascii)

# Base class for every parse-tree node below. Only repr() is
# type-specific; show() and the "ClassName[...]" wrapping that most
# repr() implementations use are common enough to live here once.
Node <- R6Class("Node",
    public = list(
        # The name first_class_name() reports for this node, in place
        # of its R6 class name. NULL for every class except
        # ClassSelector, whose class is named to avoid an R.oo clash
        # (fixed in 0.4-1 by renaming the R6 class) even though its
        # repr() - matching the Python cssselect output - still needs
        # to read "Class".
        repr_name = NULL,
        # Wraps 'content' as "ClassName[content]", the shape used by
        # every repr() below except Selector's (which has no brackets)
        # and CombinedSelector's (which wraps once per spine node, not
        # just 'self').
        repr_wrap = function(content) {
            paste0(first_class_name(self), "[", content, "]")
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

Selector <- R6Class("Selector",
    inherit = Node,
    public = list(
        parsed_tree = NULL,
        pseudo_element = NULL,
        initialize = function(tree, pseudo_element = NULL) {
            self$parsed_tree <- tree
            if (!is.null(pseudo_element))
                self$pseudo_element <- ascii_lower(pseudo_element)
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
        }
    )
)

ClassSelector <- R6Class("ClassSelector",
    inherit = Node,
    public = list(
        repr_name = "Class",
        selector = NULL,
        class_name = NULL,
        initialize = function(selector, class_name) {
            self$selector <- selector
            self$class_name <- class_name
        },
        repr = function() {
            self$repr_wrap(paste0(self$selector$repr(), ".", self$class_name))
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        }
    )
)

Function <- R6Class("Function",
    inherit = Node,
    public = list(
        selector = NULL,
        name = NULL,
        arguments = NULL,
        selector_list = NULL,
        # The (a, b) pair for an An+B (nth-*()) function, already
        # validated and parsed by validate_series() at parse time; NULL
        # for every other function
        series = NULL,
        # The comma-separated items of a :lang() argument list, one
        # token each, reassembled at parse time (see
        # lang_range_token()); NULL for every other function
        ranges = NULL,
        initialize = function(selector, name, arguments, selector_list = NULL,
                              series = NULL, ranges = NULL) {
            self$selector <- selector
            self$name <- ascii_lower(name)
            self$arguments <- arguments
            self$selector_list <- selector_list
            self$series <- series
            self$ranges <- ranges
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
            self$repr_wrap(paste0(
                self$selector$repr(),
                ":",
                self$name,
                "(",
                token_values,
                selector_list_repr,
                ")"))
        },
        argument_types = function() {
            token_types <- lapply(self$arguments, function(token) token$type)
            unlist(token_types)
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            if (!is.null(self$selector_list) && length(self$selector_list) > 0)
                specs <- specs + max_specificity(self$selector_list)
            specs
        }
    )
)

Pseudo <- R6Class("Pseudo",
    inherit = Node,
    public = list(
        selector = NULL,
        ident = NULL,
        initialize = function(selector, ident) {
            self$selector <- selector
            self$ident <- ascii_lower(ident)
        },
        repr = function() {
            self$repr_wrap(paste0(self$selector$repr(), ":", self$ident))
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        }
    )
)

# :not(), :is() and :has() all take the specificity of their most
# specific argument (CSS Selectors Level 4). vapply() pins the result to
# a 3 x n matrix, so one argument is handled just like many; the caller
# is responsible for the empty case, where there is no such argument.
max_specificity <- function(selector_list) {
    specs <- vapply(selector_list, function(s) s$specificity(), numeric(3))
    # most specific first: (id, class, element) descending
    specs[, order(-specs[1, ], -specs[2, ], -specs[3, ])[1]]
}

# Specificity for the selector-list pseudo-classes (:not(), :is(),
# :where(), :has()): the base selector's specificity plus that of the
# list's most specific member. 'ignore_list' is for :where(), which by
# spec always contributes zero specificity from its argument list;
# otherwise an empty list (possible only for :is(), via ':is()')
# contributes nothing rather than being passed to max_specificity(),
# which requires at least one selector.
selector_list_specificity <- function(selector, selector_list,
                                      ignore_list = FALSE) {
    base_specs <- selector$specificity()
    if (ignore_list || length(selector_list) == 0)
        return(base_specs)
    base_specs + max_specificity(selector_list)
}

# Base class for the four selector-list pseudo-classes (:not(), :is(),
# :where(), :has()), which differ only in the pseudo-class name printed
# by repr() and, for :where(), zero specificity from the argument list.
# Thin subclasses below exist only so the translator's first_class_name()
# dispatch (xpath_negation(), xpath_matching(), xpath_where(),
# xpath_has()) still sees one method name per pseudo-class.
SelectorListPseudo <- R6Class("SelectorListPseudo",
    inherit = Node,
    public = list(
        selector = NULL,
        selector_list = NULL,
        pseudo_name = NULL,
        zero_specificity = FALSE,
        initialize = function(selector, selector_list, pseudo_name,
                              zero_specificity = FALSE) {
            self$selector <- selector
            self$selector_list <- selector_list
            self$pseudo_name <- pseudo_name
            self$zero_specificity <- zero_specificity
        },
        repr = function() {
            self$repr_wrap(paste0(
                self$selector$repr(),
                ":", self$pseudo_name, "(",
                paste0(
                    sapply(self$selector_list, function(s) s$repr()),
                    collapse = ", "
                ),
                ")"))
        },
        specificity = function() {
            selector_list_specificity(self$selector, self$selector_list,
                                      ignore_list = self$zero_specificity)
        }
    )
)

Negation <- R6Class("Negation",
    inherit = SelectorListPseudo,
    public = list(
        initialize = function(selector, selector_list) {
            super$initialize(selector, selector_list, "not")
        }
    )
)

Matching <- R6Class("Matching",
    inherit = SelectorListPseudo,
    public = list(
        initialize = function(selector, selector_list) {
            super$initialize(selector, selector_list, "is")
        }
    )
)

Where <- R6Class("Where",
    inherit = SelectorListPseudo,
    public = list(
        initialize = function(selector, selector_list) {
            super$initialize(selector, selector_list, "where",
                             zero_specificity = TRUE)
        }
    )
)

# A :has() argument with an explicit leading combinator (selectors-4
# <relative-selector>): wraps the parsed selector alongside its combinator.
# Arguments with the omitted (implied descendant) combinator are stored
# unwrapped in Has$selector_list.
RelativeSelector <- R6Class("RelativeSelector",
    inherit = Node,
    public = list(
        combinator = NULL,
        selector = NULL,
        initialize = function(combinator, selector) {
            self$combinator <- combinator
            self$selector <- selector
        },
        repr = function() {
            self$repr_wrap(paste0(self$combinator, " ", self$selector$repr()))
        },
        specificity = function() {
            # The leading combinator contributes no specificity
            self$selector$specificity()
        }
    )
)

Has <- R6Class("Has",
    inherit = SelectorListPseudo,
    public = list(
        initialize = function(selector, selector_list) {
            super$initialize(selector, selector_list, "has")
        }
    )
)

Attrib <- R6Class("Attrib",
    inherit = Node,
    public = list(
        selector = NULL,
        namespace = NULL,
        attrib = NULL,
        operator = NULL,
        value = NULL,
        flag = NULL,
        # See Element$any_namespace: '[*|attr]' against '[\2a|attr]'
        any_namespace = FALSE,
        initialize = function(selector, namespace, attrib, operator, value,
                              flag = NULL,
                              any_namespace = identical(namespace, "*")) {
            self$selector <- selector
            self$namespace <- namespace
            self$any_namespace <- any_namespace
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
            inner <-
                if (self$operator == "exists")
                    attr
                else
                    paste0(
                        attr,
                        " ",
                        self$operator,
                        " '",
                        self$value,
                        "'",
                        if (!is.null(self$flag)) paste0(" ", self$flag) else "")
            self$repr_wrap(paste0(self$selector$repr(), "[", inner, "]"))
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[2] <- specs[2] + 1
            specs
        }
    )
)

Element <- R6Class("Element",
    inherit = Node,
    public = list(
        namespace = NULL,
        element = NULL,
        # Whether 'namespace' is the any-namespace wildcard, i.e. the
        # delimiter '*' of an <ns-prefix>. An <ident-token> that merely
        # decodes to the same character ('\2a|e') is a prefix *named*
        # '*', which no @namespace rule can bind, so the two cannot be
        # told apart by the stored value. Defaults to the reading a
        # hand-built node would have had before the flag existed.
        any_namespace = FALSE,
        initialize = function(namespace = NULL, element = NULL,
                              any_namespace = identical(namespace, "*")) {
            self$namespace <- namespace
            self$element <- element
            self$any_namespace <- any_namespace
        },
        repr = function() {
            el <-
                if (!is.null(self$element)) self$element
                else "*"
            if (!is.null(self$namespace))
                el <- paste0(self$namespace, "|", el)
            self$repr_wrap(el)
        },
        specificity = function() {
            if (!is.null(self$element)) c(0, 0, 1)
            else rep(0, 3)
        }
    )
)

Hash <- R6Class("Hash",
    inherit = Node,
    public = list(
        selector = NULL,
        id = NULL,
        initialize = function(selector, id) {
            self$selector <- selector
            self$id <- id
        },
        repr = function() {
            self$repr_wrap(paste0(self$selector$repr(), "#", self$id))
        },
        specificity = function() {
            specs <- self$selector$specificity()
            specs[1] <- specs[1] + 1
            specs
        }
    )
)

CombinedSelector <- R6Class("CombinedSelector",
    inherit = Node,
    public = list(
        selector = NULL,
        combinator = NULL,
        subselector = NULL,
        initialize = function(selector, combinator, subselector) {
            if (is.null(selector))
                internal_stop("'selector' cannot be NULL")
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
                out <- node$repr_wrap(paste0(out, " ", comb, " ",
                                             node$subselector$repr()))
            }
            out
        },
        specificity = function() {
            spine <- combinator_spine(self)
            specs <- spine$leftmost$specificity()
            for (node in spine$nodes)
                specs <- specs + node$subselector$specificity()
            specs
        }
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
            parse_selector_group(stream)
        },
        selectr_parse_error = function(e) {
            # Re-signal at the parse() boundary so the message gains the
            # source-pointer gutter, but keep the condition class and the
            # machine-readable fields so callers can handle it structurally.
            selectr_abort(format_parse_error(conditionMessage(e), css, e$pos),
                         "selectr_parse_error", pos = e$pos, selector = css)
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

# Rejects a pseudo-element parsed before this point: a legacy or '::'
# pseudo-element is only valid as the final component of a selector.
reject_pseudo_element_not_last <- function(pseudo_element, pos) {
    if (!is.null(pseudo_element)) {
        parse_stop("Got pseudo-element ::",
                   pseudo_element,
                   " not at the end of a selector",
                   pos = pos)
    }
}

# Rejects '::slotted(x)', '::part(x)' and friends. The pseudo-element is
# functional rather than merely followed by more of the compound, so
# reporting it here keeps reject_pseudo_element_not_last() from claiming
# it is not last when the '(' is read as the next simple selector.
reject_functional_pseudo_element <- function(name, stream) { # nolint: object_length_linter.
    peek <- stream$peek()
    if (token_equality(peek, "DELIM", "("))
        parse_stop("The functional pseudo-element ::", name,
                   "() is not supported", pos = peek$pos)
}

# Rejects a class name that is not identifier-shaped, e.g. '.5'. The
# tokenizer reads '.5' as a single number and '.-5' as a '.' delimiter
# followed by one, so neither reaches the ident rules that give '#5' its
# hint; both spellings are caught by hand at the call sites. Returns
# without erroring for anything else, leaving the stray-token error to
# describe it.
reject_invalid_class <- function(text, pos) {
    hint <- ident_hint(substring(text, 2), ".")
    if (!is.null(hint))
        parse_stop("Invalid class selector '", text, "'; ", hint, pos = pos)
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
        reject_pseudo_element_not_last(pseudo_element, peek$pos)
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
        # A leading '*' is the universal selector, or - before a '|' -
        # the any-namespace prefix, only when it is the delimiter. An
        # identifier that merely decodes to the same character ('\*',
        # '\2a') is an <ident-token> naming an element '*', or a
        # namespace prefix named '*' that no @namespace rule can bind,
        # so it must not be read as either - hence the flag rather than
        # a test of the value below. next_ident_or_star() draws the
        # same line for the local name of a namespaced selector.
        star_delim <- FALSE
        if (peek$type == "IDENT") {
            namespace <- stream$nxt()$value
        } else if (token_equality(peek, "DELIM", "*")) {
            stream$nxt()
            # '*|e': any namespace, including none
            namespace <- "*"
            star_delim <- TRUE
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
            any_namespace <- star_delim
        } else {
            element <- if (star_delim) NULL else namespace
            namespace <- NULL
            any_namespace <- FALSE
        }
    } else {
        element <- namespace <- NULL
        any_namespace <- FALSE
    }
    result <- Element$new(namespace, element, any_namespace)
    pseudo_element <- NULL
    while (TRUE) {
        peek <- stream$peek()
        if (any(peek$type == c("S", "EOF")) ||
            token_is_delim(peek, c(",", "+", ">", "~")) ||
            (inside_arguments && token_equality(peek, "DELIM", ")"))) {
            break
        }
        reject_pseudo_element_not_last(pseudo_element, peek$pos)
        if (peek$type == "HASH") {
            result <- Hash$new(result, stream$nxt()$value)
        } else if (token_equality(peek, "DELIM", ".")) {
            stream$nxt()
            after_dot <- stream$peek()
            if (after_dot$type == "NUMBER")
                reject_invalid_class(paste0(".", after_dot$value), peek$pos)
            result <- ClassSelector$new(result, stream$next_ident())
        } else if (token_equality(peek, "DELIM", "[")) {
            stream$nxt()
            result <- parse_attrib(result, stream)
        } else if (token_equality(peek, "DELIM", ":") ||
                   token_equality(peek, "DELIM", "::")) {
            if (token_equality(peek, "DELIM", "::")) {
                stream$nxt()
                pseudo_element <- stream$next_ident()
                reject_functional_pseudo_element(pseudo_element, stream)
                next
            } else {
                stream$nxt()
            }
            ident <- stream$next_ident()
            lident <- ascii_lower(ident)
            if (lident %in% c(
                "first-line", "first-letter", "before", "after")) {
                # Special case: CSS 2.1 pseudo-elements can have a single ':'
                # Any new pseudo-element must have two.
                pseudo_element <- ident
                reject_functional_pseudo_element(ident, stream)
                next
            }
            if (!token_equality(stream$peek(), "DELIM", "(")) {
                result <- Pseudo$new(result, ident)
                next
            }
            stream$nxt()
            stream$skip_whitespace()
            if (lident == "not") {
                # Selectors Level 4 places no nesting restriction on
                # :not(), so :not(:not(a)), :is(:not(a)), etc. are valid.
                selectors <- parse_simple_selector_arguments(stream, "not",
                                                             inside_has = inside_has)
                result <- Negation$new(result, selectors)
            } else if (any(lident == c("matches", "is"))) {
                # :is()/:matches() take a <forgiving-selector-list>, so
                # an empty argument list is valid (it matches nothing)
                selectors <- parse_simple_selector_arguments(stream, lident,
                                                             inside_has = inside_has,
                                                             forgiving = TRUE)
                result <- Matching$new(result, selectors)
            } else if (lident == "where") {
                selectors <- parse_simple_selector_arguments(stream, "where",
                                                             inside_has = inside_has,
                                                             forgiving = TRUE)
                result <- Where$new(result, selectors)
            } else if (lident == "has") {
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
                allow_commas <- lident == "lang"
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
                # The comma-separated items of a :lang() list, one
                # token each, reassembled here where the commas are
                # still visible: a single range may arrive as several
                # adjacent tokens ("de-", '*', "-DE"), and once the
                # commas are gone from `arguments` there is no telling
                # that apart from two ranges written side by side
                # ("en" '*'), which is not a range list at all.
                ranges <- list()
                range_value <- NULL
                range_pos <- 0

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
                        if (allow_commas) {
                            if (is.null(range_value))
                                range_pos <- nt$pos
                            range_value <- paste0(range_value, nt$value)
                        }

                        # Check if this is the 'of' keyword for an An+B
                        # function. It is only meaningful for
                        # nth-child()/nth-last-child(); for the
                        # of-type variants it is left in `arguments` so
                        # validate_series() below rejects it with its
                        # precise message, instead of the selector
                        # list that follows being parsed as (and
                        # failing as) more An+B tokens.
                        if (nt$type == "IDENT" && ascii_lower(nt$value) == "of" &&
                            lident %in% anb_function_names) {
                            if (any(lident == c("nth-child", "nth-last-child"))) {
                                # Remove 'of' from arguments - it's a keyword, not an argument
                                arguments <- arguments[-length(arguments)]

                                # Parse the selector list that follows 'of'
                                stream$skip_whitespace()
                                selector_list <- parse_simple_selector_arguments(stream, ident,
                                                                                 inside_has = inside_has)
                            }
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
                        if (is.null(range_value))
                            range_pos <- nt$pos
                        range_value <- paste0(range_value, nt$value)
                    } else if (nt$type == "S") {
                        # Keep whitespace tokens for the An+B (nth-*)
                        # functions so parse_series() can validate
                        # whitespace placement; other functions simply
                        # skip whitespace.
                        if (startsWith(lident, "nth-")) {
                            arguments[[i]] <- nt
                            i <- i + 1
                        }
                        ws_since_arg <- TRUE
                        next
                    } else if (token_equality(nt, "DELIM", ",") && allow_commas) {
                        # For :lang(), commas separate multiple values
                        ranges[[length(ranges) + 1]] <-
                            lang_range_token(range_value, range_pos, nt)
                        range_value <- NULL
                        stream$skip_whitespace()
                        has_arg <- FALSE
                        ws_since_arg <- FALSE
                        next
                    } else if (token_equality(nt, "DELIM", ")") ||
                               nt$type == "EOF") {
                        # EOF auto-closes the function (css-syntax):
                        # ':lang(fr' means ':lang(fr)'
                        if (allow_commas &&
                            (!is.null(range_value) || length(ranges) > 0))
                            ranges[[length(ranges) + 1]] <-
                                lang_range_token(range_value, range_pos, nt)
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

                series <- NULL
                if (lident %in% anb_function_names) {
                    series <- validate_series(arguments, ident)
                    # Whitespace tokens were only retained so
                    # validate_series() could check An+B spacing; the
                    # parsed (a, b) pair is now stored on the Function
                    # node instead, so drop them before display
                    arguments <- Filter(function(a) a$type != "S", arguments)
                }

                result <- Function$new(result, ident, arguments, selector_list,
                                       series = series,
                                       ranges = if (allow_commas) ranges)
            }
        } else {
            if (peek$type == "NUMBER" && startsWith(peek$value, "."))
                reject_invalid_class(peek$value, peek$pos)
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
            # function_name is always supplied by every call site
            # ("not", "is"/"matches", "where", "has", or the nth-*
            # ident's "of" selector list)
            parse_stop("Got pseudo-element ::", pseudo_element,
                       " inside :", function_name, "()",
                       pos = stream$peek()$pos)
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
            # Unreachable: parse_simple_selector() only returns (without
            # erroring itself) with the stream positioned at one of S,
            # EOF, ')', ',', '+', '>' or '~' -- inside_arguments makes
            # ')' a valid stop token there too -- and the chain loop
            # above consumes or breaks on every one of those, always
            # leaving nt as ')', ',' or EOF by the time it gets here
            internal_stop("Unexpected argument-list token ", token_repr(nt))
        }
    }

    arguments
}

parse_attrib <- function(selector, stream) {
    stream$skip_whitespace()
    any_namespace <- FALSE
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
            # next_ident_or_star() returns NULL for the delimiter '*',
            # i.e. '[*|attr]', and the decoded value for an identifier
            # that happens to spell one, i.e. '[\2a|attr]' - a prefix
            # named '*', which is not the any-namespace wildcard
            any_namespace <- is.null(attrib)
            namespace <- if (any_namespace) "*" else attrib
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
            return(Attrib$new(selector, namespace, attrib, "exists", NULL,
                              any_namespace = any_namespace))
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
        # An unquoted number is never a valid attribute value, in any
        # browser either, but it is a common mistake: name the fix
        # rather than the grammar
        if (value$type == "NUMBER") {
            name <- if (is.null(namespace)) attrib
                    else paste0(namespace, "|", attrib)
            parse_stop("Attribute values must be quoted unless they are ",
                       "identifiers: write [", name, op, "\"", value$value,
                       "\"]", pos = value$pos)
        }
        parse_stop("Expected string or ident, got ", token_repr(value),
                   pos = value$pos)
    }
    stream$skip_whitespace()
    nt <- stream$nxt()
    # CSS Selectors Level 4 allows an optional case-sensitivity flag
    # before the closing bracket, e.g. '[attr="value" i]'
    flag <- NULL
    if (nt$type == "IDENT" && ascii_lower(nt$value) %in% c("i", "s")) {
        flag <- ascii_lower(nt$value)
        stream$skip_whitespace()
        nt <- stream$nxt()
    }
    if (!token_equality(nt, "DELIM", "]") && nt$type != "EOF") {
        parse_stop("Expected ']', got ", token_repr(nt), pos = nt$pos)
    }
    Attrib$new(selector, namespace, attrib, op, value$value, flag,
               any_namespace = any_namespace)
}

str_int <- function(s) {
    # An+B takes <integer> values only (css-syntax-3), so reject
    # anything as.integer() would otherwise coerce through double
    # and truncate, e.g. "2.5" -> 2L or "2e1" -> 20L.
    if (!grepl("^[+-]?[0-9]+$", s))
        return(NA_integer_)
    n <- suppressWarnings(as.integer(s))
    if (!is.na(n))
        return(n)
    # A well-formed integer that simply does not fit in an R one. The
    # An+B grammar sets no bound, but no document has
    # .Machine$integer.max siblings, so saturating selects exactly what
    # the written value would; rejecting it would instead turn a
    # never-matching selector into an error.
    if (startsWith(s, "-")) -.Machine$integer.max else .Machine$integer.max
}

# The An+B grammar (css-syntax-3 section 6): whitespace is permitted
# only around the +/- sign that separates the B value (e.g. "2n + 1"),
# never inside or between the other components ("3 7", "2 n", "- n" are
# all invalid).
anb_re <- paste0("^[ \t\r\n\f]*",
                 "(odd|even|[+-]?[0-9]+|",
                 "[+-]?[0-9]*n([ \t\r\n\f]*[+-][ \t\r\n\f]*[0-9]+)?)",
                 "[ \t\r\n\f]*$")

# The identifiers the An+B grammar admits (css-syntax-3 section 6.2):
# the two keywords, the bare-'n' forms, and the <ndash-ident> and
# <ndashdigit-ident> forms ('n-', '-n-', 'n-3', '-n-3'). Everything
# numeric in a series comes from a number token instead, which anb_re
# above matches.
anb_ident_re <- "^(odd|even|-?n(-|-[0-9]+)?)$"

# The An+B grammar is written over tokens, not over decoded text: an
# escape can only begin an identifier (css-syntax-3 section 4.3.1
# "Consume a token"), so a digit written as one is a name character,
# never an <integer>. ':nth-child(\32 )' is thus the identifier "2",
# which matches no An+B production, and browsers reject it -- even
# though the decoded text anb_re sees reads as a plain '2'. This gives
# the first token spelling an identifier An+B does not admit, or NULL
# when each of them is one it does.
anb_invalid_ident <- function(tokens) {
    for (token in tokens) {
        if (token$type != "IDENT")
            next
        value <- ascii_lower(token$value)
        if (grepl(anb_ident_re, value))
            next
        return(token)
    }
    NULL
}

# The text an nth-*() argument list spells: the tokens' values, so any
# escape in the argument has been decoded away. anb_re matches this
# text, and anb_invalid_ident() the tokens it came from, since the two
# readings can differ.
series_source <- function(tokens) {
    paste0(sapply(tokens, function(x) x$value), collapse = "")
}

# The same text, case-folded for matching: the An+B microsyntax is ASCII
# case-insensitive (css-syntax-3), e.g. "2N", "ODD", "EVEN". The fold is
# ascii_lower() for the reasons given with it: a mapping independent of
# the locale, and one that can be handed any argument the parser reads,
# ':nth-child(\FFFE)' included.
series_text <- function(tokens) {
    ascii_lower(series_source(tokens))
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
        parse_stop("Invalid An+B expression in :", ascii_lower(function_name),
                   "(): ", ..., pos = pos)
    }
    for (token in tokens) {
        if (token$type == "STRING")
            invalid("a quoted string is not allowed", pos = token$pos)
        # 'of <selector-list>' is consumed by the parser for
        # :nth-child()/:nth-last-child(); anywhere else it lands here as
        # part of the series
        if (token$type == "IDENT" && identical(ascii_lower(token$value), "of"))
            invalid("'of' is only allowed in :nth-child() and ",
                    ":nth-last-child()", pos = token$pos)
    }
    series <- trimws(series_source(tokens))
    pos <- if (length(tokens)) tokens[[1]]$pos else NULL
    # parse_series() applies the An+B grammar itself, and saturates a
    # value too large for an R integer rather than failing, so NULL
    # here means the argument is not an An+B expression at all
    ab <- parse_series(tokens)
    if (is.null(ab)) {
        # An argument whose decoded text does spell an An+B expression
        # can only have been rejected by parse_series()'s token check,
        # and naming the offending identifier is more use than quoting
        # that text, which reads as valid ('\32 ' shows as '2')
        bad <- NULL
        if (grepl(anb_re, series_text(tokens)))
            bad <- anb_invalid_ident(tokens)
        if (!is.null(bad))
            invalid("an escape spells a name, so '", bad$value,
                    "' is an identifier, which An+B does not allow",
                    pos = bad$pos)
        invalid("'", series, "'", pos = pos)
    }
    invisible(ab)
}

# An internal helper of validate_series(): tokens reaching here have
# already passed its STRING-token checks, so this applies the An+B
# grammar and extracts the (a, b) pair, giving NULL when the grammar
# does not match. An A or B too large for an R integer is saturated to
# .Machine$integer.max. Also exercised directly by tests.
parse_series <- function(tokens) {
    s <- series_text(tokens)
    if (!grepl(anb_re, s))
        return(NULL)
    # The decoded text can spell an An+B expression that the tokens do
    # not, an escaped digit being read as the digit; see
    # anb_invalid_ident()
    if (!is.null(anb_invalid_ident(tokens)))
        return(NULL)
    s <- gsub("[ \t\r\n\f]+", "", s)
    if (s == "odd")
        return(2:1)
    else if (s == "even")
        return(c(2L, 0L))
    else if (s == "n")
        return(1:0)
    n_pos <- regexpr("n", s, fixed = TRUE)
    if (n_pos == -1L) {
        result <- str_int(s)
        if (is.na(result)) {
            return(NULL)
        } else {
            return(c(0L, result))
        }
    }
    # Split at the first 'n' only
    a <- trimws(substring(s, 1, n_pos - 1))
    b <- trimws(substring(s, n_pos + 1))

    intb <- str_int(b)
    if (!nzchar(a) && is.na(intb))
        return(NULL)

    if (!nzchar(a))
        a <- 1L
    else if (a == "-" || a == "+")
        a <- str_int(paste0(a, "1"))
    else
        a <- str_int(a)
    if (!nzchar(b))
        b <- 0L
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

# One item of a :lang() argument list: a RANGE token holding the
# item's tokens reassembled into a single range, starting at source
# position `pos`. An item with no tokens at all (":lang(en, )") has no
# range to report, so `delim` - the ',', ')' or EOF found where the
# range should be - stands in its place, for the translator to reject
# with the rest of its argument checks.
lang_range_token <- function(value, pos, delim) {
    if (is.null(value))
        delim
    else
        Token("RANGE", value, pos)
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
    selectr_abort(paste0(...), "selectr_parse_error", pos = pos)
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
    if (is.null(pos))
        return(message)
    if (is.null(css) || grepl("[\r\n]", css))
        return(paste0(message, " at position ", pos))
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

# Explain why `name` (the text after a '#' or '.') is not
# identifier-shaped, and spell the intended id or class with the digit
# escaped. NULL for a name that does not start with a digit (optionally
# preceded by a '-'), which the caller reports its own way.
ident_hint <- function(name, prefix) {
    m <- regmatches(name, regexec("^(-?)([0-9])(.*)$", name))[[1]]
    if (!length(m))
        return(NULL)
    paste0("an identifier cannot start with a digit. Escape it: '", prefix,
           m[2], "\\3", m[3], " ", m[4], "'")
}

# The only ways match_hash can match a non-ident name are a leading
# digit, a '-' before a digit, and a lone '-', so the two branches here
# are exhaustive.
hash_ident_hint <- function(name) {
    hint <- ident_hint(name, "#")
    if (is.null(hint)) "an ID cannot be '-' alone" else hint
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
delims_1ch <- c(">", "+", "-", "~", ",", ".", "*", "=", "[", "]", "(", ")", "|", ":", "#")
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
            match_end <- match[2]
            # A comment between two whitespace runs (or two adjacent
            # comments) leaves no token behind, so this run may be the
            # second one seen in a row. Extend the existing S token
            # instead of emitting a second one, keeping the invariant
            # that no two S tokens are ever adjacent; the token's pos
            # stays at the start of the first run for error carets.
            if (i == 1 || results[[i - 1]]$type != "S") {
                results[[i]] <- Token("S", " ", pos)
                i <- i + 1
            }
            pos <- pos + match_end
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
        # css-syntax-3 "consume a token" tests for a CDC before it
        # tests for an identifier, so '-->' is one token and not the
        # name '--' followed by a child combinator, even though '--'
        # does start an ident sequence. A CDC cannot appear anywhere
        # in a selector; tokenizing it is what lets the parser say so
        # about the whole of it, at the position it starts.
        if (substring(s, pos, pos + 2) == "-->") {
            results[[i]] <- Token("CDC", "-->", pos)
            pos <- pos + 3
            i <- i + 1
            next
        }
        # css-syntax-3 "would start an ident sequence" is narrower than
        # match_ident's character class, which also matches a lone '-'.
        # A leading digit cannot reach here, since match_number above
        # claims it, so '-' is the only start whose two readings can
        # differ: one with nothing name-like after it is not a name at
        # all, and falls through to the delimiter table below, which
        # gives it the '-' <delim-token> the specification makes it.
        starts_ident <- substring(s, pos, pos) != "-" ||
            !anyNA(match_window(match_ident_start, s, pos, len_s))
        if (starts_ident) {
            match <- match_window(match_ident, s, pos, len_s)
            if (!anyNA(match) && match[1] == 1) {
                match_end <- max(match[1], match[2])
                value <- substring(s, pos, pos + match_end - 1)
                results[[i]] <- Token("IDENT", decode_escapes(value), pos)
                pos <- pos + match_end
                i <- i + 1
                next
            }
        }
        match <- match_window(match_hash, s, pos, len_s)
        if (!anyNA(match) && match[1] == 1) {
            match_end <- max(match[1], match[2])
            value <- substring(s, pos, pos + match_end - 1)
            # The check is on the source text, not the decoded name,
            # so that an escaped digit ('#\31 ' spells the id '1') stays
            # legal while the bare digit ('#1') does not.
            if (anyNA(match_ident_start(substring(value, 2))))
                parse_stop("Invalid ID selector '", value, "'; ",
                           hash_ident_hint(substring(value, 2)),
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
                parse_stop("Unclosed string", pos = pos)
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
        # The CDC's counterpart (css-syntax-3 "consume a token"). It is
        # no more valid in a selector than a CDC is, and is read here
        # for the same reason: so that the parser rejects the construct
        # rather than the tokenizer rejecting its first character.
        if (substring(s, pos, pos + 3) == "<!--") {
            results[[i]] <- Token("CDO", "<!--", pos)
            pos <- pos + 4
            i <- i + 1
            next
        }
        # Every successful match ends in 'next', so reaching here means
        # the character cannot start any token
        parse_stop("Unexpected character '",
                   ch,
                   "'",
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
        peeked = list(),
        peeking = FALSE,
        initialize = function(tokens) {
            self$tokens <- tokens
            self$ntokens <- length(tokens)
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
