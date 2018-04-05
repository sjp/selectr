escape <- paste0("([0-9a-f]{1,6})(\r\n|[ \n\r\t\f])?", "|[^\n\r\f0-9a-f]")
nonascii <- "[^\1-\177]"
hash_re <- "([_a-z0-9-]|([0-9a-f]{1,6})(\r\n|[ \n\r\t\f])?|[^\1-\177])"

TokenMacros <- list(unicode_escape = "\\([0-9a-f]{1,6})(?:\r\n|[ \n\r\t\f])?",
                    escape = escape,
                    string_escape = paste0("\\(?:\n|\r\n|\r|\f)|", escape),
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
        initialize = function(selector, name, arguments) {
            self$selector <- selector
            self$name <- tolower(name)
            self$arguments <- arguments
        },
        repr = function() {
            token_values <- lapply(self$arguments,
                function(token) paste0("'", token$value, "'"))
            token_values <- paste0(unlist(token_values), collapse = ", ")
            token_values <- paste0("[", token_values, "]")
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":",
                self$name,
                "(",
                token_values,
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
        subselector = NULL,
        initialize = function(selector, subselector) {
            self$selector <- selector
            self$subselector <- subselector
        },
        repr = function() {
            paste0(
                first_class_name(self),
                "[",
                self$selector$repr(),
                ":not(",
                self$subselector$repr(),
                ")]")
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

Attrib <- R6Class("Attrib",
    public = list(
        selector = NULL,
        namespace = NULL,
        attrib = NULL,
        operator = NULL,
        value = NULL,
        initialize = function(selector, namespace, attrib, operator, value) {
            self$selector <- selector
            self$namespace <- namespace
            self$attrib <- attrib
            self$operator <- operator
            self$value <- value
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
                    "']]")
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

# foo
el_re <- '^[ \t\r\n\f]*([a-zA-Z]+)[ \t\r\n\f]*$'

# foo#bar or #bar
id_re <- '^[ \t\r\n\f]*([a-zA-Z]*)#([a-zA-Z0-9_-]+)[ \t\r\n\f]*$'

# foo.bar or .bar
class_re <- '^[ \t\r\n\f]*([a-zA-Z]*)\\.([a-zA-Z][a-zA-Z0-9_-]*)[ \t\r\n\f]*$'

parse <- function(css) {
    nc <- nchar(css)
    el_match <- str_match(css, el_re)[1, 2]
    if (!is.na(el_match))
        return(list(Selector$new(Element$new(element = el_match))))
    id_match <- str_match(css, id_re)[1, 2:3]
    if (!is.na(id_match[2]))
        return(list(Selector$new(
                        Hash$new(
                            Element$new(
                                element =
                                    if (nzchar(id_match[1]) == 0) NULL
                                    else id_match[1]),
                            id_match[2]))))
    class_match <- str_match(css, class_re)[1, 2:3]
    if (!is.na(class_match[3]))
        return(list(Selector$new(
                        ClassSelector$new(
                            Element$new(
                                element =
                                    if (is.na(class_match[2])) NULL
                                    else class_match[2]),
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
    # val can be NULL or (maybe) NA
    if (is.null(val) && is.null(token$value))
        return(TRUE)
    if (is.na(val) && is.na(token$value))
        return(TRUE)
    # Should be OK with regular equality
    token$value == val
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
            stop(paste0(
                "Got pseudo-element ::",
                pseudo_element,
                " not at the end of a selector"))
        }
        if (peek$is_delim(c("+", ">", "~"))) {
            # A combinator
            combinator <- stream$nxt()$value
            stream$skip_whitespace()
        } else {
            # By exclusion, the last parse_simple_selector() ended
            # at peek == ' '
            combinator <- ' '
        }
        stuff <- parse_simple_selector(stream)
        pseudo_element <- stuff$pseudo_element
        result <- CombinedSelector$new(result, combinator, stuff$result)
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_simple_selector <- function(stream, inside_negation = FALSE) {
    stream$skip_whitespace()
    selector_start <- length(stream$used)
    peek <- stream$peek()
    if (peek$type == "IDENT" || token_equality(peek, "DELIM", "*")) {
        if (peek$type == "IDENT") {
            namespace <- stream$nxt()$value
        } else {
            stream$nxt()
            namespace <- NULL
        }
        if (token_equality(stream$peek(), "DELIM", "|")) {
            stream$nxt()
            element <- stream$next_ident_or_star()
        } else {
            element <- namespace
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
            peek$is_delim(c(",", "+", ">", "~")) ||
            (inside_negation && token_equality(peek, "DELIM", ")"))) {
            break
        }
        if (!is.null(pseudo_element)) {
            stop(paste0(
                "Got pseudo-element ::",
                pseudo_element,
                " not at the end of a selector"))
        }
        if (peek$type == "HASH") {
            result <- Hash$new(result, stream$nxt()$value)
        } else if (token_equality(peek, "DELIM", ".")) {
            stream$nxt()
            result <- ClassSelector$new(result, stream$next_ident())
        } else if (token_equality(peek, "DELIM", "|")) {
            stream$nxt()
            result <- Element$new(element = stream$next_ident())
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
                if (inside_negation) {
                    stop("Got nested :not()")
                }
                res <- parse_simple_selector(stream, inside_negation = TRUE)
                argument <- res$result
                argument_pseudo_element <- res$pseudo_element
                stream$skip_whitespace()
                nt <- stream$nxt()
                if (length(argument_pseudo_element) &&
                    nzchar(argument_pseudo_element)) {
                    stop(paste0(
                        "Got pseudo-element ::",
                        argument_pseudo_element,
                        " inside :not() at ",
                        nt$pos))
                }
                if (!token_equality(nt, "DELIM", ")")) {
                    stop(paste0("Expected ')', got ", nt$value))
                }
                result <- Negation$new(result, argument)
            } else {
                arguments <- list()
                i <- 1
                while (TRUE) {
                    nt <- stream$nxt()
                    if (nt$type %in% c("IDENT", "STRING", "NUMBER") ||
                        (token_equality(nt ,"DELIM", "+") ||
                         token_equality(nt, "DELIM", "-"))) {
                        arguments[[i]] <- nt
                        i <- i + 1
                    } else if (nt$type == "S") {
                        next
                    } else if (token_equality(nt, "DELIM", ")")) {
                        break
                    } else {
                        stop(paste0("Expected an argument, got ", nt$repr()))
                    }
                }
                if (length(arguments) == 0) {
                    stop(paste0(
                        "Expected at least one argument, got ", nt$repr()))
                }
                result <- Function$new(result, ident, arguments)
            }
        } else {
            stop(paste0("Expected selector, got ", stream$peek()$repr()))
        }
    }
    if (length(stream$used) == selector_start) {
        stop(paste0("Expected selector, got ", stream$peek()$repr()))
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_attrib <- function(selector, stream) {
    stream$skip_whitespace()
    attrib <- stream$next_ident_or_star()
    if (is.null(attrib) && !token_equality(stream$peek(), "DELIM", "|"))
        stop(paste0("Expected '|', got ", stream$peek()$repr()))
    if (token_equality(stream$peek(), "DELIM", "|")) {
        stream$nxt()
        namespace <- attrib
        attrib <- stream$next_ident()
        op <- NULL
    } else if (token_equality(stream$peek(), "DELIM", "|=")) {
        namespace <- NULL
        stream$nxt()
        op <- "|="
    } else {
        namespace <- op <- NULL
    }
    if (is.null(op)) {
        stream$skip_whitespace()
        nt <- stream$nxt()
        if (token_equality(nt, "DELIM", "]")) {
            return(Attrib$new(selector, namespace, attrib, "exists", NULL))
        } else if (token_equality(nt, "DELIM", "=")) {
            op <- "="
        } else if (nt$is_delim(c("^=", "$=", "*=", "~=", "|=", "!="))) {
            op <- nt$value
        } else {
            stop(paste0("Operator expected, got ", nt$repr()))
        }
    }
    stream$skip_whitespace()
    value <- stream$nxt()
    if (!value$type %in% c("IDENT", "STRING")) {
        stop(paste0("Expected string or ident, got ", value$repr()))
    }
    stream$skip_whitespace()
    nt <- stream$nxt()
    if (!token_equality(nt, "DELIM", "]")) {
        stop(paste0("Expected ']', got ", nt$repr()))
    }
    Attrib$new(selector, namespace, attrib, op, value$value)
}

str_int <- function(s) {
    suppressWarnings(as.integer(s))
}

parse_series <- function(tokens) {
    for (token in tokens) {
        if (token$type == "STRING")
            stop("String tokens not allowed in series.")
    }
    s <- paste0(sapply(tokens, function(x) x$value), collapse = "")
    if (s == "odd")
        return(2:1)
    else if (s == "even")
        return(c(2, 0))
    else if (s == "n")
        return(1:0)
    if (is.na(str_locate(s, "n")[1, 1])) {
        result <- str_int(s)
        if (is.na(result)) {
            return(NULL)
        } else {
            return(c(0, result))
        }
    }
    ab <- str_split_fixed(s, "n", 2)[1,]
    a <- str_trim(ab[1])
    b <- str_trim(ab[2])

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

Token <- R6Class("Token",
    public = list(
        type = "",
        value = NULL,
        pos = 1,
        initialize = function(type = "", value = NULL, pos = 1) {
            self$type <- type
            self$value <- value
            self$pos <- pos
        },
        repr = function() {
            paste0("<", self$type, " '", self$value, "' at ", self$pos, ">")
        },
        is_delim = function(values) {
            self$type == "DELIM" && self$value %in% values
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    )
)

EOFToken <- R6Class("EOFToken",
    inherit = Token,
    public = list(
        initialize = function(pos = 1, type = "EOF", value = NULL) {
            super$initialize(type, value, pos)
        },
        repr = function() {
            paste0("<", self$type, " at ", self$pos, ">")
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    ))

compile_ <- function(pattern) {
    function(x) {
        str_locate(x, pattern)[1, ]
    }
}

delims_2ch <- c('~=', '|=', '^=', '$=', '*=', '::', '!=')
delims_1ch <- c('>', '+', '~', ',', '.', '*', '=', '[', ']', '(', ')', '|', ':', '#')
delim_escapes <- paste0("\\", delims_1ch, collapse = "|")
match_whitespace <- compile_('[ \t\r\n\f]+')
match_number <- compile_('[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)')
match_hash <- compile_(paste0("^#([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))
match_ident <- compile_(paste0("^([_a-zA-Z0-9-]|", nonascii, "|\\\\(?:", delim_escapes, "))+"))
match_string_by_quote <- list("'" = compile_(paste0("([^\n\r\f\\']|", TokenMacros$string_escape, ")*")),
                              '"' = compile_(paste0('([^\n\r\f\\"]|', TokenMacros$string_escape, ")*")))

# Substitution for escaped chars
sub_simple_escape <- function(x) gsub('\\\\(.)', "\\1", x)
sub_unicode_escape <- function(x) gsub(TokenMacros$unicode_escape, "\\1", x, ignore.case = TRUE)
sub_newline_escape <- function(x) gsub('\\\\(?:\n|\r\n|\r|\f)', "", x)

tokenize <- function(s) {
    pos <- 1
    i <- 1
    len_s <- nchar(s)
    results <- list()
    while (pos <= len_s) {
        ss <- substring(s, pos, len_s)
        match <- match_whitespace(ss)
        if (!is.na(match) && match[1] == 1) {
            results[[i]] <- Token$new("S", " ", pos)
            match_end <- match[2]
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_number(ss)
        if (!is.na(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            results[[i]] <- Token$new("NUMBER", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_ident(ss)
        if (!is.na(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            value <- sub_simple_escape(sub_unicode_escape(value))
            results[[i]] <- Token$new("IDENT", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_hash(ss)
        if (!is.na(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            value <- sub_simple_escape(sub_unicode_escape(value))
            hash_id <- substring(value, 2)
            results[[i]] <- Token$new("HASH", hash_id, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        # Testing presence of two char delims
        nc_inds <- seq_len(nchar(ss))
        if (length(nc_inds) %% 2 == 1)
            nc_inds <- c(nc_inds, length(nc_inds) + 1)
        split_ss_2ch <- substring(ss, nc_inds[(nc_inds %% 2) == 1],
                                      nc_inds[(nc_inds %% 2) == 0])
        delim_inds_2ch <- which(split_ss_2ch %in% delims_2ch)
        if (length(delim_inds_2ch) && delim_inds_2ch[1] == 1) {
            # We have a 2ch delim
            results[[i]] <- Token$new("DELIM", split_ss_2ch[1], pos)
            pos <- pos + 2
            i <- i + 1
            next
        }

        # Testing presence of single char delims
        split_ss_1ch <- substring(ss, nc_inds, nc_inds)
        delim_inds_1ch <- which(split_ss_1ch %in% delims_1ch)
        if (length(delim_inds_1ch) && delim_inds_1ch[1] == 1) {
            # We have a single char delim
            results[[i]] <- Token$new("DELIM", split_ss_1ch[1], pos)
            pos <- pos + 1
            i <- i + 1
            next
        }
        quote <- substring(s, pos, pos)
        if (quote %in% c("'", '"')) {
            ncs <- nchar(s)
            split_chars <- substring(s, (pos + 1):ncs, (pos + 1):ncs)
            matching_quotes <- which(split_chars == quote)
            is_escaped <- logical(length(matching_quotes))
            if (length(matching_quotes)) {
                for (j in seq_along(matching_quotes)) {
                    end_quote <- matching_quotes[j]
                    if (end_quote > 1) {
                        is_escaped[j] <- split_chars[end_quote - 1] == "\\"
                    }
                }
                if (all(is_escaped)) {
                    stop(paste0("Unclosed string at ", pos))
                }
                end_quote <- matching_quotes[min(which(!is_escaped))]
                value <- substring(s, pos + 1, pos + end_quote - 1)
                value <- sub_simple_escape(
                             sub_unicode_escape(
                                 sub_newline_escape(value)))
                results[[i]] <- Token$new("STRING", value, pos)
                pos <- pos + end_quote + 1 # one for each quote char
                i <- i + 1
            } else {
                stop(paste0("Unclosed string at ", pos))
            }
        }
        # Remove comments
        pos1 <- pos + 1
        if (substring(s, pos, pos1) == "/*") {
            rel_pos <- str_locate(ss, "\\*/")[1]
            pos <-
                if (is.na(rel_pos)) {
                    len_s + 1
                } else {
                    pos + rel_pos + 1
                }
            next
        }
        # Because we always call 'next', if we're here there must have
        # been an error
        tmp <- substring(ss, 1, 1)
        if (!tmp %in% c(delims_1ch, '"', "'")) {
            stop(paste0(
                "Unexpected character '",
                tmp,
                "' found at position ",
                pos))
        }
    }
    results[[i]] <- EOFToken$new(pos)
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
            nt <- self$tokens[[self$pos]]
            self$pos <- self$pos + 1
            nt
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
                stop(paste0("Expected ident, got ", nt$repr()))
            nt$value
        },
        next_ident_or_star = function() {
            nt <- self$nxt()
            if (nt$type == "IDENT")
                nt$value
            else if (token_equality(nt, "DELIM", "*"))
                NULL
            else
                stop(paste0("Expected ident or '*', got ", nt$repr()))
        },
        skip_whitespace = function() {
            peek <- self$peek()
            if (peek$type == "S")
                self$nxt()
        }
    )
)
