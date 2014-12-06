escape <- paste0("([0-9a-f]{1,6})(\r\n|[ \n\r\t\f])?", "|[^\n\r\f0-9a-f]")
nonascii <- "[^\1-\177]"
hash_re <- "([_a-z0-9-]|([0-9a-f]{1,6})(\r\n|[ \n\r\t\f])?|[^\1-\177])"

TokenMacros <- list(unicode_escape = "\\([0-9a-f]{1,6})(?:\r\n|[ \n\r\t\f])?",
                    escape = escape,
                    string_escape = paste0("\\(?:\n|\r\n|\r|\f)|", escape),
                    nonascii = nonascii,
                    nmchar = sprintf("([_a-z0-9-]|%s|%s)", escape, nonascii),
                    nmstart = sprintf("[_a-z]|%s|%s", escape, nonascii))

Selector <- setRefClass("Selector",
                        fields = c("parsed_tree", "pseudo_element"),
                        methods = list(
    initialize = function(tree, pseudo_element = NULL) {
        parsed_tree <<- tree
        if (! is.null(pseudo_element))
            pseudo_element <<- tolower(pseudo_element)
        else
            pseudo_element <<- pseudo_element
    },
    repr = function() {
        pseudo_el <-
            if (is.null(pseudo_element))
                ""
            else
                sprintf("::%s", pseudo_element)
        sprintf("%s%s",
                parsed_tree$repr(),
                pseudo_el)
    },
    specificity = function() {
        specs <- parsed_tree$specificity()
        if (! is.null(pseudo_element))
            specs[3] <- specs[3] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Class <- setRefClass("Class",
                     fields = c("selector", "class_name"),
                     methods = list(
    initialize = function(selector, class_name) {
        selector <<- selector
        class_name <<- class_name
    },
    repr = function() {
        sprintf("%s[%s.%s]",
                class(.self),
                selector$repr(),
                class_name)
    },
    specificity = function() {
        specs <- selector$specificity()
        specs[2] <- specs[2] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Function <- setRefClass("Function",
                        fields = c("selector", "name", "arguments"),
                        methods = list(
    initialize = function(selector, name, arguments) {
        selector <<- selector
        name <<- tolower(name)
        arguments <<- arguments
    },
    repr = function() {
        token_values <- lapply(arguments, function(token) {
            paste0("'", token$value, "'")
        })
        token_values <- paste0(unlist(token_values), collapse = ", ")
        token_values <- paste0("[", token_values, "]")
        sprintf("%s[%s:%s(%s)]",
                class(.self),
                selector$repr(),
                name,
                token_values)
    },
    argument_types = function() {
        token_types <- lapply(arguments, function(token) {
            token$type
        })
        unlist(token_types)
    },
    specificity = function() {
        specs <- selector$specificity()
        specs[2] <- specs[2] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Pseudo <- setRefClass("Pseudo",
                      fields = c("selector", "ident"),
                      methods = list(
    initialize = function(selector, ident) {
        selector <<- selector
        ident <<- tolower(ident)
    },
    repr = function() {
        sprintf("%s[%s:%s]",
                class(.self), selector$repr(), ident)
    },
    specificity = function() {
        specs <- selector$specificity()
        specs[2] <- specs[2] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Negation <- setRefClass("Negation",
                        fields = c("selector", "subselector"),
                        methods = list(
    initialize = function(selector, subselector) {
        selector <<- selector
        subselector <<- subselector
    },
    repr = function() {
        sprintf("%s[%s:not(%s)]",
                class(.self), selector$repr(), subselector$repr())
    },
    specificity = function() {
        specs <- selector$specificity()
        sub_specs <- subselector$specificity()
        specs + sub_specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Attrib <- setRefClass("Attrib",
                      fields = c("selector", "namespace", "attrib",
                                 "operator", "value"),
                      methods = list(
    initialize = function(selector, namespace, attrib, operator, value) {
        selector <<- selector
        namespace <<- namespace
        attrib <<- attrib
        operator <<- operator
        value <<- value
    },
    repr = function() {
        attr <-
            if (! is.null(namespace))
                sprintf("%s|%s", namespace, attrib)
            else
                .self$attrib
        if (operator == "exists")
            sprintf("%s[%s[%s]]",
                    class(.self), selector$repr(), attr)
        else
            sprintf("%s[%s[%s %s '%s']]",
                    class(.self), selector$repr(), attr, operator, value)
    },
    specificity = function() {
        specs <- selector$specificity()
        specs[2] <- specs[2] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Element <- setRefClass("Element",
                       fields = c("namespace", "element"),
                       methods = list(
    initialize = function(namespace = NULL, element = NULL) {
        namespace <<- namespace
        element <<- element
    },
    repr = function() {
        el <-
            if (! is.null(.self$element))
                .self$element
            else
                "*"
        if (! is.null(namespace))
            el <- sprintf("%s|%s", namespace, el)
        sprintf("%s[%s]",
                class(.self), el)
    },
    specificity = function() {
        if (! is.null(element))
            c(0, 0, 1)
        else
            rep(0, 3)
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

Hash <- setRefClass("Hash",
                    fields = c("selector", "id"),
                    methods = list(
    initialize = function(selector, id) {
        selector <<- selector
        id <<- id
    },
    repr = function() {
        sprintf("%s[%s#%s]",
                class(.self), selector$repr(), id)
    },
    specificity = function() {
        specs <- selector$specificity()
        specs[1] <- specs[1] + 1
        specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

CombinedSelector <- setRefClass("CombinedSelector",
                                fields = c("selector", "combinator", "subselector"),
                                methods = list(
    initialize = function(selector, combinator, subselector) {
        if (is.null(selector))
            stop("'selector' cannot be NULL")
        selector <<- selector
        combinator <<- combinator
        subselector <<- subselector
    },
    repr = function() {
        comb <-
            if (combinator == " ")
                "<followed>"
            else
                combinator
        sprintf("%s[%s %s %s]",
                class(.self), selector$repr(), comb, subselector$repr())
    },
    specificity = function() {
        specs <- selector$specificity()
        sub_specs <- subselector$specificity()
        specs + sub_specs
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

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
    if (! is.na(el_match))
        return(Selector$new(Element$new(element = el_match)))
    id_match <- str_match(css, id_re)[1, 2:3]
    if (! is.na(id_match[2]))
        return(Selector$new(
                   Hash$new(
                       Element$new(element = if (nchar(id_match[1]) == 0) NULL else id_match[1]),
                       id_match[2])))
    class_match <- str_match(css, class_re)[1, 2:3]
    if (! is.na(class_match[3]))
        return(Selector$new(
                   Class$new(
                       Element$new(element = if (is.na(class_match[2])) NULL else class_match[2]),
                       class_match[3])))
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
        if (token_equality(peek, "EOF", NULL) || token_equality(peek, "DELIM", ",")) {
            break
        }
        if (! is.null(pseudo_element) && nchar(pseudo_element)) {
            stop(sprintf("Got pseudo-element ::%s not at the end of a selector",
                         pseudo_element))
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
        if (! is.null(pseudo_element)) {
            stop(sprintf("Got pseudo-element ::%s not at the end of a selector",
                         pseudo_element))
        }
        if (peek$type == "HASH") {
            result <- Hash$new(result, stream$nxt()$value)
        } else if (token_equality(peek, "DELIM", ".")) {
            stream$nxt()
            result <- Class$new(result, stream$next_ident())
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
                if (token_equality(stream$peek(), "DELIM", ":")) {
                    stream$nxt()
                    pseudo_element <- stream$next_ident()
                    next
                }
            }
            ident <- stream$next_ident()
            if (tolower(ident) %in% c("first-line", "first-letter", "before", "after")) {
                # Special case: CSS 2.1 pseudo-elements can have a single ':'
                # Any new pseudo-element must have two.
                pseudo_element <- ident
                next
            }
            if (! token_equality(stream$peek(), "DELIM", "(")) {
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
                nt <- stream$nxt()
                if (length(argument_pseudo_element) &&
                    nchar(argument_pseudo_element)) {
                    stop(sprintf("Got pseudo-element ::%s inside :not() at %s",
                                 argument_pseudo_element, nt$pos))
                }
                if (! token_equality(nt, "DELIM", ")")) {
                    stop(sprintf("Expected ')', got %s", nt$value))
                }
                result <- Negation$new(result, argument)
            } else {
                arguments <- list()
                i <- 1
                while (TRUE) {
                    nt <- stream$nxt()
                    if (nt$type %in% c("IDENT", "STRING", "NUMBER") ||
                        (token_equality(nt ,"DELIM", "+") || token_equality(nt, "DELIM", "-"))) {
                        arguments[[i]] <- nt
                        i <- i + 1
                    } else if (nt$type == "S") {
                        next
                    } else if (token_equality(nt, "DELIM", ")")) {
                        break
                    } else {
                        stop(sprintf("Expected an argument, got %s", nt$repr()))
                    }
                }
                if (length(arguments) == 0) {
                    stop(sprintf("Expected at least one argument, got %s", nt$repr()))
                }
                result <- Function$new(result, ident, arguments)
            }
        } else {
            stop(sprintf("Expected selector, got %s", stream$peek()$repr()))
        }
    }
    if (length(stream$used) == selector_start) {
        stop(sprintf("Expected selector, got %s", stream$peek()$repr()))
    }
    list(result = result, pseudo_element = pseudo_element)
}

parse_attrib <- function(selector, stream) {
    stream$skip_whitespace()
    attrib <- stream$next_ident_or_star()
    if (is.null(attrib) && ! token_equality(stream$peek(), "DELIM", "|"))
        stop(sprintf("Expected '|', got %s", stream$peek()$repr()))
    if (token_equality(stream$peek(), "DELIM", "|")) {
        stream$nxt()
        if (token_equality(stream$peek(), "DELIM", "=")) {
            namespace <- NULL
            stream$nxt()
            op <- "|="
        } else {
            namespace <- attrib
            attrib <- stream$next_ident()
            op <- NULL
        }
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
        } else if (nt$is_delim(c("^=", "$=", "*=", "~=", "|=", "!="))) {# &&
            op <- nt$value
        } else {
            stop(sprintf("Operator expected, got %s", nt$repr()))
        }
    }
    stream$skip_whitespace()
    value <- stream$nxt()
    if (! value$type %in% c("IDENT", "STRING")) {
        stop(sprintf("Expected string or ident, got %s", value$repr()))
    }
    stream$skip_whitespace()
    nt <- stream$nxt()
    if (! token_equality(nt, "DELIM", "]")) {
        stop(sprintf("Expected ']', got %s", nt$repr()))
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
    if (! nzchar(a) && is.na(intb))
        return(NULL)

    if (! nzchar(a))
        a <- 1
    else if (a == "-" || a == "+")
        a <- str_int(paste0(a, "1"))
    else
        a <- str_int(a)
    if (! nzchar(b))
        b <- 0
    else
        b <- str_int(b)
    c(a, b)
}

Token <- setRefClass("Token",
                     fields = c("type", "value", "pos"),
                     methods = list(
    initialize = function(type = "", value = NULL, pos = 1) {
        type <<- type
        value <<- value
        pos <<- pos
    },
    repr = function() {
        sprintf("<%s '%s' at %i>", type, value, pos)
    },
    is_delim = function(values) {
        type == "DELIM" && value %in% values
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

EOFToken <- setRefClass("EOFToken",
                        contains = "Token",
                        methods = list(
    initialize = function(pos = 1, type = "EOF", value = NULL) {
        callSuper(type, value, pos)
    },
    repr = function() {
        sprintf("<%s at %i>", type, pos)
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

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
match_hash <- compile_(sprintf("^#([_a-zA-Z0-9-]|%s|\\\\(?:%s))+", nonascii, delim_escapes))#sprintf('#(?:%s)+', TokenMacros$nmchar))
match_ident <- compile_(sprintf("^([_a-zA-Z0-9-]|%s|\\\\(?:%s))+", nonascii, delim_escapes))#"^[\\w_-]+")
#match_ident <- compile_(sprintf('-?%s|%s*',
#                                TokenMacros$nmstart, TokenMacros$nmchar))
match_string_by_quote <- list("'" = compile_(sprintf("([^\n\r\f\\']|%s)*",
                                                     TokenMacros$string_escape)),
                              '"' = compile_(sprintf('([^\n\r\f\\"]|%s)*',
                                             TokenMacros$string_escape)))

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
        if (! is.na(match) && match[1] == 1) {
            results[[i]] <- Token$new("S", " ", pos)
            match_end <- match[2]
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_number(ss)
        if (! is.na(match) && match[1] == 1) {
            match_start <- match[1]
            match_end <- max(match[1], match[2])
            value <- substring(ss, match_start, match_end)
            results[[i]] <- Token$new("NUMBER", value, pos)
            pos <- pos + match_end
            i <- i + 1
            next
        }
        match <- match_ident(ss)
        if (! is.na(match) && match[1] == 1) {
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
        if (! is.na(match) && match[1] == 1) {
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
        nc_inds <- 1:nchar(ss)
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
            split_chars <- substring(s, (pos + 1):nchar(s), (pos + 1):nchar(s))
            matching_quotes <- which(split_chars == quote)
            is_escaped <- logical(length(matching_quotes))
            if (length(matching_quotes)) {
                for (j in 1:length(matching_quotes)) {
                    end_quote <- matching_quotes[j]
                    if (end_quote > 1) {
                        is_escaped[j] <- split_chars[end_quote - 1] == "\\"
                    }
                }
                if (all(is_escaped)) {
                    stop(sprintf("Unclosed string at %d", pos))
                }
                end_quote <- matching_quotes[min(which(! is_escaped))]
                value <- substring(s, pos + 1, pos + end_quote - 1)
                value <- sub_simple_escape(
                             sub_unicode_escape(
                                 sub_newline_escape(value)))
                results[[i]] <- Token$new("STRING", value, pos)
                pos <- pos + end_quote + 1 # one for each quote char
                i <- i + 1
            } else {
                stop(sprintf("Unclosed string at %d", pos))
            }
        }
        # Remove comments
        pos1 <- pos + 1
        if (substring(s, pos, pos1) == "/*") {
            rel_pos <- str_locate(ss, "\\*/")[1]
            pos <-
                if (is.na(pos)) {
                    len_s
                } else {
                    pos + rel_pos + 1
                }
            next
        }
        # Because we always call 'next', if we're here there must have
        # been an error
        tmp <- substring(ss, 1, 1)
        if (! tmp %in% c(delims_1ch, '"', "'")) {
            stop(sprintf("Unexpected character '%s' found at position %d",
                         tmp, pos))
        }
    }
    results[[i]] <- EOFToken$new(pos)
    results
}

TokenStream <- setRefClass("TokenStream",
                           fields = c("used", "tokens", "source_text", "peeked", "peeking", "pos", "ntokens"),
                           methods = list(
    initialize = function(tokens, source_text = NULL) {
        pos <<- 1
        tokens <<- tokens
        ntokens <<- length(tokens)
        used <<- list()
        source_text <<- source_text
        peeked <<- list()
        peeking <<- FALSE
    },
    nxt = function() {
        if (peeking) {
            peeking <<- FALSE
            used[[pos]] <<- peeked
            peeked
        } else {
            nt <- next_token()
            used[[pos]] <<- nt
            nt
        }
    },
    next_token = function() {
        nt <- tokens[[pos]]
        pos <<- pos + 1
        nt
    },
    peek = function() {
        if (! peeking) {
            peeked <<- next_token()
            peeking <<- TRUE
        }
        peeked
    },
    next_ident = function() {
        nt <- .self$nxt()
        if (nt$type != "IDENT")
            stop(sprintf("Expected ident, got %s", nt$repr()))
        nt$value
    },
    next_ident_or_star = function() {
        nt <- .self$nxt()
        if (nt$type == "IDENT")
            return(nt$value)
        else if (token_equality(nt, "DELIM", "*"))
            return(NULL)
        else
            stop(sprintf("Expected ident or '*', got %s", nt$repr()))
    },
    skip_whitespace = function() {
        peek <- .self$peek()
        if (peek$type == "S")
            .self$nxt()
    }))
