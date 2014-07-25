XPathExpr <- setRefClass("XPathExpr",
                         fields = c("path", "element", "condition", "star_prefix"),
                         methods = list(
    initialize = function(path = "", element = "*", condition = "", star_prefix = FALSE) {
        path <<- path
        element <<- element
        condition <<- condition
        star_prefix <<- star_prefix
    },
    str = function() {
        p <- paste0(path, element)
        if (nchar(condition))
            p <- paste0(p, sprintf("[%s]", condition))
        p
    },
    repr = function() {
        sprintf("%s[%s]", class(.self), .self$str())
    },
    add_condition = function(condition) {
        if (nchar(.self$condition))
            condition <<- sprintf("%s and (%s)", .self$condition, condition)
        else
            condition <<- condition
    },
    add_name_test = function() {
        if (element == "*")
            return()
        gt <- GenericTranslator$new()
        add_condition(sprintf("name() = %s", gt$xpath_literal(element)))
        element <<- "*"
    },
    add_star_prefix = function() {
        path <<- paste0(path, "*/")
    },
    join = function(combiner, other) {
        p <- paste0(.self$str(), combiner)
        if (other$path != "*/")
            p <- paste0(p, other$path)
        path <<- p
        element <<- other$element
        condition <<- other$condition
        .self
    },
    show = function() {
        cat(.self$repr(), "\n")
    }))

is_safe_name <- function(name) {
    grepl("^[a-zA-Z_][a-zA-Z0-9_.-]*$", name)
}

GenericTranslator <- setRefClass("GenericTranslator",
                                 fields = c("combinator_mapping",
                                            "attribute_operator_mapping",
                                            "id_attribute",
                                            "lang_attribute",
                                            "lower_case_element_names",
                                            "lower_case_attribute_names",
                                            "lower_case_attribute_values"),
                                 methods = list(
    initialize = function() {
        combinator_mapping <<- c(" " = "descendant",
                                 ">" = "child",
                                 "+" = "direct_adjacent",
                                 "~" = "indirect_adjacent")
        # != is not in Selectors Level 3, but included anyway
        attribute_operator_mapping <<- c("exists" = "exists",
                                         "=" = "equals",
                                         "~=" = "includes",
                                         "|=" = "dashmatch",
                                         "^=" = "prefixmatch",
                                         "$=" = "suffixmatch",
                                         "*=" = "substringmatch",
                                         "!=" = "different")
        id_attribute <<- "id"
        lang_attribute <<- "xml:lang"
        lower_case_element_names <<- FALSE
        lower_case_attribute_names <<- FALSE
        lower_case_attribute_values <<- FALSE
    },
    css_to_xpath = function(css, prefix = "descendant-or-self::") {
        selectors <- parse(css)

        lapply(selectors, function(selector) {
            if (class(selector) == "Selector" && ! is.null(selector$pseudo_element))
                stop("Pseudo-elements are not supported.")
        })

        char_selectors <-
            if (is.list(selectors))
                sapply(selectors, function(selector) {
                    selector_to_xpath(selector, prefix)
                })
            else
                selector_to_xpath(selectors, prefix)

        paste0(char_selectors, collapse = " | ")
    },
    selector_to_xpath = function(selector, prefix = "descendant-or-self::") {
        tree <- selector$parsed_tree
        xpath <- .self$xpath(tree)
        if (! inherits(xpath, "XPathExpr"))
            stop("'xpath' is not an instance of 'XPathExpr'")
        paste0(if (! is.null(prefix)) prefix else "", xpath$str())
    },
    xpath_literal = function(s) {
        lenseq <- 1:nchar(s)
        split_chars <- substring(s, lenseq, lenseq)

        if (! any(split_chars == "'")) {
            s <- paste0("'", s, "'")
        } else if (! any(split_chars == '"')) {
            s <- paste0('"', s, '"')
        } else {
            dq_inds <- which(split_chars == "'")
            sq_inds <- which(split_chars != "'")
            split_chars[dq_inds] <- paste0('"', split_chars[dq_inds], '"')
            split_chars[sq_inds] <- paste0("'", split_chars[sq_inds], "'")

            literal <- paste(split_chars, collapse = ",")
            s <- sprintf("concat(%s)", literal)
        }

        s
    },
    xpath = function(parsed_selector) {
        usingMethods("xpath_attrib", "xpath_class", "xpath_combinedselector",
                     "xpath_element", "xpath_function", "xpath_hash",
                     "xpath_negation", "xpath_pseudo")
        type_name <- class(parsed_selector)
        method <- sprintf("xpath_%s", tolower(type_name))
        do.call(method, list(parsed_selector))
    },
    xpath_combinedselector = function(combined) {
        usingMethods("xpath_descendant_combinator", "xpath_child_combinator",
                     "xpath_direct_adjacent_combinator", "xpath_indirect_adjacent_combinator")
        combinator <- sprintf("xpath_%s_combinator",
                              combinator_mapping[combined$combinator])
        do.call(combinator, list(left = .self$xpath(combined$selector),
                                 right = .self$xpath(combined$subselector)))
    },
    xpath_negation = function(negation) {
        xpath <- .self$xpath(negation$selector)
        sub_xpath <- .self$xpath(negation$subselector)
        sub_xpath$add_name_test()
        if (! is.null(sub_xpath$condition) && nchar(sub_xpath$condition)) {
            xpath$add_condition(sprintf("not(%s)", sub_xpath$condition))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_function = function(fn) {
        usingMethods("xpath_contains_function", "xpath_lang_function",
                     "xpath_nth_child_function", "xpath_nth_last_child_function",
                     "xpath_nth_of_type_function", "xpath_nth_last_of_type_function")
        method <- sprintf("xpath_%s_function", gsub("-", "_", fn$name))
        if (! exists(method))
            stop(sprintf("The pseudo-class :%s() is unknown", fn$name))
        do.call(method, list(xpath(fn$selector), fn))
    },
    xpath_pseudo = function(pseudo) {
        usingMethods("xpath_root_pseudo", "xpath_first_child_pseudo",
                     "xpath_last_child_pseudo", "xpath_first_of_type_pseudo",
                     "xpath_last_of_type_pseudo", "xpath_only_child_pseudo",
                     "xpath_only_of_type_pseudo", "xpath_empty_pseudo",
                     "xpath_link_pseudo", "xpath_visited_pseudo",
                     "xpath_hover_pseudo", "xpath_active_pseudo",
                     "xpath_focus_pseudo", "xpath_target_pseudo",
                     "xpath_enabled_pseudo", "xpath_disabled_pseudo",
                     "xpath_checked_pseudo")
        method <- sprintf("xpath_%s_pseudo", gsub("-", "_", pseudo$ident))
        if (! exists(method))
            stop(sprintf("The pseudo-class :%s is unknown", pseudo$ident))
        do.call(method, list(xpath(pseudo$selector)))
    },
    xpath_attrib = function(selector) {
        usingMethods("xpath_attrib_dashmatch", "xpath_attrib_different",
                     "xpath_attrib_equals", "xpath_attrib_exists",
                     "xpath_attrib_prefixmatch", "xpath_attrib_substringmatch",
                     "xpath_attrib_suffixmatch")
        operator <- attribute_operator_mapping[selector$operator]
        method <- sprintf("xpath_attrib_%s", operator)
        if (lower_case_attribute_names) {
            name <- tolower(selector$attrib)
        } else {
            name <- selector$attrib
        }
        safe <- is_safe_name(name)
        if (! is.null(selector$namespace)) {
            name <- sprintf("%s:%s", selector$namespace, name)
        }
        if (safe) {
            attrib <- paste0("@", name)
        } else {
            attrib <- sprintf("attribute::*[name() = %s]",
                              xpath_literal(name))
        }
        if (lower_case_attribute_names) {
            value <- tolower(selector$value)
        } else {
            value <- selector$value
        }
        do.call(method, list(xpath(selector$selector), attrib, value))
    },
    # .foo is defined as [class~=foo] in the spec
    xpath_class = function(class_selector) {
        xpath <- xpath(class_selector$selector)
        xpath_attrib_includes(xpath, "@class", class_selector$class_name)
        xpath
    },
    xpath_hash = function(id_selector) {
        xpath <- xpath(id_selector$selector)
        xpath_attrib_equals(xpath, "@id", id_selector$id)
        xpath
    },
    xpath_element = function(selector) {
        element <- selector$element
        if (is.null(element)) {
            element <- "*"
            safe <- TRUE
        } else {
            safe <- is_safe_name(element)
            if (lower_case_element_names)
                element <- tolower(element)
        }
        if (! is.null(selector$namespace)) {
            # Namespace prefixes are case-sensitive.
            # http://www.w3.org/TR/css3-namespace/#prefixes
            element <- sprintf("%s:%s", selector$namespace, element)
            safe <- safe && is_safe_name(selector$namespace)
        }
        xpath <- XPathExpr$new(element = element)
        if (! safe)
            xpath$add_name_test()
        xpath
    },
    xpath_descendant_combinator = function(left, right) {
        left$join("/descendant-or-self::*/", right)
    },
    xpath_child_combinator = function(left, right) {
        left$join("/", right)
    },
    xpath_direct_adjacent_combinator = function(left, right) {
        xpath <- left$join("/following-sibling::", right)
        xpath$add_name_test()
        xpath$add_condition("position() = 1")
        xpath
    },
    xpath_indirect_adjacent_combinator = function(left, right) {
        left$join("/following-sibling::", right)
    },
    xpath_nth_child_function = function(xpath, fn, last = FALSE, add_name_test = TRUE) {
        ab <- parse_series(fn$arguments)
        a <- ab[1]
        b <- ab[2]
        if (add_name_test) {
            xpath$add_name_test()
        }
        xpath$add_star_prefix()
        # non-last
        # --------
        #    position() = an+b
        # -> position() - b = an
        #
        # if a < 0:
        #    position() - b < 0
        # -> position() < b
        #
        # last
        # ----
        #    last() - position() = an+b -1
        # -> last() - position() - b +1 = an
        #
        # if a < 0:
        #    last() - position() - b +1 < 0
        # -> position() > last() - b +1
        #
        if (b > 0) {
            b_neg <- as.character(-b)
        } else {
            b_neg <- sprintf("+%s", -b)
        }
        if (a == 0) {
            if (last) {
                # http://www.w3.org/TR/selectors/#nth-last-child-pseudo
                # The :nth-last-child(an+b) pseudo-class notation represents
                # an element that has an+b-1 siblings after it in the document tree
                #
                #    last() - position() = an+b-1
                # -> position() = last() -b +1 (for a==0)
                #
                if (b == 1) {
                    b <- "last()"
                } else {
                    b <- sprintf("last() %s +1", b_neg)
                }
            }
            xpath$add_condition(sprintf("position() = %s", b))
            return(xpath)
        }
        if (a != 1) {
            if (last) {
                if (b == 0) {
                    expr <- sprintf("(last() - position() +1) mod %s = 0", a)
                } else {
                    expr <- sprintf("(last() - position() %s +1) mod %s = 0",
                                    b_neg, a)
                }
            } else {
                if (b == 0) {
                    expr <- sprintf("position() mod %s = 0", a)
                } else {
                    expr <- sprintf("(position() %s) mod %s = 0", b_neg, a)
                }
            }
        } else {
            expr <- character(0)
        }
        if (last) {
            tmpop <- if (a > 0) "<=" else ">="
            if (b == 0) {
                expr <- c(expr, sprintf("(position() %s last() +1)", tmpop))
            } else {
                expr <- c(expr, sprintf("position() %s (last() %s +1)", tmpop, b_neg))
            }
        } else {
            tmpop <- if (a > 0) ">=" else "<="
            if (b > 0) {
                # position() > 0 so if b < 0, position() > b, always
                expr <- c(expr, sprintf("position() %s %s", tmpop, b))
            } else if (b == 0) {
                expr <- c(expr, "position()")
            }
        }
        expr <- paste0(expr, collapse = " and ")
        if (length(expr)) {
            xpath$add_condition(expr)
        }
        xpath
        # FIXME: handle an+b, odd, even
        # an+b means every-a, plus b, e.g., 2n+1 means odd
        # 0n+b means b
        # n+0 means a=1, i.e., all elements
        # an means every a elements, i.e., 2n means even
        # -n means -1n
        # -1n+6 means elements 6 and previous
    },
    xpath_nth_last_child_function = function(xpath, fn) {
        xpath_nth_child_function(xpath, fn, last = TRUE)
    },
    xpath_nth_of_type_function = function(xpath, fn) {
        if (xpath$element == "*") {
            stop("*:nth-of-type() is not implemented")
        }
        xpath_nth_child_function(xpath, fn, add_name_test = FALSE)
    },
    xpath_nth_last_of_type_function = function(xpath, fn) {
        if (xpath$element == "*") {
            stop("*:nth-of-type() is not implemented")
        }
        xpath_nth_child_function(xpath, fn, last = TRUE, add_name_test = FALSE)
    },
    xpath_contains_function = function(xpath, fn) {
        if (! (fn$argument_types() %in% c("STRING", "IDENT"))) {
            stop(sprintf("Expected a single string or ident for :contains(), got %s",
                         paste0("(", paste0(fn$argument_types(), collapse = ", "), ")")))
        }
        value <- fn$arguments[[1]]$value
        xpath$add_condition(sprintf("contains(string(.), %s)", xpath_literal(value)))
        xpath
    },
    xpath_lang_function = function(xpath, fn) {
        if (! (fn$argument_types() %in% c("STRING", "IDENT"))) {
            stop(sprintf("Expected a single string or ident for :lang(), got %s",
                         fn$arguments[[1]]$repr()))
        }
        value <- fn$arguments[[1]]$value
        xpath$add_condition(sprintf("lang(%s)", xpath_literal(value)))
        xpath
    },
    xpath_root_pseudo = function(xpath) {
        xpath$add_condition("not(parent::*)")
        xpath
    },
    xpath_first_child_pseudo = function(xpath) {
        xpath$add_star_prefix()
        xpath$add_name_test()
        xpath$add_condition("position() = 1")
        xpath
    },
    xpath_last_child_pseudo = function(xpath) {
        xpath$add_star_prefix()
        xpath$add_name_test()
        xpath$add_condition("position() = last()")
        xpath
    },
    xpath_first_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:first-of-type is not implemented")
        }
        xpath$add_star_prefix()
        xpath$add_condition("position() = 1")
        xpath
    },
    xpath_last_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:last-of-type is not implemented")
        }
        xpath$add_star_prefix()
        xpath$add_condition("position() = last()")
        xpath
    },
    xpath_only_child_pseudo = function(xpath) {
        xpath$add_name_test()
        xpath$add_star_prefix()
        xpath$add_condition('last() = 1')
        xpath
    },
    xpath_only_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:only-of-type is not implemented")
        }
        xpath$add_condition("last() = 1")
        xpath
    },
    xpath_empty_pseudo = function(xpath) {
        xpath$add_condition("not(*) and not(string-length())")
        xpath
    },
    pseudo_never_matches = function(xpath) {
        xpath$add_condition("0")
        xpath
    },

    # All are pseudo_never_matches()
    xpath_link_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_visited_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_hover_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_active_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_focus_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_target_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_enabled_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_disabled_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_checked_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },

    xpath_attrib_exists = function(xpath, name, value) {
        xpath$add_condition(name)
        xpath
    },
    xpath_attrib_equals = function(xpath, name, value) {
        xpath$add_condition(sprintf("%s = %s", name, xpath_literal(value)))
        xpath
    },
    xpath_attrib_different = function(xpath, name, value) {
        if (! is.null(value)) {
            xpath$add_condition(sprintf("not(%s) or %s != %s",
                                        name, name, xpath_literal(value)))
        } else {
            xpath$add_condition(sprintf("%s != %s",
                                        name, xpath_literal(value)))
        }
        xpath
    },
    xpath_attrib_includes = function(xpath, name, value) {
        if (! is.null(value) && nzchar(value) &&
            grepl("^[^ \t\r\n\f]+$", value)) {
            xpath$add_condition(sprintf("%s and contains(concat(' ', normalize-space(%s), ' '), %s)",
                                        name, name, xpath_literal(paste0(" ", value, " "))))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_attrib_dashmatch = function(xpath, name, value) {
        if (! is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and (%s = %s or starts-with(%s, %s))",
                                        name, name, xpath_literal(value),
                                        name, xpath_literal(paste0(value, "-"))))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_attrib_prefixmatch = function(xpath, name, value) {
        if (! is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and starts-with(%s, %s)",
                                        name, name, xpath_literal(value)))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    # In XPath there is starts-with but not ends-with, hence the oddness
    xpath_attrib_suffixmatch = function(xpath, name, value) {
        if (! is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and substring(%s, string-length(%s)-%s) = %s",
                                        name, name, name, nchar(value) - 1, xpath_literal(value)))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_attrib_substringmatch = function(xpath, name, value) {
        if (! is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and contains(%s, %s)",
                                        name, name, xpath_literal(value)))
        } else {
            xpath$add_condition("0")
        }
        xpath
    }))

HTMLTranslator <- setRefClass("HTMLTranslator",
                              contains = "GenericTranslator",
                              fields = "xhtml",
                              methods = list(
    initialize = function(xhtml = FALSE, ...) {
        callSuper(...)
        xhtml <<- xhtml
        if (! xhtml) {
            lower_case_element_names <<- TRUE
            lower_case_attribute_names <<- TRUE
        }
        lang_attribute <<- "lang"
    },
    xpath_checked_pseudo = function(xpath) {
        xpath$add_condition(
            paste0("(@selected and name(.) = 'option') or ",
                   "(@checked ",
                   "and (name(.) = 'input' or name(.) = 'command')",
                   "and (@type = 'checkbox' or @type = 'radio'))"))
        xpath
    },
    xpath_lang_function = function(xpath, fn) {
        if (! (fn$argument_types() %in% c("STRING", "IDENT"))) {
            stop(sprintf("Expected a single string or ident for :lang(), got %s",
                         fn$arguments[[1]]$repr()))
        }
        value <- fn$arguments[[1]]$value
        xpath$add_condition(sprintf(paste0("ancestor-or-self::*[@lang][1][starts-with(concat(",
                                           "translate(@%s, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                                                          "'abcdefghijklmnopqrstuvwxyz'), ",
                                           "'-'), %s)]", collapse = ""),
                                    lang_attribute, xpath_literal(paste0(tolower(value), "-"))))
        xpath
    },
    xpath_link_pseudo = function(xpath) {
        xpath$add_condition("@href and (name(.) = 'a' or name(.) = 'link' or name(.) = 'area')")
        xpath
    },
    xpath_disabled_pseudo = function(xpath) {
        xpath$add_condition(
            paste("(",
                  "@disabled and",
                  "(",
                  "(name(.) = 'input' and @type != 'hidden') or",
                  "name(.) = 'button' or",
                  "name(.) = 'select' or",
                  "name(.) = 'textarea' or",
                  "name(.) = 'command' or",
                  "name(.) = 'fieldset' or",
                  "name(.) = 'optgroup' or",
                  "name(.) = 'option'",
                  ")",
                  ") or (",
                  "(",
                  "(name(.) = 'input' and @type != 'hidden') or",
                  "name(.) = 'button' or",
                  "name(.) = 'select' or",
                  "name(.) = 'textarea'",
                  ")",
                  "and ancestor::fieldset[@disabled]",
                  ")"))
        xpath
    },
    xpath_enabled_pseudo = function(xpath) {
        xpath$add_condition(
            paste("(@href and (name(.) = 'a' or name(.) = 'link' or name(.) = 'area'))",
                  "or",
                  "((name(.) = 'command' or name(.) = 'fieldset' or name(.) = 'optgroup') and not(@disabled))",
                  "or",
                  "(((name(.) = 'input' and @type != 'hidden')",
                  "or name(.) = 'button'",
                  "or name(.) = 'select'",
                  "or name(.) = 'textarea'",
                  "or name(.) = 'keygen')",
                  "and not (@disabled or ancestor::fieldset[@disabled]))",
                  "or (name(.) = 'option' and not(@disabled or ancestor::optgroup[@disabled]))"))
        xpath
    }))
