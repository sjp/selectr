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
    join = function(combiner, other) {
        p <- paste0(.self$str(), combiner)
        if (other$path != "*/")
            p <- paste0(p, other$path)
        path <<- p
        element <<- other$element
        condition <<- other$condition
        .self
    },
    show = function() { # nocov start
        cat(.self$repr(), "\n")
    } # nocov end
    ))

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
        selectors <-
            if (is.null(selectors)) list()
            else if (!is.list(selectors)) list(selectors)
            else selectors

        lapply(selectors, function(selector) {
            if (class(selector) == "Selector" && !is.null(selector$pseudo_element))
                stop("Pseudo-elements are not supported.")
        })

        char_selectors <-
            sapply(selectors, function(selector) selector_to_xpath(selector, prefix))

        paste0(char_selectors, collapse = " | ")
    },
    selector_to_xpath = function(selector, prefix = "descendant-or-self::") {
        tree <- selector$parsed_tree
        xpath <- .self$xpath(tree)
        if (!inherits(xpath, "XPathExpr"))
            stop("'xpath' is not an instance of 'XPathExpr'")
        paste0(if (!is.null(prefix)) prefix else "", xpath$str())
    },
    xpath_literal = function(s) {
        lenseq <- seq_len(nchar(s))
        split_chars <- substring(s, lenseq, lenseq)

        if (!any(split_chars == "'")) {
            s <- paste0("'", s, "'")
        } else if (!any(split_chars == '"')) {
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
        method_name <- sprintf("xpath_%s", tolower(type_name))

        method <- .self[[method_name]]
        # manually bind method if not already bound
        if (is.null(method)) { # nocov start
            cls <- getClass(class(.self))
            cls_methods <- cls@refMethods
            method <- get(method_name, envir = cls_methods)
            .self[[method_name]] <- method
        } # nocov end

        do.call(method, list(parsed_selector))
    },
    xpath_combinedselector = function(combined) {
        usingMethods("xpath_descendant_combinator", "xpath_child_combinator",
                     "xpath_direct_adjacent_combinator", "xpath_indirect_adjacent_combinator")
        combinator <- sprintf("xpath_%s_combinator",
                              combinator_mapping[combined$combinator])

        method <- .self[[combinator]]
        # manually bind method if not already bound
        if (is.null(method)) { # nocov start
            cls <- getClass(class(.self))
            cls_methods <- cls@refMethods
            method <- get(combinator, envir = cls_methods)
            .self[[combinator]] <- method
        } # nocov end

        do.call(method, list(left = .self$xpath(combined$selector),
                             right = .self$xpath(combined$subselector)))
    },
    xpath_negation = function(negation) {
        xpath <- .self$xpath(negation$selector)
        sub_xpath <- .self$xpath(negation$subselector)
        sub_xpath$add_name_test()
        if (!is.null(sub_xpath$condition) && nchar(sub_xpath$condition)) {
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
        method_name <- sprintf("xpath_%s_function", gsub("-", "_", fn$name))

        method <- .self[[method_name]]
        # manually bind method if not already bound
        if (is.null(method)) { # nocov start
            cls <- getClass(class(.self))
            cls_methods <- cls@refMethods
            method <- get(method_name, envir = cls_methods)
            .self[[method_name]] <- method
        } # nocov end

        if (is.null(method))
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
        method_name <- sprintf("xpath_%s_pseudo", gsub("-", "_", pseudo$ident))

        method <- .self[[method_name]]
        # manually bind method if not already bound
        if (is.null(method)) { # nocov start
            cls <- getClass(class(.self))
            cls_methods <- cls@refMethods
            method <- get(method_name, envir = cls_methods)
            .self[[method_name]] <- method
        } # nocov end

        if (is.null(method))
            stop(sprintf("The pseudo-class :%s is unknown", pseudo$ident))

        do.call(method, list(xpath(pseudo$selector)))
    },
    xpath_attrib = function(selector) {
        usingMethods("xpath_attrib_dashmatch", "xpath_attrib_different",
                     "xpath_attrib_equals", "xpath_attrib_exists",
                     "xpath_attrib_prefixmatch", "xpath_attrib_substringmatch",
                     "xpath_attrib_suffixmatch")
        operator <- attribute_operator_mapping[selector$operator]
        method_name <- sprintf("xpath_attrib_%s", operator)
        if (lower_case_attribute_names) {
            name <- tolower(selector$attrib)
        } else {
            name <- selector$attrib
        }
        safe <- is_safe_name(name)
        if (!is.null(selector$namespace)) {
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

        method <- .self[[method_name]]
        # manually bind method if not already bound
        if (is.null(method)) { # nocov start
            cls <- getClass(class(.self))
            cls_methods <- cls@refMethods
            method <- get(method_name, envir = cls_methods)
            .self[[method_name]] <- method
        } # nocov end

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
        if (!is.null(selector$namespace)) {
            # Namespace prefixes are case-sensitive.
            # http://www.w3.org/TR/css3-namespace/#prefixes
            element <- sprintf("%s:%s", selector$namespace, element)
            safe <- safe && is_safe_name(selector$namespace)
        }
        xpath <- XPathExpr$new(element = element)
        if (!safe)
            xpath$add_name_test()
        xpath
    },
    xpath_descendant_combinator = function(left, right) {
        left$join("/descendant::", right)
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

        # From https://www.w3.org/TR/css3-selectors/#structural-pseudos:
        #
        # :nth-child(an+b)
        #       an+b-1 siblings before
        #
        # :nth-last-child(an+b)
        #       an+b-1 siblings after
        #
        # :nth-of-type(an+b)
        #       an+b-1 siblings with the same expanded element name before
        #
        # :nth-last-of-type(an+b)
        #       an+b-1 siblings with the same expanded element name after
        #
        # So,
        # for :nth-child and :nth-of-type
        #
        #    count(preceding-sibling::<nodetest>) = an+b-1
        #
        # for :nth-last-child and :nth-last-of-type
        #
        #    count(following-sibling::<nodetest>) = an+b-1
        #
        # therefore,
        #    count(...) - (b-1) = 0 (mod a)
        #
        # if a == 0:
        # ~~~~~~~~~~
        #    count(...) = b-1
        #
        # if a < 0:
        # ~~~~~~~~~
        #    count(...) - b +1 <= 0
        # -> count(...) <= b-1
        #
        # if a > 0:
        # ~~~~~~~~~
        #    count(...) - b +1 >= 0
        # -> count(...) >= b-1

        # work with b-1 instead
        b_min_1 <- b - 1

        # early-exit condition 1:
        # ~~~~~~~~~~~~~~~~~~~~~~~
        # for a == 1, nth-*(an+b) means n+b-1 siblings before/after,
        # and since n %in% {0, 1, 2, ...}, if b-1<=0,
        # there is always an "n" matching any number of siblings (maybe none)
        if (a == 1 && b_min_1 <=0) {
            return(xpath)
        }
        # early-exit condition 2:
        # ~~~~~~~~~~~~~~~~~~~~~~~
        # an+b-1 siblings with a<0 and (b-1)<0 is not possible
        if (a < 0 && b_min_1 < 0) {
            xpath$add_condition("0")
            return(xpath)
        }

        # `add_name_test` boolean is inverted and somewhat counter-intuitive:
        #
        # nth_of_type() calls nth_child(add_name_test=False)
        if (add_name_test) {
            nodetest <- "*"
        } else {
            nodetest <- sprintf("%s", xpath$element)
        }

        # count siblings before or after the element
        if (!last) {
            siblings_count <- sprintf("count(preceding-sibling::%s)", nodetest)
        } else {
            siblings_count <- sprintf("count(following-sibling::%s)", nodetest)
        }

        # special case of fixed position: nth-*(0n+b)
        # if a == 0:
        # ~~~~~~~~~~
        #    count(***-sibling::***) = b-1
        if (a == 0) {
            xpath$add_condition(sprintf("%s = %s", siblings_count, b_min_1))
            return(xpath)
        }

        expr <- character(0)

        if (a > 0) {
            # siblings count, an+b-1, is always >= 0,
            # so if a>0, and (b-1)<=0, an "n" exists to satisfy this,
            # therefore, the predicate is only interesting if (b-1)>0
            if (b_min_1 > 0) {
                expr <- c(expr, sprintf("%s >= %s", siblings_count, b_min_1))
            }
        } else {
            # if a<0, and (b-1)<0, no "n" satisfies this,
            # this is tested above as an early exist condition
            # otherwise,
            expr <- c(expr, sprintf("%s <= %s", siblings_count, b_min_1))
        }

        # operations modulo 1 or -1 are simpler, one only needs to verify:
        #
        # - either:
        # count(***-sibling::***) - (b-1) = n = 0, 1, 2, 3, etc.,
        #   i.e. count(***-sibling::***) >= (b-1)
        #
        # - or:
        # count(***-sibling::***) - (b-1) = -n = 0, -1, -2, -3, etc.,
        #   i.e. count(***-sibling::***) <= (b-1)
        # we we just did above.
        #
        if (abs(a) != 1) {
            # count(***-sibling::***) - (b-1) = 0 (mod a)
            left <- siblings_count

            # apply "modulo a" on 2nd term, -(b-1),
            # to simplify things like "(... +6) % -3",
            # and also make it positive with |a|
            b_neg <- (-b_min_1) %% abs(a)

            if (b_neg != 0) {
                b_neg <- sprintf("+%s", b_neg)
                left <- sprintf("(%s %s)", left, b_neg)
            }

            expr <- c(expr, sprintf("%s mod %s = 0", left, a))
        }

        if (length(expr)) {
            expr <- paste0(expr, collapse = " and ")
            xpath$add_condition(expr)
        }
        xpath
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
            stop("*:nth-last-of-type() is not implemented")
        }
        xpath_nth_child_function(xpath, fn, last = TRUE, add_name_test = FALSE)
    },
    xpath_contains_function = function(xpath, fn) {
        if (!(fn$argument_types() %in% c("STRING", "IDENT"))) {
            stop(sprintf("Expected a single string or ident for :contains(), got %s",
                         paste0("(", paste0(fn$argument_types(), collapse = ", "), ")")))
        }
        value <- fn$arguments[[1]]$value
        xpath$add_condition(sprintf("contains(., %s)", xpath_literal(value)))
        xpath
    },
    xpath_lang_function = function(xpath, fn) {
        if (!(fn$argument_types() %in% c("STRING", "IDENT"))) {
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
        xpath$add_condition("count(preceding-sibling::*) = 0")
        xpath
    },
    xpath_last_child_pseudo = function(xpath) {
        xpath$add_condition("count(following-sibling::*) = 0")
        xpath
    },
    xpath_first_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:first-of-type is not implemented")
        }
        xpath$add_condition(sprintf("count(preceding-sibling::%s) = 0", xpath$element))
        xpath
    },
    xpath_last_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:last-of-type is not implemented")
        }
        xpath$add_condition(sprintf("count(following-sibling::%s) = 0", xpath$element))
        xpath
    },
    xpath_only_child_pseudo = function(xpath) {
        xpath$add_condition("count(parent::*/child::*) = 1")
        xpath
    },
    xpath_only_of_type_pseudo = function(xpath) {
        if (xpath$element == "*") {
            stop("*:only-of-type is not implemented")
        }
        xpath$add_condition(sprintf("count(parent::*/child::%s) = 1", xpath$element))
        xpath
    },
    xpath_empty_pseudo = function(xpath) {
        xpath$add_condition("not(*) and not(string-length())")
        xpath
    },

    #pseudo_never_matches = function(xpath) {
    #    xpath$add_condition("0")
    #    xpath
    #},

    # All are pseudo_never_matches()
    xpath_link_pseudo     = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_visited_pseudo  = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_hover_pseudo    = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_active_pseudo   = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_focus_pseudo    = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_target_pseudo   = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_enabled_pseudo  = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_disabled_pseudo = function(xpath) { xpath$add_condition("0") ; xpath },
    xpath_checked_pseudo  = function(xpath) { xpath$add_condition("0") ; xpath },

    xpath_attrib_exists = function(xpath, name, value) {
        xpath$add_condition(name)
        xpath
    },
    xpath_attrib_equals = function(xpath, name, value) {
        xpath$add_condition(sprintf("%s = %s", name, xpath_literal(value)))
        xpath
    },
    xpath_attrib_different = function(xpath, name, value) {
        xpath$add_condition(sprintf("not(%s) or %s != %s",
                                    name, name, xpath_literal(value)))
        xpath
    },
    xpath_attrib_includes = function(xpath, name, value) {
        if (!is.null(value) && nzchar(value) &&
            grepl("^[^ \t\r\n\f]+$", value)) {
            xpath$add_condition(sprintf("%s and contains(concat(' ', normalize-space(%s), ' '), %s)",
                                        name, name, xpath_literal(paste0(" ", value, " "))))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_attrib_dashmatch = function(xpath, name, value) {
        xpath$add_condition(sprintf("%s and (%s = %s or starts-with(%s, %s))",
                                    name, name, xpath_literal(value),
                                    name, xpath_literal(paste0(value, "-"))))
        xpath
    },
    xpath_attrib_prefixmatch = function(xpath, name, value) {
        if (!is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and starts-with(%s, %s)",
                                        name, name, xpath_literal(value)))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    # In XPath there is starts-with but not ends-with, hence the oddness
    xpath_attrib_suffixmatch = function(xpath, name, value) {
        if (!is.null(value) && nzchar(value)) {
            xpath$add_condition(sprintf("%s and substring(%s, string-length(%s)-%s) = %s",
                                        name, name, name, nchar(value) - 1, xpath_literal(value)))
        } else {
            xpath$add_condition("0")
        }
        xpath
    },
    xpath_attrib_substringmatch = function(xpath, name, value) {
        if (!is.null(value) && nzchar(value)) {
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
        if (!xhtml) {
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
        if (!(fn$argument_types() %in% c("STRING", "IDENT"))) {
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
