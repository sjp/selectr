XPathExpr <- R6Class("XPathExpr",
    public = list(
        path = "",
        element = "*",
        condition = "",
        star_prefix = FALSE,
        initialize = function(
            path = "", element = "*", condition = "", star_prefix = FALSE) {
            self$path <- path
            self$element <- element
            self$condition <- condition
            self$star_prefix <- star_prefix
        },
        str = function() {
            p <- paste0(self$path, self$element)
            if (nzchar(self$condition))
                p <- paste0(p, "[", self$condition, "]")
            p
        },
        repr = function() {
            paste0(first_class_name(self), "[", self$str(), "]")
        },
        add_condition = function(condition) {
            self$condition <-
                if (nzchar(self$condition))
                    self$condition <-
                        paste0(self$condition, " and (", condition, ")")
                else
                    condition
        },
        add_name_test = function() {
            if (self$element == "*")
                return()
            self$add_condition(paste0("name() = ", xpath_literal(self$element)))
            self$element <- "*"
        },
        join = function(combiner, other) {
            p <- paste0(self$str(), combiner)
            if (other$path != "*/")
                p <- paste0(p, other$path)
            self$path <- p
            self$element <- other$element
            self$condition <- other$condition
            self
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    ))

is_safe_name <- function(name) {
    grepl("^[a-zA-Z_][a-zA-Z0-9_.-]*$", name)
}

first_class_name <- function(obj) {
    result <- class(obj)[1]

    # HACK!
    # R.oo clashes with our preferred use of 'Class' for the name of the
    # ClassSelector class, even though it is hidden in our package.
    # Because the name of the class is used in places, perform a
    # special case rename from ClassSelector to Class.
    if (result == "ClassSelector") "Class" else result
}

xpath_literal <- function(literal) {
    lenseq <- seq_len(nchar(literal))
    split_chars <- substring(literal, lenseq, lenseq)

    if (!any(split_chars == "'")) {
        literal <- paste0("'", literal, "'")
    } else if (!any(split_chars == '"')) {
        literal <- paste0('"', literal, '"')
    } else {
        dq_inds <- which(split_chars == "'")
        sq_inds <- which(split_chars != "'")
        split_chars[dq_inds] <- paste0('"', split_chars[dq_inds], '"')
        split_chars[sq_inds] <- paste0("'", split_chars[sq_inds], "'")

        literal <- paste(split_chars, collapse = ",")
        literal <- paste0("concat(", literal, ")")
    }

    literal
}

GenericTranslator <- R6Class("GenericTranslator",
    public = list(
        combinator_mapping = c(" " = "descendant",
                               ">" = "child",
                               "+" = "direct_adjacent",
                               "~" = "indirect_adjacent"),
        # != is not in Selectors Level 3, but included anyway
        attribute_operator_mapping = c("exists" = "exists",
                                       "=" = "equals",
                                       "~=" = "includes",
                                       "|=" = "dashmatch",
                                       "^=" = "prefixmatch",
                                       "$=" = "suffixmatch",
                                       "*=" = "substringmatch",
                                       "!=" = "different"),
        id_attribute = "id",
        lang_attribute = "xml:lang",
        lower_case_element_names = FALSE,
        lower_case_attribute_names = FALSE,
        lower_case_attribute_values = FALSE,
        css_to_xpath = function(css, prefix = "descendant-or-self::") {
            selectors <- parse(css)
            selectors <-
                if (is.null(selectors)) list()
                else if (!is.list(selectors)) list(selectors)
                else selectors

            lapply(selectors, function(selector) {
                if (first_class_name(selector) == "Selector" &&
                    !is.null(selector$pseudo_element))
                    stop("Pseudo-elements are not supported.")
            })

            char_selectors <-
                sapply(selectors,
                       function(selector)
                           self$selector_to_xpath(selector, prefix))

            paste0(char_selectors, collapse = " | ")
        },
        selector_to_xpath = function(selector, prefix = "descendant-or-self::") {
            tree <- selector$parsed_tree
            xpath <- self$xpath(tree)
            if (!inherits(xpath, "XPathExpr"))
                stop("'xpath' is not an instance of 'XPathExpr'")
            paste0(if (!is.null(prefix)) prefix else "", xpath$str())
        },
        xpath = function(parsed_selector) {
            type_name <- first_class_name(parsed_selector)
            method_name <- paste0("xpath_", tolower(type_name))

            if (method_name == "xpath_attrib")
                self$xpath_attrib(parsed_selector)
            else if (method_name == "xpath_class")
                self$xpath_class(parsed_selector)
            else if (method_name == "xpath_combinedselector")
                self$xpath_combinedselector(parsed_selector)
            else if (method_name == "xpath_element")
                self$xpath_element(parsed_selector)
            else if (method_name == "xpath_function")
                self$xpath_function(parsed_selector)
            else if (method_name == "xpath_hash")
                self$xpath_hash(parsed_selector)
            else if (method_name == "xpath_negation")
                self$xpath_negation(parsed_selector)
            else if (method_name == "xpath_pseudo")
                self$xpath_pseudo(parsed_selector)
            else
                stop(paste0("Unknown method name '", type_name, "'"))
        },
        xpath_combinedselector = function(combined) {
            combinator <- paste0(
                "xpath_",
                self$combinator_mapping[combined$combinator],
                "_combinator")

            left_xpath <- self$xpath(combined$selector)
            right_xpath <- self$xpath(combined$subselector)
            if (combinator == "xpath_descendant_combinator")
                self$xpath_descendant_combinator(
                    left = left_xpath, right = right_xpath)
            else if (combinator == "xpath_child_combinator")
                self$xpath_child_combinator(
                    left = left_xpath, right = right_xpath)
            else if (combinator == "xpath_direct_adjacent_combinator")
                self$xpath_direct_adjacent_combinator(
                    left = left_xpath, right = right_xpath)
            else if (combinator == "xpath_indirect_adjacent_combinator")
                self$xpath_indirect_adjacent_combinator(
                    left = left_xpath, right = right_xpath)
            else if (combinator == "xpath_indirect_adjacent_combinator")
                self$xpath_indirect_adjacent_combinator(
                    left = left_xpath, right = right_xpath)
            else
                stop(paste0("Unknown combinator '",
                            self$combinator_mapping[combined$combinator], "'"))
        },
        xpath_negation = function(negation) {
            xpath <- self$xpath(negation$selector)
            sub_xpath <- self$xpath(negation$subselector)
            sub_xpath$add_name_test()
            if (!is.null(sub_xpath$condition) && nzchar(sub_xpath$condition)) {
                xpath$add_condition(paste0("not(", sub_xpath$condition, ")"))
            } else {
                xpath$add_condition("0")
            }
            xpath
        },
        xpath_function = function(fn) {
            method_name <- paste0(
                "xpath_",
                gsub("-", "_", fn$name),
                "_function")
            xp <- self$xpath(fn$selector)

            if (method_name == "xpath_contains_function")
                self$xpath_contains_function(xp, fn)
            else if (method_name == "xpath_lang_function")
                self$xpath_lang_function(xp, fn)
            else if (method_name == "xpath_nth_child_function")
                self$xpath_nth_child_function(xp, fn)
            else if (method_name == "xpath_nth_last_child_function")
                self$xpath_nth_last_child_function(xp, fn)
            else if (method_name == "xpath_nth_of_type_function")
                self$xpath_nth_of_type_function(xp, fn)
            else if (method_name == "xpath_nth_last_of_type_function")
                self$xpath_nth_last_of_type_function(xp, fn)
            else
                stop(paste0(
                    "The pseudo-class :",
                    gsub("-", "_", fn$name),
                    "() is unknown"))
        },
        xpath_pseudo = function(pseudo) {
            method_name <- paste0(
                "xpath_",
                gsub("-", "_", pseudo$ident),
                "_pseudo")
            xp <- self$xpath(pseudo$selector)

            if (method_name == "xpath_root_pseudo")
                self$xpath_root_pseudo(xp)
            else if (method_name == "xpath_first_child_pseudo")
                self$xpath_first_child_pseudo(xp)
            else if (method_name == "xpath_last_child_pseudo")
                self$xpath_last_child_pseudo(xp)
            else if (method_name == "xpath_first_of_type_pseudo")
                self$xpath_first_of_type_pseudo(xp)
            else if (method_name == "xpath_last_of_type_pseudo")
                self$xpath_last_of_type_pseudo(xp)
            else if (method_name == "xpath_only_child_pseudo")
                self$xpath_only_child_pseudo(xp)
            else if (method_name == "xpath_only_of_type_pseudo")
                self$xpath_only_of_type_pseudo(xp)
            else if (method_name == "xpath_empty_pseudo")
                self$xpath_empty_pseudo(xp)
            else if (method_name == "xpath_link_pseudo")
                self$xpath_link_pseudo(xp)
            else if (method_name == "xpath_visited_pseudo")
                self$xpath_visited_pseudo(xp)
            else if (method_name == "xpath_hover_pseudo")
                self$xpath_hover_pseudo(xp)
            else if (method_name == "xpath_active_pseudo")
                self$xpath_active_pseudo(xp)
            else if (method_name == "xpath_focus_pseudo")
                self$xpath_focus_pseudo(xp)
            else if (method_name == "xpath_target_pseudo")
                self$xpath_target_pseudo(xp)
            else if (method_name == "xpath_enabled_pseudo")
                self$xpath_enabled_pseudo(xp)
            else if (method_name == "xpath_disabled_pseudo")
                self$xpath_disabled_pseudo(xp)
            else if (method_name == "xpath_checked_pseudo")
                self$xpath_checked_pseudo(xp)
            else
                stop(paste0("The pseudo-class :", pseudo$ident, " is unknown"))
        },
        xpath_attrib = function(selector) {
            operator <- self$attribute_operator_mapping[selector$operator]
            method_name <- paste0("xpath_attrib_", operator)
            if (self$lower_case_attribute_names) {
                name <- tolower(selector$attrib)
            } else {
                name <- selector$attrib
            }
            safe <- is_safe_name(name)
            if (!is.null(selector$namespace)) {
                name <- paste0(selector$namespace, ":", name)
            }
            if (safe) {
                attrib <- paste0("@", name)
            } else {
                attrib <- paste0(
                    "attribute::*[name() = ", xpath_literal(name), "]")
            }
            if (self$lower_case_attribute_names) {
                value <- tolower(selector$value)
            } else {
                value <- selector$value
            }

            xp <- self$xpath(selector$selector)
            if (method_name == "xpath_attrib_dashmatch")
                self$xpath_attrib_dashmatch(xp, attrib, value)
            else if (method_name == "xpath_attrib_different")
                self$xpath_attrib_different(xp, attrib, value)
            else if (method_name == "xpath_attrib_equals")
                self$xpath_attrib_equals(xp, attrib, value)
            else if (method_name == "xpath_attrib_exists")
                self$xpath_attrib_exists(xp, attrib, value)
            else if (method_name == "xpath_attrib_includes")
                self$xpath_attrib_includes(xp, attrib, value)
            else if (method_name == "xpath_attrib_prefixmatch")
                self$xpath_attrib_prefixmatch(xp, attrib, value)
            else if (method_name == "xpath_attrib_substringmatch")
                self$xpath_attrib_substringmatch(xp, attrib, value)
            else if (method_name == "xpath_attrib_suffixmatch")
                self$xpath_attrib_suffixmatch(xp, attrib, value)
            else
                stop(paste0("Unknown attribute operator '", operator, "'"))
        },
        # .foo is defined as [class~=foo] in the spec
        xpath_class = function(class_selector) {
            xpath <- self$xpath(class_selector$selector)
            self$xpath_attrib_includes(xpath, "@class",
                                       class_selector$class_name)
            xpath
        },
        xpath_hash = function(id_selector) {
            xpath <- self$xpath(id_selector$selector)
            self$xpath_attrib_equals(xpath, "@id", id_selector$id)
            xpath
        },
        xpath_element = function(selector) {
            element <- selector$element
            if (is.null(element)) {
                element <- "*"
                safe <- TRUE
            } else {
                safe <- is_safe_name(element)
                if (self$lower_case_element_names)
                    element <- tolower(element)
            }
            if (!is.null(selector$namespace)) {
                # Namespace prefixes are case-sensitive.
                # http://www.w3.org/TR/css3-namespace/#prefixes
                element <- paste0(selector$namespace, ":", element)
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
        xpath_nth_child_function = function(xpath, fn, last = FALSE,
                                            add_name_test = TRUE) {
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
                nodetest <- xpath$element
            }

            # count siblings before or after the element
            if (!last) {
                siblings_count <- paste0("count(preceding-sibling::",
                                         nodetest, ")")
            } else {
                siblings_count <- paste0("count(following-sibling::",
                                         nodetest, ")")
            }

            # special case of fixed position: nth-*(0n+b)
            # if a == 0:
            # ~~~~~~~~~~
            #    count(***-sibling::***) = b-1
            if (a == 0) {
                xpath$add_condition(paste0(siblings_count, " = ", b_min_1))
                return(xpath)
            }

            expr <- character(0)

            if (a > 0) {
                # siblings count, an+b-1, is always >= 0,
                # so if a>0, and (b-1)<=0, an "n" exists to satisfy this,
                # therefore, the predicate is only interesting if (b-1)>0
                if (b_min_1 > 0) {
                    expr <- c(expr, paste0(siblings_count, " >= ", b_min_1))
                }
            } else {
                # if a<0, and (b-1)<0, no "n" satisfies this,
                # this is tested above as an early exist condition
                # otherwise,
                expr <- c(expr, paste0(siblings_count, " <= ", b_min_1))
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
                    b_neg <- paste0("+", b_neg)
                    left <- paste0("(", left, " ", b_neg, ")")
                }

                expr <- c(expr, paste0(left, " mod ", a, " = 0"))
            }

            if (length(expr)) {
                expr <- paste0(expr, collapse = " and ")
                xpath$add_condition(expr)
            }
            xpath
        },
        xpath_nth_last_child_function = function(xpath, fn) {
            self$xpath_nth_child_function(xpath, fn, last = TRUE)
        },
        xpath_nth_of_type_function = function(xpath, fn) {
            if (xpath$element == "*") {
                stop("*:nth-of-type() is not implemented")
            }
            self$xpath_nth_child_function(xpath, fn, add_name_test = FALSE)
        },
        xpath_nth_last_of_type_function = function(xpath, fn) {
            if (xpath$element == "*") {
                stop("*:nth-last-of-type() is not implemented")
            }
            self$xpath_nth_child_function(xpath, fn, last = TRUE,
                                          add_name_test = FALSE)
        },
        xpath_contains_function = function(xpath, fn) {
            if (!(fn$argument_types() %in% c("STRING", "IDENT"))) {
                stop(paste0(
                    "Expected a single string or ident for :contains(), got (",
                    paste0(fn$argument_types(), collapse = ", "), ")"))
            }
            value <- fn$arguments[[1]]$value
            xpath$add_condition(paste0(
                "contains(., ", xpath_literal(value), ")"))
            xpath
        },
        xpath_lang_function = function(xpath, fn) {
            if (!(fn$argument_types() %in% c("STRING", "IDENT"))) {
                stop(paste0(
                    "Expected a single string or ident for :lang(), got ",
                    fn$arguments[[1]]$repr()))
            }
            value <- fn$arguments[[1]]$value
            xpath$add_condition(paste0("lang(", xpath_literal(value), ")"))
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
            xpath$add_condition(paste0(
                "count(preceding-sibling::", xpath$element, ") = 0"))
            xpath
        },
        xpath_last_of_type_pseudo = function(xpath) {
            if (xpath$element == "*") {
                stop("*:last-of-type is not implemented")
            }
            xpath$add_condition(paste0(
                "count(following-sibling::", xpath$element, ") = 0"))
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
            xpath$add_condition(paste0(
                "count(parent::*/child::", xpath$element, ") = 1"))
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
            xpath$add_condition(paste0(name, " = ", xpath_literal(value)))
            xpath
        },
        xpath_attrib_different = function(xpath, name, value) {
            xpath$add_condition(paste0("not(", name, ") or ", name, " != ",
                                       xpath_literal(value)))
            xpath
        },
        xpath_attrib_includes = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value) &&
                grepl("^[^ \t\r\n\f]+$", value)) {
                xpath$add_condition(paste0(
                    name,
                    " and contains(concat(' ', normalize-space(",
                    name,
                    "), ' '), ",
                    xpath_literal(paste0(" ", value, " ")),
                    ")"))
            } else {
                xpath$add_condition("0")
            }
            xpath
        },
        xpath_attrib_dashmatch = function(xpath, name, value) {
            xpath$add_condition(paste0(
                name,
                " and (",
                name,
                " = ",
                xpath_literal(value),
                " or starts-with(",
                name,
                ", ",
                xpath_literal(paste0(value, "-")),
                "))"))
            xpath
        },
        xpath_attrib_prefixmatch = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value)) {
                xpath$add_condition(paste0(
                    name,
                    " and starts-with(",
                    name,
                    ", ",
                    xpath_literal(value),
                    ")"))
            } else {
                xpath$add_condition("0")
            }
            xpath
        },
        # In XPath there is starts-with but not ends-with, hence the oddness
        xpath_attrib_suffixmatch = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value)) {
                xpath$add_condition(paste0(
                    name,
                    " and substring(",
                    name,
                    ", string-length(",
                    name,
                    ")-",
                    nchar(value) - 1,
                    ") = ",
                    xpath_literal(value)))
            } else {
                xpath$add_condition("0")
            }
            xpath
        },
        xpath_attrib_substringmatch = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value)) {
                xpath$add_condition(paste0(
                    name,
                    " and contains(",
                    name,
                    ", ",
                    xpath_literal(value),
                    ")"))
            } else {
                xpath$add_condition("0")
            }
            xpath
        }
    )
)

HTMLTranslator <- R6Class("HTMLTranslator",
    inherit = GenericTranslator,
    public = list(
        xhtml = FALSE,
        initialize = function(xhtml = FALSE, ...) {
            self$xhtml <- xhtml
            if (!xhtml) {
                self$lower_case_element_names <- TRUE
                self$lower_case_attribute_names <- TRUE
            }
            self$lang_attribute <- "lang"
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
                stop(paste0(
                    "Expected a single string or ident for :lang(), got ",
                    fn$arguments[[1]]$repr()))
            }
            value <- fn$arguments[[1]]$value
            xpath$add_condition(paste0(
                "ancestor-or-self::*[@lang][1][starts-with(concat(",
                "translate(@",
                self$lang_attribute,
                ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                "'abcdefghijklmnopqrstuvwxyz'), '-'), ",
                xpath_literal(paste0(tolower(value), "-")),
                ")]"))
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
        }
    )
)
