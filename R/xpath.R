XPathExpr <- R6Class("XPathExpr",
    public = list(
        path = "",
        element = "*",
        # Sequential predicates rendered as [p1][p2]... between the
        # element and the condition. Unlike conditions (which are
        # AND-ed together into a single predicate), the order of
        # predicates is significant: a positional predicate such as [1]
        # filters the node set produced by the predicates before it
        predicates = character(0),
        condition = "",
        star_prefix = FALSE,
        # When an explicit element name cannot be used as an XPath name
        # test (and so 'element' has been folded into a condition on
        # '*'), an equivalent node test for that name; NULL otherwise.
        # Lets the of-type pseudo-classes distinguish such elements from
        # the universal selector and count their siblings correctly.
        name_test = NULL,
        initialize = function(
            path = "", element = "*", condition = "", star_prefix = FALSE) {
            self$path <- path
            self$element <- element
            if (nzchar(condition))
                self$add_condition(condition)
            self$star_prefix <- star_prefix
        },
        str = function() {
            p <- paste0(self$path, self$element)
            if (length(self$predicates))
                p <- paste0(p,
                            paste0("[", self$predicates, "]", collapse = ""))
            if (nzchar(self$condition))
                p <- paste0(p, "[", self$condition, "]")
            p
        },
        repr = function() {
            paste0(first_class_name(self), "[", self$str(), "]")
        },
        add_condition = function(condition) {
            # Always AND with the existing condition: an "or" appended here
            # would flatten into the accumulated condition chain (XPath
            # "and" binds tighter than "or"), changing its meaning.
            # Callers wanting alternatives must OR-join them and add the
            # result as one condition.
            #
            # Parenthesize only when needed: "or" is the only XPath
            # operator binding more loosely than the "and" used to join
            # conditions, so a condition without " or " can never change
            # meaning when joined. The substring test may over-wrap when
            # " or " only occurs nested or inside a literal, which is
            # harmless; it can never under-wrap.
            if (grepl(" or ", condition, fixed = TRUE))
                condition <- paste0("(", condition, ")")
            self$condition <-
                if (nzchar(self$condition))
                    paste0(self$condition, " and ", condition)
                else
                    condition
        },
        add_predicate = function(predicate) {
            self$predicates <- c(self$predicates, predicate)
        },
        add_name_test = function(as_predicate = FALSE) {
            if (self$element == "*")
                return()
            add <-
                if (as_predicate) self$add_predicate
                else self$add_condition
            if (is_safe_nodetest(self$element)) {
                # A safe name stays an XPath name test on the self axis,
                # so namespace prefixes keep resolving through the
                # namespace map supplied at evaluation time (URI-based),
                # exactly as the same name is matched at the top level
                # of a selector. A name() comparison would instead match
                # the document's literal prefix.
                add(paste0("self::", self$element))
                self$name_test <- self$element
            } else {
                add(paste0("name() = ",
                           xpath_literal(self$element)))
                self$name_test <- paste0("*[name() = ",
                                         xpath_literal(self$element), "]")
            }
            self$element <- "*"
        },
        join = function(combiner, other) {
            p <- paste0(self$str(), combiner)
            if (other$path != "*/")
                p <- paste0(p, other$path)
            self$path <- p
            self$element <- other$element
            self$predicates <- other$predicates
            self$condition <- other$condition
            self$name_test <- other$name_test
            self
        },
        show = function() { # nocov start
            cat(self$repr(), "\n")
        } # nocov end
    ))

is_safe_name <- function(name) {
    grepl("^[a-zA-Z_][a-zA-Z0-9_.-]*$", name)
}

# A name (optionally prefixed, e.g. 'svg:g') that can be used directly
# as an XPath name test
is_safe_nodetest <- function(name) {
    parts <- strsplit(name, ":", fixed = TRUE)[[1]]
    length(parts) <= 2 && all(is_safe_name(parts))
}

# The XPath node test matching the same elements as the subject of an
# of-type pseudo-class, or NULL when the subject is the universal
# selector (for which of-type pseudo-classes are not implemented)
of_type_nodetest <- function(xpath) {
    if (xpath$element != "*")
        xpath$element
    else
        xpath$name_test
}

# Shared translation for pseudo-classes that can never match in a
# static document
pseudo_never_matches <- function(xpath) {
    xpath$add_condition("0")
    xpath
}

# Validate that all arguments of :lang() are STRING, IDENT, or * (DELIM).
# A lone '-' lexes as an IDENT but is not a valid <ident> per
# css-syntax, so reject it too.
validate_lang_args <- function(fn) {
    arg_types <- fn$argument_types()
    arg_values <- sapply(fn$arguments, function(a) a$value)
    valid_types <- (arg_types %in% c("STRING", "IDENT") |
                  (arg_types == "DELIM" & arg_values == "*")) &
                  !(arg_types == "IDENT" & arg_values == "-")
    if (!all(valid_types)) {
        stop("Expected string, ident, or * arguments for :lang(), got ",
             token_repr(fn$arguments[[which(!valid_types)[1]]]))
    }
}

# The language values named by the arguments of :lang(), combining an
# ident or string ending in '-' with a following '*' DELIM into a
# single wildcard range (e.g. "en-" + "*" = "en-*")
extract_lang_values <- function(fn) {
    lang_values <- character(0)
    i <- 1
    while (i <= length(fn$arguments)) {
        arg <- fn$arguments[[i]]
        if (arg$type %in% c("IDENT", "STRING") &&
            grepl("-$", arg$value) &&
            i < length(fn$arguments) &&
            fn$arguments[[i + 1]]$type == "DELIM" &&
            fn$arguments[[i + 1]]$value == "*") {
            lang_values <- c(lang_values, paste0(arg$value, "*"))
            i <- i + 2  # Skip the next token since we combined it
        } else {
            lang_values <- c(lang_values, arg$value)
            i <- i + 1
        }
    }
    lang_values
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
    if (!is.character(literal) || length(literal) != 1) {
        stop("literal must be a single character string")
    }

    if (!nzchar(literal)) {
        return("''")
    }

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
        attribute_operator_mapping = c("exists" = "exists",
                                       "=" = "equals",
                                       "~=" = "includes",
                                       "|=" = "dashmatch",
                                       "^=" = "prefixmatch",
                                       "$=" = "suffixmatch",
                                       "*=" = "substringmatch"),
        id_attribute = "id",
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
            method <- self[[paste0("xpath_", tolower(type_name))]]
            if (is.null(method))
                stop("Unknown method name '", type_name, "'")
            method(parsed_selector)
        },
        xpath_combinedselector = function(combined) {
            combinator <- self$combinator_mapping[combined$combinator]
            method <- self[[paste0("xpath_", combinator, "_combinator")]]
            if (is.null(method))
                stop("Unknown combinator '", combinator, "'")
            method(left = self$xpath(combined$selector),
                   right = self$xpath(combined$subselector))
        },
        xpath_argument_condition = function(subselector) {
            # Translate one functional pseudo-class argument into a
            # condition on the candidate element. A complex argument
            # (CombinedSelector) applies its rightmost compound to the
            # candidate, with everything to its left becoming an
            # existence test through reversed axes (e.g. :is(a > b)
            # matches a 'b' whose parent is an 'a')
            if (first_class_name(subselector) == "CombinedSelector") {
                sub_xpath <- self$xpath(subselector$subselector)
                sub_xpath$add_name_test()
                rev_test <- self$reversed_combinator_test(
                    subselector$selector, subselector$combinator)
                if (nzchar(sub_xpath$condition))
                    paste0(sub_xpath$condition, " and ", rev_test)
                else
                    rev_test
            } else {
                sub_xpath <- self$xpath(subselector)
                sub_xpath$add_name_test()
                # An argument that imposes no condition (a bare '*')
                # matches everything; return an explicit "true()" so
                # callers can tell "always true" apart from "no
                # condition" instead of silently dropping the argument
                # from the selector list
                if (nzchar(sub_xpath$condition)) sub_xpath$condition
                else "true()"
            }
        },
        selector_list_condition = function(selector_list) {
            # OR-join the conditions imposed by a selector list's
            # arguments into a single condition. NULL when the list
            # imposes no condition: either it is absent, or one of its
            # arguments (e.g. the universal selector '*') is always
            # true, making the whole list match unconditionally
            if (is.null(selector_list) || length(selector_list) == 0)
                return(NULL)
            conditions <- vapply(selector_list,
                                 self$xpath_argument_condition,
                                 character(1))
            if (any(conditions == "true()"))
                return(NULL)
            paste0(conditions, collapse = " or ")
        },
        reversed_combinator_test = function(selector, combinator) {
            # Existence test, relative to the candidate element, for the
            # left-hand side of a combinator inside a pseudo-class
            # argument: ' ' -> an ancestor, '>' -> the parent, '~' -> any
            # preceding sibling, '+' -> the immediately preceding sibling.
            # The left-hand side may itself be complex, so recurse
            inner <- self$xpath_argument_condition(selector)
            axis <-
                if (combinator == " ") "ancestor::*"
                else if (combinator == ">") "parent::*"
                else if (combinator == "~") "preceding-sibling::*"
                else if (combinator == "+") "preceding-sibling::*[1]"
                else stop("Unknown combinator '", combinator, "'")
            if (inner == "true()") axis else paste0(axis, "[", inner, "]")
        },
        xpath_negation = function(negation) {
            xpath <- self$xpath(negation$selector)

            # Negate the OR of the argument conditions (any match means
            # the element is excluded); a list that matches everything
            # (e.g. :not(*), :not(a, *)) can never be satisfied
            condition <- self$selector_list_condition(negation$selector_list)
            if (is.null(condition)) {
                xpath$add_condition("0")
            } else {
                xpath$add_condition(paste0("not(", condition, ")"))
            }
            xpath
        },
        xpath_matching = function(matching) {
            xpath <- self$xpath(matching$selector)

            # Add the OR of the argument conditions (any match suffices)
            # as a single condition so the alternatives stay grouped and
            # AND with the rest of the compound selector; a list that
            # matches everything (e.g. :is(a, *)) imposes no condition
            condition <- self$selector_list_condition(matching$selector_list)
            if (!is.null(condition)) {
                xpath$add_condition(condition)
            }

            xpath
        },
        xpath_where = function(where) {
            # :where() behaves exactly like :is() in terms of matching,
            # but has zero specificity (handled in the Where class itself)
            self$xpath_matching(where)
        },
        xpath_has_test = function(selector, combinator) {
            # Existence test for one :has() argument, as a path relative
            # to the candidate element. Unlike the other functional
            # pseudo-classes, :has() looks forward, so a complex argument
            # extends the path step by step; the leading combinator
            # applies to the leftmost compound
            if (first_class_name(selector) == "CombinedSelector") {
                left <- self$xpath_has_test(selector$selector, combinator)
                sub_xpath <- self$xpath(selector$subselector)
                sub_xpath$add_name_test()
                joiner <-
                    if (selector$combinator == " ") "//"
                    else if (selector$combinator == ">") "/"
                    else if (any(selector$combinator == c("~", "+")))
                        "/following-sibling::"
                    else stop("Unknown combinator '", selector$combinator, "'")
                rel_test <- paste0(left, joiner, sub_xpath$element)
                if (selector$combinator == "+") {
                    rel_test <- paste0(rel_test, "[1]")
                }
                if (nzchar(sub_xpath$condition)) {
                    rel_test <- paste0(rel_test, "[", sub_xpath$condition, "]")
                }
                rel_test
            } else {
                sub_xpath <- self$xpath(selector)
                sub_xpath$add_name_test()
                axis <-
                    if (combinator == ">") "child::"
                    else if (any(combinator == c("~", "+"))) "following-sibling::"
                    else ".//"
                rel_test <- paste0(axis, sub_xpath$element)
                if (combinator == "+") {
                    # Only the immediately following sibling: constrain
                    # position before applying the match conditions, as in
                    # xpath_direct_adjacent_combinator
                    rel_test <- paste0(rel_test, "[1]")
                }
                if (nzchar(sub_xpath$condition)) {
                    rel_test <- paste0(rel_test, "[", sub_xpath$condition, "]")
                }
                rel_test
            }
        },
        xpath_has = function(has) {
            # :has() takes a relative selector list (selectors-4
            # section 17): each argument may carry a leading combinator
            # scoping the match (> child, ~ subsequent sibling, + next
            # sibling); the omitted combinator means descendant
            xpath <- self$xpath(has$selector)

            # Build conditions that check for the existence of a match
            conditions <- vapply(has$selector_list, function(subselector) {
                if (first_class_name(subselector) == "RelativeSelector")
                    self$xpath_has_test(subselector$selector,
                                        subselector$combinator)
                else
                    self$xpath_has_test(subselector, " ")
            }, character(1))

            # Combine conditions with OR (any match means the element matches)
            if (length(conditions) > 0) {
                combined_condition <- paste0(conditions, collapse = " | ")
                xpath$add_condition(combined_condition)
            }

            xpath
        },
        xpath_function = function(fn) {
            method_name <- paste0(
                "xpath_",
                gsub("-", "_", fn$name),
                "_function")
            xp <- self$xpath(fn$selector)

            method <- self[[method_name]]
            if (is.null(method))
                stop("The pseudo-class :",
                     gsub("-", "_", fn$name),
                     "() is unknown")
            method(xp, fn)
        },
        xpath_pseudo = function(pseudo) {
            method_name <- paste0(
                "xpath_",
                gsub("-", "_", pseudo$ident),
                "_pseudo")
            xp <- self$xpath(pseudo$selector)

            method <- self[[method_name]]
            if (is.null(method))
                stop("The pseudo-class :", pseudo$ident, " is unknown")
            method(xp)
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
            if (identical(selector$namespace, "*")) {
                # '[*|attr]': 'attr' in any namespace, including none.
                # An unprefixed XPath attribute test only matches
                # attributes with no namespace, so test against
                # local-name() instead.
                attrib <- paste0(
                    "@*[local-name() = ", xpath_literal(name), "]")
            } else {
                if (!is.null(selector$namespace)) {
                    name <- paste0(selector$namespace, ":", name)
                }
                if (safe) {
                    attrib <- paste0("@", name)
                } else {
                    attrib <- paste0(
                        "attribute::*[name() = ", xpath_literal(name), "]")
                }
            }
            if (self$lower_case_attribute_values &&
                !identical(selector$flag, "s")) {
                # An explicit 's' flag opts out of any implicit
                # case-insensitivity
                value <- tolower(selector$value)
            } else {
                value <- selector$value
            }

            xp <- self$xpath(selector$selector)
            if (identical(selector$flag, "i") &&
                !is.null(value) && nzchar(value)) {
                # '[attr="value" i]': match the value ASCII
                # case-insensitively, so compare the ASCII-lowercased
                # attribute against the ASCII-lowercased value.
                # An empty value needs no lowercasing, and skipping it
                # keeps the existence tests (e.g. 'not(@attr)') exact.
                value <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                "abcdefghijklmnopqrstuvwxyz", value)
                attrib <- paste0(
                    "translate(",
                    attrib,
                    ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
                    " 'abcdefghijklmnopqrstuvwxyz')")
            }
            method <- self[[method_name]]
            if (is.null(method))
                stop("Unknown attribute operator '", operator, "'")
            method(xp, attrib, value)
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
            self$xpath_attrib_equals(xpath, paste0("@", self$id_attribute),
                                     id_selector$id)
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
            namespace <- selector$namespace
            if (identical(namespace, "*") && element != "*") {
                # '*|e': 'e' in any namespace, including none.  An
                # unprefixed XPath name test only matches the null
                # namespace, so test against local-name() instead.
                xpath <- XPathExpr$new()
                xpath$add_condition(paste0("local-name() = ",
                                           xpath_literal(element)))
                xpath$name_test <- paste0("*[local-name() = ",
                                          xpath_literal(element), "]")
                return(xpath)
            }
            if (identical(namespace, "")) {
                # '|e': 'e' in no namespace, which is exactly what an
                # unprefixed XPath name test matches.  '|*' needs an
                # explicit namespace-uri() check.
                if (element == "*") {
                    xpath <- XPathExpr$new()
                    xpath$add_condition("namespace-uri() = ''")
                    return(xpath)
                }
                if (!safe) {
                    # An unsafe name must not fall through to the name()
                    # fallback below: name() is unprefixed for an element
                    # in a *default* namespace too, so the null namespace
                    # has to be pinned explicitly alongside the local name.
                    nodetest <- paste0("*[namespace-uri() = '' and local-name() = ",
                                       xpath_literal(element), "]")
                    xpath <- XPathExpr$new()
                    xpath$add_condition(paste0("namespace-uri() = '' and local-name() = ",
                                               xpath_literal(element)))
                    xpath$name_test <- nodetest
                    return(xpath)
                }
                namespace <- NULL
            }
            if (!is.null(namespace) && namespace != "*") {
                # Namespace prefixes are case-sensitive.
                # https://www.w3.org/TR/css-namespaces-3/#prefixes
                element <- paste0(namespace, ":", element)
                safe <- safe && is_safe_name(namespace)
            }
            xpath <- XPathExpr$new(element = element)
            if (!safe)
                xpath$add_name_test()
            xpath
        },
        xpath_descendant_combinator = function(left, right) {
            left$join("//", right)
        },
        xpath_child_combinator = function(left, right) {
            left$join("/", right)
        },
        xpath_direct_adjacent_combinator = function(left, right) {
            xpath <- left$join("/following-sibling::", right)
            # Constrain position before testing the name:
            # *[1][self::e] is "the first following sibling, if it is
            # an e", whereas *[self::e][1] would wrongly select the
            # first following e. Conditions from the right selector
            # (e.g. attribute tests) stay behind both, giving
            # *[1][self::e][condition].
            xpath$add_predicate("1")
            xpath$add_name_test(as_predicate = TRUE)
            xpath
        },
        xpath_indirect_adjacent_combinator = function(left, right) {
            left$join("/following-sibling::", right)
        },
        xpath_nth_child_function = function(xpath, fn, last = FALSE,
                                            add_name_test = TRUE) {
            ab <- parse_series(fn$arguments)

            # Validate that parse_series returned valid results
            if (is.null(ab) || length(ab) != 2) {
                stop("Invalid nth-child expression")
            }

            a <- ab[1]
            b <- ab[2]

            # Validate that a and b are valid integers (not NA)
            if (is.na(a) || is.na(b)) {
                stop("Invalid nth-child expression: could not parse as valid integers")
            }

            # From https://www.w3.org/TR/selectors-4/#structural-pseudos:
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
            # CSS Selectors Level 4 adds optional "of S" selector list:
            # :nth-child(an+b of S) - count only siblings that match selector S
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
            if (a == 1 && b_min_1 <= 0) {
                # CSS Level 4: When selector list is provided, ensure current element matches
                condition <- self$selector_list_condition(fn$selector_list)
                if (!is.null(condition)) {
                    xpath$add_condition(condition)
                }
                return(xpath)
            }
            # early-exit condition 2:
            # ~~~~~~~~~~~~~~~~~~~~~~~
            # an+b-1 siblings with a<0 and (b-1)<0 is not possible
            if (a < 0 && b_min_1 < 0) {
                xpath$add_condition("0")

                # CSS Level 4: When selector list is provided, ensure current element matches
                # Even though the condition is always false, we should still check the selector
                condition <- self$selector_list_condition(fn$selector_list)
                if (!is.null(condition)) {
                    xpath$add_condition(condition)
                }

                return(xpath)
            }

            # `add_name_test` boolean is inverted and somewhat counter-intuitive:
            #
            # nth_of_type() calls nth_child(add_name_test=False)
            if (add_name_test) {
                nodetest <- "*"
            } else {
                nodetest <- of_type_nodetest(xpath)
            }

            # Build the predicate for selector list filtering (CSS Level 4):
            # only siblings matching the list are counted; a list that
            # matches everything counts all siblings (no predicate)
            selector_list_cond <- self$selector_list_condition(fn$selector_list)
            selector_predicate <-
                if (is.null(selector_list_cond)) ""
                else paste0("[", selector_list_cond, "]")

            # count siblings before or after the element
            if (!last) {
                siblings_count <- paste0("count(preceding-sibling::",
                                         nodetest, selector_predicate, ")")
            } else {
                siblings_count <- paste0("count(following-sibling::",
                                         nodetest, selector_predicate, ")")
            }

            # special case of fixed position: nth-*(0n+b)
            # if a == 0:
            # ~~~~~~~~~~
            #    count(***-sibling::***) = b-1
            if (a == 0) {
                xpath$add_condition(paste0(siblings_count, " = ", b_min_1))

                # CSS Level 4: When selector list is provided, ensure current element matches
                if (!is.null(selector_list_cond)) {
                    xpath$add_condition(selector_list_cond)
                }

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

            # CSS Level 4: When selector list is provided, ensure current element matches
            if (!is.null(selector_list_cond)) {
                xpath$add_condition(selector_list_cond)
            }

            xpath
        },
        xpath_nth_last_child_function = function(xpath, fn) {
            self$xpath_nth_child_function(xpath, fn, last = TRUE)
        },
        xpath_nth_of_type_function = function(xpath, fn) {
            if (is.null(of_type_nodetest(xpath))) {
                stop("*:nth-of-type() is not implemented")
            }
            self$xpath_nth_child_function(xpath, fn, add_name_test = FALSE)
        },
        xpath_nth_last_of_type_function = function(xpath, fn) {
            if (is.null(of_type_nodetest(xpath))) {
                stop("*:nth-last-of-type() is not implemented")
            }
            self$xpath_nth_child_function(xpath, fn, last = TRUE,
                                          add_name_test = FALSE)
        },
        xpath_lang_function = function(xpath, fn) {
            validate_lang_args(fn)
            lang_values <- extract_lang_values(fn)

            # Build conditions for each language value
            conditions <- vapply(lang_values, function(value) {
                if (value == "*") {
                    # Wildcard * matches everything - use a condition that's always true
                    "true()"
                } else if (grepl("\\*$", value)) {
                    # Wildcard suffix like "en-*" - match any language starting with prefix
                    # Use XPath's lang() function which does prefix matching.
                    # Strip the trailing "-*": lang('en') matches "en" and any
                    # "en-..." tag, whereas lang('en-') would match nothing
                    # because lang() only extends its argument at a '-' boundary.
                    prefix <- sub("-?\\*$", "", value)
                    paste0("lang(", xpath_literal(prefix), ")")
                } else {
                    # Regular language tag
                    paste0("lang(", xpath_literal(value), ")")
                }
            }, character(1), USE.NAMES = FALSE)

            # Combine conditions with OR; add_condition() supplies the
            # grouping parentheses when more than one alternative is joined
            if (length(conditions) > 0) {
                xpath$add_condition(paste(conditions, collapse = " or "))
            }

            xpath
        },
        xpath_dir_function = function(xpath, fn) {
            # :dir() takes exactly one identifier (CSS Selectors Level 4).
            # A lone '-' lexes as an IDENT but is not a valid <ident>
            # per css-syntax, so reject it too.
            arg_types <- fn$argument_types()
            arg_values <- sapply(fn$arguments, function(a) a$value)
            if (length(fn$arguments) != 1 || arg_types != "IDENT" ||
                arg_values == "-") {
                stop("Expected a single ident argument for :dir(), got ",
                     token_repr(fn$arguments[[1]]))
            }
            # :dir() requires runtime directionality detection based on
            # document language, inherited dir attributes, and text analysis.
            # Not possible in static XPath, so we make it never match.
            xpath$add_condition("0")
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
            nodetest <- of_type_nodetest(xpath)
            if (is.null(nodetest)) {
                stop("*:first-of-type is not implemented")
            }
            xpath$add_condition(paste0(
                "count(preceding-sibling::", nodetest, ") = 0"))
            xpath
        },
        xpath_last_of_type_pseudo = function(xpath) {
            nodetest <- of_type_nodetest(xpath)
            if (is.null(nodetest)) {
                stop("*:last-of-type is not implemented")
            }
            xpath$add_condition(paste0(
                "count(following-sibling::", nodetest, ") = 0"))
            xpath
        },
        xpath_only_child_pseudo = function(xpath) {
            # Not count(parent::*/child::*) = 1: for the root element
            # parent::* is empty (its parent is the document node), which
            # would make the count 0 and the root never match, while the
            # equivalent :first-child:last-child does match it.
            xpath$add_condition(paste(
                "count(preceding-sibling::*) = 0 and",
                "count(following-sibling::*) = 0"))
            xpath
        },
        xpath_only_of_type_pseudo = function(xpath) {
            nodetest <- of_type_nodetest(xpath)
            if (is.null(nodetest)) {
                stop("*:only-of-type is not implemented")
            }
            xpath$add_condition(paste0(
                "count(preceding-sibling::", nodetest, ") = 0 and ",
                "count(following-sibling::", nodetest, ") = 0"))
            xpath
        },
        xpath_empty_pseudo = function(xpath) {
            xpath$add_condition("not(*) and not(string-length())")
            xpath
        },

        # Pseudo-classes that depend on dynamic state which a static
        # document does not have; the HTML translator overrides the
        # ones it can answer from attributes
        xpath_any_link_pseudo = pseudo_never_matches,
        xpath_link_pseudo     = pseudo_never_matches,
        xpath_visited_pseudo  = pseudo_never_matches,
        xpath_hover_pseudo    = pseudo_never_matches,
        xpath_active_pseudo   = pseudo_never_matches,
        xpath_focus_pseudo    = pseudo_never_matches,
        xpath_target_pseudo   = pseudo_never_matches,
        xpath_target_within_pseudo = pseudo_never_matches,
        xpath_local_link_pseudo    = pseudo_never_matches,
        xpath_enabled_pseudo  = pseudo_never_matches,
        xpath_disabled_pseudo = pseudo_never_matches,
        xpath_checked_pseudo  = pseudo_never_matches,

        xpath_attrib_exists = function(xpath, name, value) {
            xpath$add_condition(name)
            xpath
        },
        xpath_attrib_equals = function(xpath, name, value) {
            xpath$add_condition(paste0(name, " = ", xpath_literal(value)))
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
        # The generic :lang() translation uses the XPath lang()
        # function, which is defined in terms of xml:lang; only the
        # HTML translation reads the language from an attribute
        lang_attribute = "lang",
        initialize = function(xhtml = FALSE) {
            self$xhtml <- xhtml
            if (!xhtml) {
                self$lower_case_element_names <- TRUE
                self$lower_case_attribute_names <- TRUE
            }
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
            validate_lang_args(fn)
            lang_values <- extract_lang_values(fn)

            # Build conditions for each language value
            conditions <- vapply(lang_values, function(value) {
                if (value == "*") {
                    # Wildcard * matches any element with a lang attribute
                    # Check for any ancestor-or-self with @lang attribute
                    paste0("ancestor-or-self::*[@", self$lang_attribute, "]")
                } else if (grepl("\\*$", value)) {
                    # Wildcard suffix like "en-*" - match any language starting with prefix
                    prefix <- sub("\\*$", "", value)  # Remove trailing *
                    # Don't add '-' if prefix already ends with it
                    search_prefix <- if (grepl("-$", prefix)) tolower(prefix) else paste0(tolower(prefix), "-")
                    paste0(
                        "ancestor-or-self::*[@", self$lang_attribute, "][1][starts-with(concat(",
                        "translate(@",
                        self$lang_attribute,
                        ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                        "'abcdefghijklmnopqrstuvwxyz'), '-'), ",
                        xpath_literal(search_prefix),
                        ")]")
                } else {
                    # Regular language tag
                    paste0(
                        "ancestor-or-self::*[@", self$lang_attribute, "][1][starts-with(concat(",
                        "translate(@",
                        self$lang_attribute,
                        ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
                        "'abcdefghijklmnopqrstuvwxyz'), '-'), ",
                        xpath_literal(paste0(tolower(value), "-")),
                        ")]")
                }
            }, character(1), USE.NAMES = FALSE)

            # Combine conditions with OR; add_condition() supplies the
            # grouping parentheses when more than one alternative is joined
            if (length(conditions) > 0) {
                xpath$add_condition(paste(conditions, collapse = " or "))
            }

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
                      "(name(.) = 'input' and not(@type = 'hidden')) or",
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
                      "(name(.) = 'input' and not(@type = 'hidden')) or",
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
                      "(((name(.) = 'input' and not(@type = 'hidden'))",
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
