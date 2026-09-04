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
        # Whether 'condition' is a top-level or-expression stored
        # alone (unparenthesized); it must be wrapped if another
        # condition is ever AND-joined to it
        condition_is_or = FALSE,
        star_prefix = FALSE,
        # When an explicit element name cannot be used as an XPath name
        # test (and so 'element' has been folded into a condition on
        # '*'), an equivalent node test for that name; NULL otherwise.
        # Lets the of-type pseudo-classes distinguish such elements from
        # the universal selector and count their siblings correctly.
        name_test = NULL,
        # Whether the leftmost compound of the selector contained
        # ':scope', anchoring the expression at the query's scoping
        # root: selector_to_xpath() then emits the self axis instead of
        # the usual prefix. join() keeps the flag of its left operand,
        # so it survives to the full complex selector; a ':scope' that
        # is not leftmost is rejected when the flagged expression turns
        # up as the right side of a combinator (or inside a
        # pseudo-class argument)
        scoped = FALSE,
        initialize = function(path = "", element = "*", star_prefix = FALSE) {
            self$path <- path
            self$element <- element
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
        add_condition = function(condition, is_or_group = FALSE) {
            # Always AND with the existing condition: an "or" (or a union,
            # see below) appended here would flatten into the accumulated
            # condition chain, changing its meaning. Callers wanting
            # alternatives must OR- or union-join them and add the result
            # as one condition, flagged with 'is_or_group'.
            #
            # Parenthesize only when needed. 'is_or_group' covers any
            # expression that a reader would need XPath's precedence rules
            # to see as a single unit once it sits beside an "and": an
            # "or", the only operator that binds more loosely than "and",
            # needs no parentheses while alone in the bracketed predicate;
            # a union ('|') binds tighter than "and" and so is already
            # correct unparenthesized, but is flagged the same way purely
            # for readability (xpath_has()). Defer them to the moment the
            # group is joined with another condition, on whichever side it
            # sits; the joined result is an and-chain, no longer a group.
            #
            # "0" - the condition a never-matching simple selector adds
            # (an empty ':is()', an impossible :nth-child(), a
            # substring match on the empty string, ...) - absorbs
            # everything AND-ed with it, in either order: once one
            # conjunct is constant-false the whole predicate is, so the
            # rest is noise to anyone reading the expression. Folding
            # here rather than at each call site catches the compound
            # whichever way round it was written, giving 'e.warning:is()'
            # the plain "e[0]" instead of a class test AND-ed with 0.
            if (identical(condition, "0")) {
                self$condition <- "0"
                self$condition_is_or <- FALSE
                return(invisible(NULL))
            }
            if (identical(self$condition, "0"))
                return(invisible(NULL))
            if (nzchar(self$condition)) {
                if (is_or_group)
                    condition <- paste0("(", condition, ")")
                if (self$condition_is_or) {
                    self$condition <- paste0("(", self$condition, ")")
                    self$condition_is_or <- FALSE
                }
                self$condition <- paste0(self$condition, " and ", condition)
            } else {
                self$condition <- condition
                self$condition_is_or <- is_or_group
            }
        },
        add_predicate = function(predicate) {
            self$predicates <- c(self$predicates, predicate)
        },
        add_name_test = function(as_predicate = FALSE) {
            if (self$element == "*")
                return()
            if (is_safe_nodetest(self$element)) {
                # A name that can be written as an XPath name test is
                # matched on the self axis, giving it exactly the
                # semantics of the bare name test in a path step: an
                # unprefixed name matches the null namespace only, and
                # a prefix resolves through the namespace map supplied
                # at evaluation time. Comparing name() instead would
                # make the same name mean different things depending on
                # where it sits in the selector - matching a *default*
                # namespace too, so that ':is(p)' selected elements a
                # top-level 'p' does not, and testing a prefixed name
                # against the document's literal prefix, not its URI.
                test <- paste0("self::", self$element)
                if (as_predicate)
                    self$add_predicate(test)
                else
                    self$add_condition(test)
                self$name_test <- self$element
            } else {
                # A name XPath cannot express as a name test (e.g. one
                # starting with a digit) has to be compared against
                # name(), which - having no namespace map to consult -
                # also matches the name in a default namespace. This
                # branch is only reached from the element translation
                # itself, so the looser match applies wherever such a
                # name appears, top level included.
                self$add_condition(paste0("name() = ",
                                          xpath_literal(self$element)))
                self$name_test <- paste0("*[name() = ",
                                         xpath_literal(self$element), "]")
            }
            self$element <- "*"
        },
        join = function(combiner, other) {
            self$path <- paste0(self$str(), combiner, other$path)
            self$element <- other$element
            self$predicates <- other$predicates
            self$condition <- other$condition
            self$condition_is_or <- other$condition_is_or
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
# as an XPath node test. The local part may be the wildcard '*'
# ('svg:*' matches any element in the namespace bound to 'svg'), but a
# prefix must be a name
is_safe_nodetest <- function(name) {
    parts <- strsplit(name, ":", fixed = TRUE)[[1]]
    n <- length(parts)
    (n == 1 || n == 2) &&
        (parts[n] == "*" || is_safe_name(parts[n])) &&
        (n == 1 || is_safe_name(parts[1]))
}

# The XPath node test matching the same elements as the subject of an
# of-type pseudo-class, or NULL when the subject is the universal
# selector. Selectors 4 does define the of-type pseudo-classes on '*'
# ("same expanded element name as its siblings"), but counting the
# siblings would mean comparing their names against the matched
# element's own name, which XPath 1.0 cannot express (no current()
# outside XSLT) - so that case is not implemented, an error shared
# with the Python cssselect library
of_type_nodetest <- function(xpath) {
    nodetest <- if (xpath$element != "*") xpath$element else xpath$name_test
    # A namespaced wildcard ('svg|*', kept as the node test 'svg:*')
    # is just as much the universal selector for this purpose: counting
    # those siblings would group them by namespace rather than by
    # expanded name, so it shares the error instead of silently
    # translating to different semantics
    if (is.null(nodetest) || grepl("(^|:)\\*$", nodetest))
        NULL
    else
        nodetest
}

# of_type_nodetest(), raising the shared "not implemented" error for
# an of-type pseudo-class whose subject is the universal selector.
# 'name' is the pseudo-class as it should appear in the message, e.g.
# "nth-of-type()" or "first-of-type".
of_type_nodetest_or_stop <- function(xpath, name) {
    nodetest <- of_type_nodetest(xpath)
    if (is.null(nodetest))
        translation_stop(paste0("*:", name, " is not implemented"),
                         paste0("*:", name))
    nodetest
}

# A translation failure: valid CSS that names a feature the current
# translator cannot express as XPath 1.0 (e.g. a non-leading ':scope',
# or an of-type pseudo-class on the universal selector). Raised without
# a 'selector' field so it can bubble up through the internal xpath()
# recursion undecorated; GenericTranslator$css_to_xpath() catches it at
# the boundary and adds the selector text, mirroring how parse()
# annotates a selectr_parse_error with a caret gutter.
translation_stop <- function(message, feature) {
    selectr_abort(message, "selectr_translation_error", feature = feature)
}

# Shared translation for pseudo-classes that can never match in a
# static document
pseudo_never_matches <- function(xpath) {
    xpath$add_condition("0")
    xpath
}

# ':scope' is only translatable as the leftmost compound of a
# top-level selector, where it anchors the expression at the context
# node (see xpath_scope_pseudo). Anywhere else - to the right of a
# combinator, or inside a functional pseudo-class argument - XPath 1.0
# has no way to refer back to the node the query started from
stop_non_leading_scope <- function() {
    translation_stop(
        "The pseudo-class :scope is only supported at the start of a selector",
        ":scope")
}

# A wildcard in non-trailing position (e.g. :lang(*-CH) or :lang(de-*-DE),
# quoted or not) is a valid RFC 4647 extended-filtering range. The HTML
# translators approximate it from the nearest language-attributed
# ancestor, but the generic translator's only tool is XPath 1.0's
# lang() function, which can express a prefix match but not an interior
# wildcard, so it rejects such ranges rather than silently mismatching.
stop_lang_interior_wildcard <- function(range) {
    translation_stop(
        paste0("Only a bare '*' or a trailing '...-*' wildcard is ",
               "supported by the generic translator's :lang(); the range ",
               range, " has a wildcard in a non-trailing position"),
        ":lang()")
}

# Classify a single (already reassembled) :lang() range:
#   "any"      - a bare "*" (match any language)
#   "exact"    - no wildcard, e.g. "en" or "en-GB"
#   "prefix"   - a single trailing wildcard, e.g. "en-*"
#   "extended" - a wildcard in any other position, e.g. "*-CH", "de-*-DE"
#                (RFC 4647 extended filtering)
lang_range_kind <- function(value) {
    n_star <- nchar(value) - nchar(gsub("*", "", value, fixed = TRUE))
    if (value == "*") {
        "any"
    } else if (n_star == 0) {
        "exact"
    } else if (n_star == 1 && grepl("-\\*$", value)) {
        "prefix"
    } else {
        "extended"
    }
}

# Whether an "exact" or "prefix" range (see lang_range_kind()) names
# more than one subtag once any trailing wildcard is stripped, e.g.
# "en-GB" and "en-GB-*" are multi-subtag but "en" and "en-*" are not.
# A range with more than one subtag needs RFC 4647 extended filtering
# to match correctly (subtags may be skipped between the ones named),
# not a plain prefix test - see lang_extended_html_condition().
lang_range_multi_subtag <- function(value) {
    grepl("-", sub("-\\*$", "", value), fixed = TRUE)
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
        translation_stop(
            paste0("Expected string, ident, or * arguments for :lang(), got ",
                   token_repr(fn$arguments[[which(!valid_types)[1]]])),
            ":lang()")
    }
}

# The language values named by the arguments of :lang(), combining an
# ident or string ending in '-' with a following '*' DELIM into a
# single wildcard range (e.g. "en-" + "*" = "en-*")
extract_lang_values <- function(fn) {
    # The tokenizer splits a range at every '*', so a wildcard range
    # arrives as several tokens: unquoted "*-CH" as ['*', "-CH"], "en-*"
    # as ["en-", '*'], and "de-*-DE" as ["de-", '*', "-DE"]. A quoted
    # range is a single STRING token carrying its wildcards verbatim.
    # Reassemble each whole range: a '*' glues onto a value ending in '-'
    # (the trailing-wildcard case), and a '-'-led continuation subtag
    # glues onto a value still ending in '*' (the part after a '*' split).
    # Commas between ranges are dropped during parsing, but a fresh range
    # never begins with '-', so the leading '-' reliably marks a
    # continuation rather than a new range.
    ranges <- character(0)
    for (arg in fn$arguments) {
        n <- length(ranges)
        if (arg$type == "DELIM" && arg$value == "*") {
            if (n > 0 && grepl("-$", ranges[n])) {
                ranges[n] <- paste0(ranges[n], "*")
            } else {
                ranges <- c(ranges, "*")
            }
        } else if (n > 0 && grepl("\\*$", ranges[n]) &&
                   startsWith(arg$value, "-")) {
            ranges[n] <- paste0(ranges[n], arg$value)
        } else {
            ranges <- c(ranges, arg$value)
        }
    }
    ranges
}

# The language string declared on an element, and the step selecting
# the nearest ancestor-or-self that declares one. HTML reads @lang;
# XHTML documents conventionally carry @xml:lang, often alongside
# @lang, and the HTML language determination gives @xml:lang precedence
# when both are present. XPath 1.0 has no conditional, so the
# preference is arithmetic: string-length(@lang) is multiplied by
# not(@xml:lang), truncating @lang to nothing whenever @xml:lang is
# there.
lang_attr_value <- function(xhtml) {
    if (xhtml)
        paste0("concat(@xml:lang, substring(@lang, 1, ",
               "string-length(@lang) * not(@xml:lang)))")
    else
        "@lang"
}

lang_attr_ancestor <- function(xhtml) {
    paste0("ancestor-or-self::*[",
           if (xhtml) "@xml:lang or @lang" else "@lang",
           "][1]")
}

# The same string lowercased, for the ASCII case-insensitive matching
# that language ranges use
lang_attr_value_lc <- function(xhtml) {
    paste0("translate(", lang_attr_value(xhtml),
           ", 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
           "'abcdefghijklmnopqrstuvwxyz')")
}

# The HTML :lang() translation of an RFC 4647 extended-filtering range
# (one with a wildcard in non-trailing position, e.g. "*-CH" or
# "de-*-DE"). It tests the nearest language-attributed ancestor,
# dash-bracketing the lowercased language string as "-<lang>-" so that
# each subtag is delimited, then walks the range's subtags left to
# right: a literal first subtag must start the tag, a literal subtag
# after a '*' may appear anywhere further along (contains), and
# substring-after threads the remaining tail so later subtags must
# follow earlier ones in order.
lang_extended_html_condition <- function(value, xhtml) {
    cursor <- paste0("concat('-', ", lang_attr_value_lc(xhtml), ", '-')")
    subtags <- strsplit(tolower(value), "-", fixed = TRUE)[[1]]
    conditions <- character(0)
    anywhere <- FALSE  # may the next literal subtag be preceded by others?
    anchored <- FALSE  # has a literal subtag been matched yet?
    for (subtag in subtags) {
        if (subtag == "*") {
            anywhere <- TRUE
            next
        }
        if (!nzchar(subtag))
            next
        needle <- xpath_literal(paste0("-", subtag, "-"))
        if (!anchored && !anywhere) {
            conditions <- c(conditions,
                            paste0("starts-with(", cursor, ", ", needle, ")"))
        } else {
            conditions <- c(conditions,
                            paste0("contains(", cursor, ", ", needle, ")"))
        }
        cursor <- paste0("substring-after(", cursor, ", ",
                         xpath_literal(paste0("-", subtag)), ")")
        anywhere <- FALSE
        anchored <- TRUE
    }
    paste0(lang_attr_ancestor(xhtml), "[",
           paste(conditions, collapse = " and "), "]")
}

first_class_name <- function(obj) {
    if (!is.null(obj$repr_name)) obj$repr_name else class(obj)[1]
}

# 'type' is an HTML enumerated attribute whose keywords match ASCII
# case-insensitively, but an HTML parser preserves attribute *values*,
# so a spelling such as type="RADIO" reaches XPath unchanged. Fold the
# value to lower case before comparing so the form pseudo-classes accept
# the uppercase keywords the way browsers do
fold_type <- "translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"

# A disabled <fieldset> disables its descendant controls - and its
# descendant <fieldset>s - except those inside its first <legend> child
# (HTML's "actually disabled" rule). A
# control is disabled by a fieldset when it has more disabled-fieldset
# ancestors than first-legend ancestors that protect it: each protecting
# legend (first child of a disabled fieldset) cancels exactly the one
# fieldset it belongs to, so nested disabled fieldsets still disable.
# Element names are matched by local-name() rather than name() so these
# fragments work under a namespaced (XHTML) document too - see the
# xhtml translator's local-name() convention for type selectors
disabled_by_fieldset <- paste0(
    "count(ancestor::*[local-name() = 'fieldset'][@disabled])",
    " > count(ancestor::*[local-name() = 'legend']",
    "[not(preceding-sibling::*[local-name() = 'legend'])]",
    "[parent::*[local-name() = 'fieldset'][@disabled]])")

# An <optgroup> or an <option> is also "actually disabled" when its
# nearest ancestor <select> is disabled. HTML's walk for that select
# stops at the first ancestor that is a <select>, <datalist>, <hr> or
# <option>, so naming those elements and taking [1] - the ancestor axis
# is a reverse axis, so position 1 is the nearest - gives the node the
# walk lands on, and the remaining predicates ask whether it is a
# disabled <select>. An <option> inside a <datalist> nested in a
# disabled <select> is left enabled, as the walk returns the <datalist>
# rather than reaching the <select>.
# One corner is approximated, as the <legend> counting above is: the
# walk also gives up once it has passed a second <optgroup> ancestor,
# which this expression cannot count, so an <option> nested two
# <optgroup>s deep (non-conforming markup) inside a disabled <select>
# is reported disabled here
nearest_select_disabled <- paste0(
    "ancestor::*[local-name() = 'select' or local-name() = 'datalist'",
    " or local-name() = 'hr' or local-name() = 'option'][1]",
    "[local-name() = 'select'][@disabled]")

# An <option>'s own disabledness: its @disabled, or the nearest ancestor
# among <optgroup>, <select>, <datalist>, <hr> and <option> being a
# disabled <optgroup>. An <option> can be a nested descendant of an
# <optgroup> in the customizable-<select> markup, so this is a walk up
# to the nearest of those elements rather than a test on the parent
option_own_disabled <- paste0(
    "@disabled or ancestor::*[local-name() = 'optgroup'",
    " or local-name() = 'select' or local-name() = 'datalist'",
    " or local-name() = 'hr' or local-name() = 'option'][1]",
    "[local-name() = 'optgroup'][@disabled]")

# The compound's element name, for pruning the HTML pseudo-class
# disjunctions below against it - but only when 'xpath$element' is a
# plain, unprefixed name usable as an XPath name test. is_safe_name()
# already excludes the universal selector '*' and a namespaced name such
# as 'svg:input' (a colon fails the regex); an unsafe name has already
# been folded into a name() condition by add_name_test(), which resets
# 'element' to '*' - so both cases fall through to the same NULL result,
# meaning "unknown, keep every disjunct". This also means a pseudo-class
# argument translated against a fresh, element-less selector (e.g. the
# ':checked' in 'input:not(:checked)', which xpath_argument_condition()
# translates from '*') is never pruned - exactly as it should be, since
# that '*' says nothing about the candidate element the compound pins.
known_local_element <- function(xpath) {
    if (is_safe_name(xpath$element)) xpath$element else NULL
}

# OR-join 'disjuncts' - each list(element, condition, is_or = FALSE) -
# where 'condition' assumes the context node is already known to be
# 'element' (no local-name(.) test of its own), and 'is_or' says whether
# 'condition' is itself a bare top-level "or" expression that needs
# parentheses once joined with something else. Without this pruning,
# the HTML pseudo-class methods below would build a fixed disjunction
# over every element the HTML standard gives the pseudo-class, even
# though the compound had already pinned the element down (e.g.
# 'input:checked' would carry the entire never-firing 'option'
# disjunct, whose local-name(.) test can never be true once 'input' is
# already known).
#
# When the compound's element is not known (known_local_element() is
# NULL - a bare ':checked', or one inside :not()/:is() applied to '*'),
# every disjunct must still test its own local-name(.), exactly as
# before this pruning existed.
#
# When it is known and no disjunct names it, the predicate is genuinely
# always-false for this element, and add_condition("0") is correct -
# this is not the "no bare never-match" policy further down in this file
# (the GenericTranslator "Policy:" comment above xpath_checked_pseudo and
# its never-matching neighbours); that policy refuses to pretend an
# *unsupported* pseudo-class matches something, while this is a
# supported pseudo-class whose disjuncts have each been shown
# unreachable for this specific, statically-known element.
add_disjunction <- function(xpath, disjuncts) {
    known <- known_local_element(xpath)
    if (!is.null(known)) {
        disjuncts <- Filter(function(d) identical(d$element, known),
                            disjuncts)
        if (!length(disjuncts)) {
            xpath$add_condition("0")
            return(xpath)
        }
        conditions <- vapply(disjuncts, `[[`, character(1), "condition")
        is_or <- length(conditions) > 1 || isTRUE(disjuncts[[1]]$is_or)
    } else {
        conditions <- vapply(disjuncts, function(d)
            paste0("local-name(.) = ", xpath_literal(d$element),
                  " and (", d$condition, ")"), character(1))
        is_or <- length(conditions) > 1
    }
    xpath$add_condition(paste(conditions, collapse = " or "),
                        is_or_group = is_or)
    xpath
}

# The disjuncts shared by xpath_required_pseudo() and
# xpath_optional_pseudo(): both partition the same element set (input,
# select, textarea) that can take @required, and differ only in whether
# 'required_condition' is '@required' or 'not(@required)'
required_optional_disjuncts <- function(required_condition) {
    list(
        list(element = "input",
             condition = paste0(required_condition, " and not(", fold_type,
                                " = 'hidden')")),
        list(element = "select", condition = required_condition),
        list(element = "textarea", condition = required_condition))
}

# XPath 1.0's Number production has no exponent form, and R's default
# formatting switches to scientific notation for large round values
# (and honours options(scipen=)), so every number written into an
# expression goes through here rather than being pasted directly.
xpath_number <- function(number) {
    format(number, scientific = FALSE, trim = TRUE)
}

xpath_literal <- function(literal) {
    if (!is.character(literal) || length(literal) != 1) {
        internal_stop("literal must be a single character string")
    }

    if (!nzchar(literal)) {
        return("''")
    }

    if (!grepl("'", literal, fixed = TRUE)) {
        return(paste0("'", literal, "'"))
    }
    if (!grepl('"', literal, fixed = TRUE)) {
        return(paste0('"', literal, '"'))
    }

    # XPath 1.0 string literals have no escape mechanism, so a literal
    # containing both quote characters must be split apart and
    # rejoined with concat(): each maximal run of "'" is wrapped in
    # double quotes, and each maximal run of everything else (which
    # may itself contain '"') in single quotes.
    runs <- regmatches(literal, gregexpr("'+|[^']+", literal))[[1]]
    is_sq_run <- substring(runs, 1, 1) == "'"
    parts <- ifelse(is_sq_run, paste0('"', runs, '"'), paste0("'", runs, "'"))
    paste0("concat(", paste(parts, collapse = ","), ")")
}

# HTML form controls to which the 'readonly' content attribute applies
# (the HTML Standard's list of textual/numeric/date input types); an
# <input> with no 'type' attribute defaults to 'text', which is in the
# list, so a missing attribute counts as present
readonly_capable_input_types <- c("text", "search", "url", "tel", "email",
                                  "password", "date", "month", "week",
                                  "time", "datetime-local", "number")
readonly_capable_condition <- paste0(
    "not(@type) or ",
    paste(paste0(fold_type, " = ",
                vapply(readonly_capable_input_types, xpath_literal,
                       character(1))), collapse = " or "))

# 'contenteditable' without a value, or set to "inherit", actually
# inherits editability from the nearest ancestor that sets it, which a
# document with no live DOM cannot resolve; this static approximation of
# ':read-write' therefore considers only an element's own attribute
fold_contenteditable <- paste0(
    "translate(@contenteditable, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', ",
    "'abcdefghijklmnopqrstuvwxyz')")
contenteditable_condition <- paste0(
    "@contenteditable and not(", fold_contenteditable, " = 'false')")

# The condition for xpath_read_write_pseudo() (':read-only' is simply
# its negation): an <input> whose type takes 'readonly' or a <textarea>,
# so long as neither its own @readonly nor @disabled (including
# disabling by an ancestor <fieldset>, as for xpath_disabled_pseudo)
# applies; or any element carrying its own @contenteditable. Returns the
# condition text together with whether it is a bare top-level "or" (see
# add_condition()) needing parentheses once joined with another
# condition - true whenever a form-control branch is included, since it
# is then always OR-ed with the contenteditable branch
read_write_condition <- function(xpath) {
    known <- known_local_element(xpath)
    mutable_form_control <- paste0(
        "not(@readonly) and not(@disabled or (", disabled_by_fieldset, "))")
    input_branch <- paste0("(", readonly_capable_condition, ") and ",
                           mutable_form_control)
    form_branch <- if (identical(known, "input")) {
        input_branch
    } else if (identical(known, "textarea")) {
        mutable_form_control
    } else if (is.null(known)) {
        paste0("(local-name(.) = 'input' and (", input_branch, ")) or ",
              "(local-name(.) = 'textarea' and (", mutable_form_control, "))")
    } else {
        NULL
    }
    if (is.null(form_branch))
        list(condition = contenteditable_condition, is_or = FALSE)
    else
        list(condition = paste(form_branch, "or", contenteditable_condition),
             is_or = TRUE)
}

# The static definition of an HTML submit button (button/submit), shared
# between the ':default' submit-button branch below and its own
# lookup of the first such control within the nearest enclosing <form>
submit_control_condition <- paste0(
    "(local-name(.) = 'button' and (not(@type) or ", fold_type,
    " = 'submit')) or (local-name(.) = 'input' and (", fold_type,
    " = 'submit' or ", fold_type, " = 'image'))")

# An enclosing <form>, matched by local-name() for the same reason
# disabled_by_fieldset above is. The ancestor axis is a reverse axis, so
# adding '[1]' to this picks the *nearest* enclosing form
form_ancestor <- "ancestor::*[local-name() = 'form']"

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
        css_to_xpath = function(css, prefix = "descendant-or-self::") {
            tryCatch({
                selectors <- parse(css)

                for (selector in selectors) {
                    if (first_class_name(selector) == "Selector" &&
                        !is.null(selector$pseudo_element))
                        translation_stop("Pseudo-elements are not supported.",
                                         paste0("::", selector$pseudo_element))
                }

                char_selectors <-
                    sapply(selectors,
                           function(selector)
                               self$selector_to_xpath(selector, prefix))

                paste0(char_selectors, collapse = " | ")
            },
            selectr_translation_error = function(e) {
                # Re-signal at the css_to_xpath() boundary so the
                # condition gains the selector text, mirroring how
                # parse() annotates a selectr_parse_error.
                selectr_abort(conditionMessage(e), "selectr_translation_error",
                             feature = e$feature, selector = css)
            },
            error = function(e) {
                # A selector that nests functional pseudo-classes very
                # deeply (e.g. hundreds of :not()) overflows R's
                # expression nesting limit. R >= 4.3 raises a dedicated
                # `expressionStackOverflowError`; older R raises a plain
                # error with the same message text, so fall back to
                # matching that.
                if (inherits(e, "expressionStackOverflowError") ||
                    grepl("nested too deeply", conditionMessage(e), fixed = TRUE)) {
                    selectr_abort("selector nests functional pseudo-classes too deeply",
                                 "selectr_translation_error", selector = css)
                }
                stop(e)
            })
        },
        selector_to_xpath = function(selector, prefix = "descendant-or-self::") {
            tree <- selector$parsed_tree
            xpath <- self$xpath(tree)
            if (!inherits(xpath, "XPathExpr"))
                internal_stop("'xpath' is not an instance of 'XPathExpr'")
            # A selector starting with ':scope' is anchored at the
            # query's scoping root - the context node the expression is
            # evaluated from - so the self axis replaces the supplied
            # prefix (which would instead range over the descendants):
            # ':scope > a' becomes 'self::*/a' and a bare ':scope'
            # becomes 'self::*'
            if (xpath$scoped)
                prefix <- "self::"
            paste0(prefix, xpath$str())
        },
        xpath = function(parsed_selector) {
            type_name <- first_class_name(parsed_selector)
            method <- self[[paste0("xpath_", tolower(type_name))]]
            if (is.null(method))
                internal_stop("Unknown method name '", type_name, "'")
            method(parsed_selector)
        },
        xpath_combinedselector = function(combined) {
            # Fold the chain of combinators in a loop, translating each
            # right-hand compound in turn; see 'combinator_spine' for
            # why this is not written as a recursion
            spine <- combinator_spine(combined)
            left <- self$xpath(spine$leftmost)
            for (node in spine$nodes) {
                combinator <- self$combinator_mapping[node$combinator]
                method <- self[[paste0("xpath_", combinator, "_combinator")]]
                if (is.null(method))
                    internal_stop("Unknown combinator '", combinator, "'")
                right <- self$xpath(node$subselector)
                if (right$scoped)
                    stop_non_leading_scope()
                left <- method(left = left, right = right)
            }
            left
        },
        xpath_argument_condition = function(subselector) {
            # Translate one functional pseudo-class argument into a
            # condition on the candidate element, returned as
            # list(condition, is_or) where 'is_or' marks a top-level
            # or-expression (see 'condition_is_or'). A complex argument
            # (CombinedSelector) applies its rightmost compound to the
            # candidate, with everything to its left becoming an
            # existence test through reversed axes (e.g. :is(a > b)
            # matches a 'b' whose parent is an 'a')
            if (first_class_name(subselector) == "CombinedSelector") {
                sub_xpath <- self$xpath(subselector$subselector)
                if (sub_xpath$scoped)
                    stop_non_leading_scope()
                sub_xpath$add_name_test()
                rev_test <- self$reversed_combinator_test(
                    subselector$selector, subselector$combinator)
                condition <-
                    if (identical(sub_xpath$condition, "0")) {
                        # A rightmost compound that can never match makes
                        # the whole argument impossible; drop the
                        # existence test, as add_condition() would
                        "0"
                    } else if (nzchar(sub_xpath$condition)) {
                        cond <- sub_xpath$condition
                        # The condition becomes one operand of an
                        # "and", so a stored or-group needs its
                        # parentheses now
                        if (sub_xpath$condition_is_or)
                            cond <- paste0("(", cond, ")")
                        paste0(cond, " and ", rev_test)
                    } else
                        rev_test
                list(condition = condition, is_or = FALSE)
            } else {
                sub_xpath <- self$xpath(subselector)
                if (sub_xpath$scoped)
                    stop_non_leading_scope()
                sub_xpath$add_name_test()
                # An argument that imposes no condition (a bare '*')
                # matches everything; return an explicit "true()" so
                # callers can tell "always true" apart from "no
                # condition" instead of silently dropping the argument
                # from the selector list
                if (nzchar(sub_xpath$condition))
                    list(condition = sub_xpath$condition,
                         is_or = sub_xpath$condition_is_or)
                else
                    list(condition = "true()", is_or = FALSE)
            }
        },
        selector_list_condition = function(selector_list) {
            # OR-join the conditions imposed by a selector list's
            # arguments into a single list(condition, is_or). NULL when
            # the list imposes no condition: either it is absent, or
            # one of its arguments (e.g. the universal selector '*') is
            # always true, making the whole list match unconditionally.
            # A single-argument list is an or-group only if that
            # argument's own condition is one (e.g. a nested :is())
            if (is.null(selector_list) || length(selector_list) == 0)
                return(NULL)
            conditions <- lapply(selector_list,
                                 self$xpath_argument_condition)
            exprs <- vapply(conditions, `[[`, character(1), "condition")
            if (any(exprs == "true()"))
                return(NULL)
            list(condition = paste0(exprs, collapse = " or "),
                 is_or = length(exprs) > 1 || conditions[[1]]$is_or)
        },
        reversed_combinator_test = function(selector, combinator) {
            # Existence test, relative to the candidate element, for the
            # left-hand side of a combinator inside a pseudo-class
            # argument: ' ' -> an ancestor, '>' -> the parent, '~' -> any
            # preceding sibling, '+' -> the immediately preceding sibling.
            # The left-hand side may itself be complex, so recurse
            inner <- self$xpath_argument_condition(selector)$condition
            axis <-
                if (combinator == " ") "ancestor::*"
                else if (combinator == ">") "parent::*"
                else if (combinator == "~") "preceding-sibling::*"
                else if (combinator == "+") "preceding-sibling::*[1]"
                else internal_stop("Unknown combinator '", combinator, "'")
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
                xpath$add_condition(paste0("not(", condition$condition, ")"))
            }
            xpath
        },
        xpath_matching = function(matching) {
            xpath <- self$xpath(matching$selector)

            # An empty forgiving list (':is()') has no alternative to
            # satisfy, so it matches nothing
            if (length(matching$selector_list) == 0) {
                xpath$add_condition("0")
                return(xpath)
            }

            # Add the OR of the argument conditions (any match suffices)
            # as a single condition so the alternatives stay grouped and
            # AND with the rest of the compound selector; a list that
            # matches everything (e.g. :is(a, *)) imposes no condition
            condition <- self$selector_list_condition(matching$selector_list)
            if (!is.null(condition)) {
                xpath$add_condition(condition$condition, condition$is_or)
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
                if (sub_xpath$scoped)
                    stop_non_leading_scope()
                # The name stays the node test of the path step itself
                # (e.g. '//svg:g'), except under '+', where the
                # position predicate [1] must come before the name test
                # and so the name has to move into the predicate.
                if (selector$combinator == "+")
                    sub_xpath$add_name_test()
                joiner <-
                    if (selector$combinator == " ") "//"
                    else if (selector$combinator == ">") "/"
                    else if (any(selector$combinator == c("~", "+")))
                        "/following-sibling::"
                    else internal_stop("Unknown combinator '",
                                       selector$combinator, "'")
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
                if (sub_xpath$scoped)
                    stop_non_leading_scope()
                # As above: the name is the node test of the axis
                # step, except under '+' where [1] must precede it
                if (combinator == "+")
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
                # Flag a multi-argument union with is_or_group so it is
                # parenthesized if AND-joined with anything else later - a
                # single argument needs no parentheses either way and is
                # left exactly as add_condition() would render it bare.
                xpath$add_condition(combined_condition,
                                    is_or_group = length(conditions) > 1)
            }

            xpath
        },
        # Look up the method implementing a (functional) pseudo-class,
        # or NULL if there is none. CSS pseudo-class names are
        # hyphenated; the method name replaces '-' with '_', so a name
        # containing an underscore is rejected up front, otherwise
        # ':first_child' would alias ':first-child' instead of being
        # reported as unknown
        pseudo_method = function(name, suffix) {
            if (grepl("_", name, fixed = TRUE))
                return(NULL)
            self[[paste0("xpath_", gsub("-", "_", name), suffix)]]
        },
        # pseudo_method(), erroring with the pseudo-class as it should
        # appear in the message ('suffix' distinguishes a functional
        # pseudo-class's trailing '()' from a plain one) when there is
        # no such method
        resolve_pseudo_method = function(name, suffix) {
            method <- self$pseudo_method(name, suffix)
            if (is.null(method)) {
                label <- paste0(":", name,
                                if (suffix == "_function") "()" else "")
                translation_stop(
                    paste0("The pseudo-class ", label, " is unknown"), label)
            }
            method
        },
        xpath_function = function(fn) {
            xp <- self$xpath(fn$selector)
            method <- self$resolve_pseudo_method(fn$name, "_function")
            method(xp, fn)
        },
        xpath_pseudo = function(pseudo) {
            xp <- self$xpath(pseudo$selector)
            method <- self$resolve_pseudo_method(pseudo$ident, "_pseudo")
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
                    safe <- safe && is_safe_name(selector$namespace)
                }
                if (safe) {
                    attrib <- paste0("@", name)
                } else {
                    attrib <- paste0(
                        "attribute::*[name() = ", xpath_literal(name), "]")
                }
            }
            value <- selector$value

            xp <- self$xpath(selector$selector)
            if (identical(selector$flag, "i") &&
                !is.null(value) && nzchar(value)) {
                # '[attr="value" i]': match the value ASCII
                # case-insensitively, so compare the ASCII-lowercased
                # attribute against the ASCII-lowercased value. (An
                # explicit 's' flag, the default anyway, opts out of
                # this and leaves the comparison case-sensitive.)
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
                internal_stop("Unknown attribute operator '", operator, "'")
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
                    # has to be pinned explicitly alongside the name test.
                    xpath <- XPathExpr$new(element = element)
                    xpath$add_name_test()
                    xpath$add_condition("namespace-uri() = ''")
                    # The of-type nodetest must carry the namespace pin
                    # set by the condition above
                    xpath$name_test <- paste0("*[name() = ",
                                              xpath_literal(element),
                                              " and namespace-uri() = '']")
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
            a <- fn$series[1]
            b <- fn$series[2]

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
                    xpath$add_condition(condition$condition, condition$is_or)
                }
                return(xpath)
            }
            # early-exit condition 2:
            # ~~~~~~~~~~~~~~~~~~~~~~~
            # an+b-1 siblings with a<=0 and (b-1)<0 is not possible: for
            # a<0, an+b-1 only decreases from its already-negative value
            # at n=0; for a==0 it is fixed at b-1, which is < 0
            if (a <= 0 && b_min_1 < 0) {
                xpath$add_condition("0")

                # CSS Level 4: an 'of S' argument is still translated,
                # so an argument this translator cannot express (e.g. a
                # non-leading ':scope') is reported here as it would be
                # for any other count. Its condition is then dropped by
                # add_condition(), which folds anything AND-ed with the
                # always-false "0": the element matches nothing whether
                # or not it matches S
                condition <- self$selector_list_condition(fn$selector_list)
                if (!is.null(condition)) {
                    xpath$add_condition(condition$condition, condition$is_or)
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
                else paste0("[", selector_list_cond$condition, "]")

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
                xpath$add_condition(paste0(siblings_count, " = ",
                                           xpath_number(b_min_1)))

                # CSS Level 4: When selector list is provided, ensure current element matches
                if (!is.null(selector_list_cond)) {
                    xpath$add_condition(selector_list_cond$condition,
                                        selector_list_cond$is_or)
                }

                return(xpath)
            }

            expr <- character(0)

            if (a > 0) {
                # siblings count, an+b-1, is always >= 0,
                # so if a>0, and (b-1)<=0, an "n" exists to satisfy this,
                # therefore, the predicate is only interesting if (b-1)>0
                if (b_min_1 > 0) {
                    expr <- c(expr, paste0(siblings_count, " >= ",
                                           xpath_number(b_min_1)))
                }
            } else {
                # if a<0, and (b-1)<0, no "n" satisfies this,
                # this is tested above as an early exist condition
                # otherwise,
                expr <- c(expr, paste0(siblings_count, " <= ",
                                       xpath_number(b_min_1)))
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
                    b_neg <- paste0("+ ", xpath_number(b_neg))
                    left <- paste0("(", left, " ", b_neg, ")")
                }

                expr <- c(expr, paste0(left, " mod ", xpath_number(a), " = 0"))
            }

            if (length(expr)) {
                expr <- paste0(expr, collapse = " and ")
                xpath$add_condition(expr)
            }

            # CSS Level 4: When selector list is provided, ensure current element matches
            if (!is.null(selector_list_cond)) {
                xpath$add_condition(selector_list_cond$condition,
                                    selector_list_cond$is_or)
            }

            xpath
        },
        xpath_nth_last_child_function = function(xpath, fn) {
            self$xpath_nth_child_function(xpath, fn, last = TRUE)
        },
        xpath_nth_of_type_function = function(xpath, fn) {
            of_type_nodetest_or_stop(xpath, "nth-of-type()")
            self$xpath_nth_child_function(xpath, fn, add_name_test = FALSE)
        },
        xpath_nth_last_of_type_function = function(xpath, fn) {
            of_type_nodetest_or_stop(xpath, "nth-last-of-type()")
            self$xpath_nth_child_function(xpath, fn, last = TRUE,
                                          add_name_test = FALSE)
        },
        xpath_lang_function = function(xpath, fn) {
            validate_lang_args(fn)
            lang_values <- extract_lang_values(fn)

            # Build conditions for each language range
            conditions <- vapply(lang_values, function(value) {
                # Wildcard * matches any element whose language is
                # known, i.e. one that inherits a non-empty xml:lang
                # from its nearest xml:lang-bearing ancestor-or-self
                # (xml:lang="" resets the language to unknown). The
                # "xml" prefix is bound in every XPath context, so the
                # attribute can be walked directly
                known <- "ancestor-or-self::*[@xml:lang][1][string-length(@xml:lang) > 0]"
                kind <- lang_range_kind(value)
                if (kind == "any") {
                    known
                } else if (value == "") {
                    # Selectors 4 defines :lang("") as matching elements
                    # whose content language is *not* tagged at all - the
                    # negation of "any" above, not a literal xml:lang=""
                    # comparison (which lang() would otherwise produce)
                    paste0("not(", known, ")")
                } else if (kind == "prefix") {
                    # Wildcard suffix like "en-*" - match any language starting with prefix
                    # Use XPath's lang() function which does prefix matching.
                    # Strip the trailing "-*": lang('en') matches "en" and any
                    # "en-..." tag, whereas lang('en-') would match nothing
                    # because lang() only extends its argument at a '-' boundary.
                    prefix <- sub("-?\\*$", "", value)
                    paste0("lang(", xpath_literal(prefix), ")")
                } else if (kind == "extended") {
                    # A wildcard in non-trailing position (e.g. "*-CH"):
                    # XPath 1.0's lang() cannot express RFC 4647 extended
                    # filtering, and unlike the HTML translators there is
                    # no lang-attribute to walk, so reject it
                    stop_lang_interior_wildcard(value)
                } else {
                    # Regular language tag
                    paste0("lang(", xpath_literal(value), ")")
                }
            }, character(1), USE.NAMES = FALSE)

            # Combine conditions with OR; more than one alternative
            # forms an or-group, which add_condition() parenthesizes
            # if it is ever joined with another condition
            if (length(conditions) > 0) {
                xpath$add_condition(paste(conditions, collapse = " or "),
                                    is_or_group = length(conditions) > 1)
            }

            xpath
        },
        xpath_dir_function = function(xpath, fn) {
            # :dir() takes exactly one identifier (CSS Selectors Level 4).
            # A lone '-' lexes as an IDENT but is not a valid <ident>
            # per css-syntax, so reject it too.
            if (length(fn$arguments) != 1 ||
                fn$arguments[[1]]$type != "IDENT" ||
                fn$arguments[[1]]$value == "-") {
                translation_stop(
                    paste0("Expected a single ident argument for :dir(), got ",
                           token_repr(fn$arguments[[1]])),
                    ":dir()")
            }
            # :dir() requires runtime directionality detection based on
            # document language, inherited dir attributes, and text analysis.
            # Not possible in static XPath, so we make it never match.
            #
            # Deliberately not overridden on the HTML translator either:
            # an ancestor-or-self::*[@dir][1] walk parallel to the HTML
            # :lang() would approximate it, but resolved directionality
            # (selectors-4 section 9.4) also turns on dir=auto, bdi, and
            # form-control rules a static document cannot answer, so the
            # honest translation is "no match" (cssselect does the
            # same). If demand ever justifies the approximation,
            # implement it in lockstep with selectrs
            xpath$add_condition("0")
            xpath
        },
        xpath_scope_pseudo = function(xpath) {
            # ':scope' matches only the query's scoping root, i.e. the
            # context node the expression is evaluated from. There is
            # no condition to add - any other simple selectors in the
            # compound already constrain the node - so just flag the
            # expression; selector_to_xpath() anchors a flagged
            # selector with 'self::' in place of the prefix, and the
            # call sites that cannot anchor it (the right side of a
            # combinator, pseudo-class arguments) reject the flag
            xpath$scoped <- TRUE
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
            nodetest <- of_type_nodetest_or_stop(xpath, "first-of-type")
            xpath$add_condition(paste0(
                "count(preceding-sibling::", nodetest, ") = 0"))
            xpath
        },
        xpath_last_of_type_pseudo = function(xpath) {
            nodetest <- of_type_nodetest_or_stop(xpath, "last-of-type")
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
            nodetest <- of_type_nodetest_or_stop(xpath, "only-of-type")
            xpath$add_condition(paste0(
                "count(preceding-sibling::", nodetest, ") = 0 and ",
                "count(following-sibling::", nodetest, ") = 0"))
            xpath
        },
        xpath_empty_pseudo = function(xpath) {
            # Selectors 3 semantics, deliberately: white-space-only
            # elements do not match, which is what every browser
            # implements (checked June 2026). The Selectors 4 TR
            # loosening - not(normalize-space()) - has shipped nowhere;
            # revisit if browsers move
            xpath$add_condition("not(*) and not(string-length())")
            xpath
        },

        # Pseudo-classes that depend on dynamic state which a static
        # document does not have; the HTML translator overrides the
        # ones it can answer from attributes.
        #
        # Policy: a runtime-state family is either accepted in full
        # (every member listed here, so that e.g. ':focus' and
        # ':focus-within' behave alike) or not at all - anything not
        # listed stays a "pseudo-class is unknown" error, keeping typos
        # detectable. A pseudo-class whose state is readable from
        # document attributes belongs here only together with a real
        # translation on the HTML translator (as for ':checked' and
        # ':required'): a bare never-match entry would replace a
        # missing feature with silently wrong answers
        xpath_any_link_pseudo = pseudo_never_matches,
        xpath_link_pseudo     = pseudo_never_matches,
        xpath_visited_pseudo  = pseudo_never_matches,
        xpath_hover_pseudo    = pseudo_never_matches,
        xpath_active_pseudo   = pseudo_never_matches,
        xpath_focus_pseudo    = pseudo_never_matches,
        xpath_focus_within_pseudo  = pseudo_never_matches,
        xpath_focus_visible_pseudo = pseudo_never_matches,
        xpath_target_pseudo   = pseudo_never_matches,
        xpath_target_within_pseudo = pseudo_never_matches,
        xpath_local_link_pseudo    = pseudo_never_matches,
        xpath_enabled_pseudo  = pseudo_never_matches,
        xpath_disabled_pseudo = pseudo_never_matches,
        xpath_checked_pseudo  = pseudo_never_matches,
        # The required/optional state is an HTML form notion; the
        # HTML translator answers it from the @required attribute
        xpath_required_pseudo = pseudo_never_matches,
        xpath_optional_pseudo = pseudo_never_matches,
        # Likewise editability, the shown-placeholder state and the
        # default form control are HTML notions the HTML translator
        # answers from attributes (see read_write_condition() and
        # xpath_placeholder_shown_pseudo()/xpath_default_pseudo() below)
        xpath_read_write_pseudo = pseudo_never_matches,
        xpath_read_only_pseudo  = pseudo_never_matches,
        xpath_placeholder_shown_pseudo = pseudo_never_matches,
        xpath_default_pseudo = pseudo_never_matches,

        xpath_attrib_exists = function(xpath, name, value) {
            xpath$add_condition(name)
            xpath
        },
        xpath_attrib_equals = function(xpath, name, value) {
            xpath$add_condition(paste0(name, " = ", xpath_literal(value)))
            xpath
        },
        # The four methods below (and xpath_attrib_dashmatch just after)
        # omit the "name and " existence guard cssselect prepends to each
        # condition: with no such attribute, every one of these tests is
        # already false, because each nzchar(value) branch below sends the
        # empty-value case - the one case where that would not hold - to
        # add_condition("0") instead.
        xpath_attrib_includes = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value) &&
                grepl("^[^ \t\r\n\f]+$", value)) {
                xpath$add_condition(paste0(
                    "contains(concat(' ', normalize-space(",
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
                " = ",
                xpath_literal(value),
                " or starts-with(",
                name,
                ", ",
                xpath_literal(paste0(value, "-")),
                ")"), is_or_group = TRUE)
            xpath
        },
        xpath_attrib_prefixmatch = function(xpath, name, value) {
            if (!is.null(value) && nzchar(value)) {
                xpath$add_condition(paste0(
                    "starts-with(",
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
                    "substring(",
                    name,
                    ", string-length(",
                    name,
                    ") - ",
                    xpath_number(nchar(value) - 1),
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
                    "contains(",
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
        # function, which is defined in terms of xml:lang; the HTML
        # translation reads the language from the attributes directly
        # (see lang_attr_value())
        initialize = function(xhtml = FALSE) {
            self$xhtml <- xhtml
            if (!xhtml) {
                self$lower_case_element_names <- TRUE
                self$lower_case_attribute_names <- TRUE
            }
        },
        # The form-state pseudo-classes cover the element set the HTML
        # standard gives them: ':checked' the checkbox and radio inputs
        # and the selected options, ':enabled'/':disabled' the elements
        # that can be "actually disabled" (button, input, select,
        # textarea, optgroup, option, fieldset). The obsolete <command>
        # and <keygen> elements, and the hyperlinks that older drafts
        # made ':enabled', are deliberately not matched - no browser
        # matches 'a:enabled' either; ':link'/':any-link' select links
        xpath_checked_pseudo = function(xpath) {
            add_disjunction(xpath, list(
                list(element = "option", condition = "@selected"),
                list(element = "input",
                     condition = paste0("@checked and (", fold_type,
                                        " = 'checkbox' or ", fold_type,
                                        " = 'radio')"))))
        },
        # ':required' and ':optional' partition the form elements that
        # can take the required attribute (input, select, textarea);
        # an element outside that set (e.g. a button) is neither. As
        # in xpath_disabled_pseudo, a hidden input is excluded - the
        # required attribute does not apply to it - but the rarer
        # non-required input types (range, color, the button types)
        # are not carved out. required_optional_disjuncts() is shared
        # between the two pseudo-classes: only the leading condition
        # differs
        xpath_required_pseudo = function(xpath) {
            add_disjunction(xpath, required_optional_disjuncts("@required"))
        },
        xpath_optional_pseudo = function(xpath) {
            add_disjunction(xpath,
                            required_optional_disjuncts("not(@required)"))
        },
        xpath_read_write_pseudo = function(xpath) {
            rw <- read_write_condition(xpath)
            xpath$add_condition(rw$condition, is_or_group = rw$is_or)
            xpath
        },
        # ':read-only' matches exactly the elements ':read-write' does
        # not - Selectors 4 defines it as that negation, with no element
        # set of its own
        xpath_read_only_pseudo = function(xpath) {
            xpath$add_condition(paste0(
                "not(", read_write_condition(xpath)$condition, ")"))
            xpath
        },
        # ':placeholder-shown' matches an <input> or <textarea> with a
        # placeholder and an empty current value; a <textarea>'s value
        # is its text content rather than an attribute, hence string()
        # (the context node's string-value) instead of string(@value)
        xpath_placeholder_shown_pseudo = function(xpath) {
            add_disjunction(xpath, list(
                list(element = "input",
                     condition = "@placeholder and not(string(@value))"),
                list(element = "textarea",
                     condition = "@placeholder and not(string())")))
        },
        # ':default' matches a selected <option>, a checked checkbox or
        # radio <input>, and the default submit button of a form (the
        # first submit button, in document order, among the descendants
        # of its nearest enclosing <form>). The last part is approximate
        # - it does not follow a 'form' attribute pointing at a form
        # elsewhere in the document - but is otherwise exact. Testing
        # whether the candidate *is* that first control needs a node
        # identity test; XPath 1.0 has no current() (that is XSLT-only,
        # see of_type_nodetest()) and libxml2's XPath evaluation does not
        # expose generate-id() to either the XML or xml2 binding, so
        # identity is tested the classic XPath 1.0 way instead: two
        # single-node node-sets denote the same node exactly when their
        # union still has one member (a distinct pair unions to two).
        # The leading form_ancestor test guards the degenerate case of
        # a submit control with no enclosing form, where the right-hand
        # node-set is empty and the union would otherwise vacuously
        # equal ".", by requiring a form ancestor to exist before the
        # count comparison is trusted
        xpath_default_pseudo = function(xpath) {
            first_submit <- paste0(
                form_ancestor, " and count(. | ", form_ancestor,
                "[1]/descendant::*[", submit_control_condition, "][1]) = 1")
            add_disjunction(xpath, list(
                list(element = "option", condition = "@selected"),
                list(element = "input",
                     condition = paste0(
                         "(@checked and (", fold_type, " = 'checkbox' or ",
                         fold_type, " = 'radio')) or ((", fold_type,
                         " = 'submit' or ", fold_type, " = 'image') and ",
                         first_submit, ")"),
                     is_or = TRUE),
                list(element = "button",
                     condition = paste0("(not(@type) or ", fold_type,
                                        " = 'submit') and ", first_submit))))
        },
        xpath_lang_function = function(xpath, fn) {
            validate_lang_args(fn)
            lang_values <- extract_lang_values(fn)

            # Build conditions for each language range
            conditions <- vapply(lang_values, function(value) {
                # Wildcard * matches any element whose language is
                # known. Only the nearest language-attributed
                # ancestor-or-self counts, and an empty value there
                # resets the language to unknown
                known <- paste0(lang_attr_ancestor(self$xhtml),
                                "[string-length(", lang_attr_value(self$xhtml),
                                ") > 0]")
                kind <- lang_range_kind(value)
                if (kind == "any") {
                    known
                } else if (value == "") {
                    # Selectors 4 defines :lang("") as matching elements
                    # whose content language is *not* tagged at all - the
                    # negation of "any" above (no language-attributed
                    # ancestor-or-self has a non-empty value), not a
                    # literal lang="" comparison
                    paste0("not(", known, ")")
                } else if (kind == "extended" || lang_range_multi_subtag(value)) {
                    # A wildcard in non-trailing position (e.g. "*-CH" or
                    # "de-*-DE"), or more than one subtag named without a
                    # wildcard (e.g. "de-DE"): RFC 4647 extended filtering,
                    # approximated from the nearest language-attributed
                    # ancestor. A single-subtag range needs no walk - the
                    # plain prefix test below is equivalent and cheaper
                    lang_extended_html_condition(value, self$xhtml)
                } else {
                    # An exact tag ("en", "en-GB") or a trailing-wildcard
                    # prefix range ("en-*"), both of which match the
                    # language and any of its subtags: dash-terminate
                    # both sides and test for a prefix
                    prefix <- tolower(sub("\\*$", "", value))
                    # Don't add '-' if the range already ends with it
                    if (!grepl("-$", prefix))
                        prefix <- paste0(prefix, "-")
                    paste0(
                        lang_attr_ancestor(self$xhtml),
                        "[starts-with(concat(", lang_attr_value_lc(self$xhtml),
                        ", '-'), ", xpath_literal(prefix), ")]")
                }
            }, character(1), USE.NAMES = FALSE)

            # Combine conditions with OR; more than one alternative
            # forms an or-group, which add_condition() parenthesizes
            # if it is ever joined with another condition
            if (length(conditions) > 0) {
                xpath$add_condition(paste(conditions, collapse = " or "),
                                    is_or_group = length(conditions) > 1)
            }

            xpath
        },
        # ':link' matches the HTML Standard's hyperlink elements that
        # have an href: <a> and <area> only. A <link> in the <head> is
        # not a hyperlink in that sense - it is metadata, and is never
        # rendered - so it is deliberately outside the set
        xpath_link_pseudo = function(xpath) {
            add_disjunction(xpath, list(
                list(element = "a", condition = "@href"),
                list(element = "area", condition = "@href")))
        },
        xpath_any_link_pseudo = function(xpath) {
            # ':any-link' is ':link or :visited' (selectors-4 section
            # 9.1), and a static document has no visited state, so
            # every link is unvisited and ':any-link' collapses to
            # ':link'. Sharing the :link condition keeps the subset
            # relation between the two by construction
            self$xpath_link_pseudo(xpath)
        },
        xpath_disabled_pseudo = function(xpath) {
            # An element that can be disabled by an ancestor <fieldset>
            # (input, button, select, textarea, and a <fieldset> itself)
            # is disabled by @disabled or by disabled_by_fieldset; an
            # <optgroup> or <option> by the rules below. No input type
            # is carved out: the disabled attribute applies to every
            # <input>, hidden ones included
            fieldset_disjunct <- paste0("@disabled or (", disabled_by_fieldset,
                                        ")")
            add_disjunction(xpath, list(
                list(element = "input", condition = fieldset_disjunct,
                     is_or = TRUE),
                list(element = "button", condition = fieldset_disjunct,
                     is_or = TRUE),
                list(element = "select", condition = fieldset_disjunct,
                     is_or = TRUE),
                list(element = "textarea", condition = fieldset_disjunct,
                     is_or = TRUE),
                list(element = "fieldset", condition = fieldset_disjunct,
                     is_or = TRUE),
                # an <optgroup> or an <option> is "actually disabled"
                # when its nearest ancestor <select> is disabled, and an
                # <option> also when it is under a disabled <optgroup>,
                # each without any @disabled of its own
                list(element = "optgroup",
                     condition = paste0("@disabled or ",
                                        nearest_select_disabled),
                     is_or = TRUE),
                list(element = "option",
                     condition = paste0(option_own_disabled, " or ",
                                        nearest_select_disabled),
                     is_or = TRUE)))
        },
        xpath_enabled_pseudo = function(xpath) {
            not_fieldset_disabled <- paste0("not(@disabled or (",
                                            disabled_by_fieldset, "))")
            add_disjunction(xpath, list(
                list(element = "fieldset",
                     condition = not_fieldset_disabled),
                list(element = "optgroup",
                     condition = paste0("not(@disabled or ",
                                        nearest_select_disabled, ")")),
                list(element = "input", condition = not_fieldset_disabled),
                list(element = "button", condition = not_fieldset_disabled),
                list(element = "select", condition = not_fieldset_disabled),
                list(element = "textarea",
                     condition = not_fieldset_disabled),
                list(element = "option",
                     condition = paste0("not(", option_own_disabled, " or ",
                                        nearest_select_disabled, ")"))
            ))
        }
    )
)
