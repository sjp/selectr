\name{NEWS}
\title{NEWS file for the selectr package}
\encoding{UTF-8}

\section{Changes in version 0.7-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item The \code{querySelector()} family of functions now accepts
      a set of nodes, so queries can be chained: the \pkg{xml2}
      \code{xml_nodeset} or \pkg{XML} \code{XMLNodeSet} returned by one
      call can be passed to the next, where previously it raised "The
      object given to querySelectorAll() is not an 'XML' or 'xml2'
      document or node." The selector is evaluated from each node of
      the set in turn (so a relative selector such as
      \code{":scope > a"} applies per node), and a node matching from
      more than one of them is returned once, at the position it first
      matched.

      \item The \code{querySelector()} family of functions now uses the
      \code{html} translator by default when the document being queried
      was parsed as HTML, by \code{XML::htmlParse()} or
      \code{xml2::read_html()}; the \code{generic} (XML) translator
      remains the default for everything else. Element and attribute
      names are therefore matched case-insensitively, and
      \code{:checked}, \code{:disabled}, \code{:link} and \code{:lang()}
      match as they do in a browser, without \code{translator = "html"}
      having to be passed on every call. An explicit \code{translator}
      argument still wins, and the choice carries through a chain of
      queries that begins at an HTML document.

      \item An \pkg{xml2} \code{xml_missing}, as returned by a failed
      \code{xml_find_first()}, is now accepted by the
      \code{querySelector()} family and yields no matches, rather than
      an error.

      \item A new \code{?selectors} help topic gives the full
      selector-support reference: every selector, combinator and
      pseudo-class that selectr understands, which of the three
      translators supports each of them, and where the implementation
      diverges from CSS Selectors Level 4.

      \item The \code{html} and \code{xhtml} translators now support
      four more Selectors 4 pseudo-classes that are decidable from
      document attributes alone: \code{:read-write}, \code{:read-only},
      \code{:placeholder-shown} and \code{:default}. Previously these
      raised "The pseudo-class :read-write is unknown" (and similarly
      for the others); on the \code{generic} translator they translate
      to a never-matching expression, as \code{:checked} already did.
      Pseudo-classes that depend on live document state or user
      interaction, such as \code{:indeterminate}, remain unknown.

    }
  }
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Parse errors now include a source-pointer block showing the
      offending CSS selector with a caret (\code{^}) beneath the
      character that triggered the error, aligned correctly even when
      the selector contains tabs or wide (double-width) characters.

      \item Parse, translation and argument errors are now signalled as
      structured conditions inheriting a common \code{selectr_error}
      class, so callers can catch the family without matching on
      message text, or narrow to a specific shape:
      \code{selectr_parse_error} for malformed CSS (fields
      \code{$selector} and \code{$pos}), \code{selectr_translation_error}
      for CSS that is valid but untranslatable (fields \code{$selector},
      \code{$feature}, and \code{$pos}/\code{$column} for a construct
      that can be located), and \code{selectr_argument_error} for a bad
      R-level argument. Previously only parse errors carried a distinct
      class; translation and argument failures were plain
      \code{simpleError}s. Message text is unchanged.

      \item A reported position - the \code{$pos} field and the caret
      of the source-pointer block - now points at the token the message
      names, rather than at wherever the parser happened to stop. The
      caret under \code{"a||b"} sits on the first \code{|} of the
      \code{||} the message names (previously the second), under
      \code{"a:before:empty"} on the \code{:before} it calls
      misplaced (previously on the \code{:empty} that follows it),
      under \code{"a::part(b)"} on the \code{part} it names
      (previously on the \code{(}), and under
      \code{"e:nth-child(2x)"} on the \code{x} that takes the
      argument out of the \code{An+B} grammar (previously on the start
      of the argument).

      \item The minimum required version of R has been increased from
      \code{3.3} to \code{3.6}, required by \code{errorCondition()},
      which the new error hierarchy uses.

      \item \code{css_to_xpath()} now signals an error when
      \code{selector}, \code{prefix} or \code{translator} have lengths
      that are not all 1 or the common (maximum) length, instead of
      recycling as base R does and silently mispairing them.

      \item Generated XPath expressions are shorter: the \code{html}
      and \code{xhtml} pseudo-class translations drop disjuncts that
      cannot apply once the compound selector's element name is known
      (\code{input:checked} loses the \code{option} branch, and
      \code{div:checked} collapses to \code{div[0]}), and the attribute
      operators no longer emit a redundant existence guard beside a
      test that already implies it. The expressions are semantically
      unchanged, but code comparing \code{css_to_xpath()} output
      against stored strings will need updating.

      \item Translating long selectors is measurably faster: the
      tokenizer matches against a bounded window of the remaining input
      instead of re-slicing it for every token, and translator
      instances are cached and reused. The test suite has also been
      migrated to testthat edition 3.

      \item \code{querySelector()} no longer finds every match in order
      to return the first one: on an \pkg{XML} document, node or
      \code{XMLNodeSet} it asks XPath for the first node directly, and
      on an \code{xml_nodeset} it stops at the first node of the set
      that matches. The node returned is unchanged --- still the first
      in document order, a grouped selector such as \code{"a, b"}
      included --- but a query with many matches is several times
      faster.

      \item Arithmetic in the generated XPath now puts a space on both
      sides of the operator. An \code{An+B} argument with a non-zero
      remainder is written
      \code{(count(preceding-sibling::*) + 2) mod 3 = 0} where
      previously it wrote \code{(count(preceding-sibling::*) +2)}, and
      the \code{[attr$=value]} suffix match is written
      \code{substring(@href, string-length(@href) - 2)} where
      previously it wrote \code{string-length(@href)-2}. Both
      expressions are unchanged as XPath; only the string produced by
      \code{css_to_xpath()} differs, which matters if it is compared,
      cached or stored.

      \item A compound selector containing a simple selector that can
      never match is now translated to the predicate \code{[0]} alone,
      instead of that \code{0} being AND-ed with the rest of the
      compound's conditions: \code{"e.warning:is()"} gives
      \code{"e[0]"} where previously it gave
      \code{"e[contains(concat(' ', normalize-space(@class), ' '), ' warning ') and 0]"},
      and \code{"li:nth-child(-n of .item)"} gives \code{"li[0]"},
      dropping the now-redundant \code{.item} test. The node set
      selected is unchanged --- both forms match nothing --- so only
      code inspecting the output of \code{css_to_xpath()} is affected.

      \item Generated XPath expressions are shorter again, and every
      change below leaves the node set selected exactly as it was. The
      \code{html} and \code{xhtml} pseudo-class translations now prune
      their per-element disjuncts against a prefixed or
      \code{*|}-qualified compound as well, since those pin the same
      local name the disjuncts test (\code{"h|input:required"} and
      \code{"*|input:required"} lose the \code{select} and
      \code{textarea} branches, as \code{"input:required"} already
      did). The enumerated attribute-value sets behind
      \code{:required}, \code{:optional}, \code{:read-write},
      \code{:read-only}, \code{:placeholder-shown} and
      \code{contenteditable} are written as one \code{contains()} over
      a delimited set rather than one folded comparison per member.
      \code{:enabled} and \code{:disabled} name their seven elements
      once and guard each disabling rule by the elements it applies to,
      instead of repeating every rule --- the \code{fieldset} walk
      included --- once per element. A selector list whose every
      alternative is impossible folds to a single \code{0}
      (\code{":is(:hover, :visited)"} gives \code{"*[0]"} rather than
      \code{"*[0 or 0]"}), and a condition a compound repeats exactly
      is now kept once (\code{"div.scene.scene"} tests the class once).
      As before, code comparing \code{css_to_xpath()} output against
      stored strings will need updating.

      \item Generated XPath expressions are shorter and cheaper to
      evaluate once more, again with no change to the node set
      selected. The per-element disjuncts of the \code{html} and
      \code{xhtml} form pseudo-classes now name the element before
      asking anything else of it, so that XPath's left-to-right
      evaluation of the conjunction rejects a candidate on the
      one-word \code{local-name()} test rather than on a
      \code{translate()} call spelling out both alphabets:
      \code{":checked"} gives
      \code{"(@selected and local-name() = 'option') or (@checked and local-name() = 'input' and (...))"}.
      A condition every disjunct shares is asked once in front of them
      all, so \code{":required"} and \code{":optional"} ask for
      \code{@required} once instead of once per element. The
      \code{contenteditable} guard behind \code{:read-write} and
      \code{:read-only} tests the attribute itself for a \code{|}
      rather than its case-folded copy, which cannot differ. The
      empty-value test in \code{:placeholder-shown} is written
      \code{not(string-length(@value))} rather than
      \code{not(string(@value))}. A selector list mixing impossible
      alternatives with live ones keeps one \code{0} instead of one
      per dead alternative (\code{":is(:hover, :visited, p)"} gives
      \code{"*[0 or self::p]"}). And a condition a compound repeats
      exactly, already kept once, no longer gains parentheses around
      the copy that was kept (\code{":checked:checked"} now reads
      exactly as \code{":checked"} does).

      \item A branch of a selector list that repeats an earlier branch
      exactly now renders once, as a condition a compound repeats
      already did, and again with no change to the node set selected.
      This covers the argument of \code{:is()}, \code{:where()} and
      \code{:not()} and the \code{of S} list of \code{:nth-child()}
      and \code{:nth-last-child()}: \code{":is(a, a)"} gives
      \code{"*[self::a]"} rather than \code{"*[self::a or self::a]"},
      and \code{"a:not(b, b)"} gives \code{"a[not(self::b)]"}. A list
      left with a single branch is no longer a disjunction, so it also
      sheds the parentheses it would have carried into a conjunction:
      \code{":nth-child(2 of a, a)"} ends \code{"and self::a"} rather
      than \code{"and (self::a or self::a)"}.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item The \code{html} translator now matches the values of the
      attributes HTML defines as ASCII case-insensitive
      (\code{type}, \code{rel}, \code{lang}, \code{dir},
      \code{media}, \code{method}, \code{target}, \code{checked},
      \code{disabled}, \code{selected}, ... --- the list in HTML's
      "Case-sensitivity of selectors") without regard to case, as a
      browser does: \code{"input[type=radio]"} now selects
      \code{<input type="RADIO">}, and \code{"[lang|=en]"} an element
      with \code{lang="En-us"}. A trailing \code{s} flag asks for the
      exact comparison back. \code{class}, \code{id}, \code{href},
      \code{name}, any \code{data-*} and every namespaced attribute
      are unaffected, as are the \code{generic} and \code{xhtml}
      translators, which do not serve HTML documents.

      \item The \code{html} translator now lowercases element and
      attribute names by folding \code{A}-\code{Z} only, as an HTML
      parser does. Names were folded with \code{tolower()}, which also
      maps non-ASCII letters --- and does so according to the locale ---
      so a selector such as \code{"\\C4"} (LATIN CAPITAL LETTER A WITH
      DIAERESIS) was translated into a name test no document could
      match.

      \item \code{:placeholder-shown} now requires the
      \code{placeholder} attribute to be non-empty, and matches an
      \code{<input>} only when its \code{type} is one the attribute
      applies to (so not \code{hidden}, \code{checkbox},
      \code{radio}, \code{file}, \code{submit}, \code{image},
      \code{reset}, \code{button}, \code{color}, \code{range},
      \code{date}, \code{month}, \code{week}, \code{time} or
      \code{datetime-local}). An empty \code{placeholder} displays
      nothing, and a date picker or a checkbox never shows one, yet
      both previously matched.

      \item An \code{<input>} whose \code{type} is \code{range},
      \code{color}, \code{submit}, \code{image}, \code{reset} or
      \code{button} is now neither \code{:required} nor
      \code{:optional} under the \code{html} and \code{xhtml}
      translators, joining \code{type="hidden"}. The \code{required}
      attribute does not apply to any of these states, so such an input
      was previously reported as \code{:required} when it carried a
      meaningless \code{required} attribute, and as \code{:optional}
      when it did not.

      \item A large \code{An+B} value in \code{:nth-child()} and the
      other \code{:nth-*()} pseudo-classes is now written into the
      generated XPath in full. Values of five or more digits were
      formatted with R's default number printing, so
      \code{css_to_xpath(":nth-child(100001)")} produced
      \code{"... = 1e+05"}, which XPath 1.0 has no syntax for. libxml2
      accepts an exponent, so \pkg{xml2} and \pkg{XML} selected the
      right nodes regardless, but the expression was not portable to
      other XPath engines.

      \item An \code{A} or \code{B} too large for an R integer (e.g.
      \code{":nth-child(99999999999)"}) is now saturated to
      \code{.Machine$integer.max} instead of raising "Invalid
      nth-child expression: could not parse as valid integers". The
      \code{An+B} grammar puts no bound on either value, and no
      document has that many siblings, so the clamped series selects
      exactly what the written one would.

      \item The \code{html}-qualified pseudo-classes (\code{:checked},
      \code{:disabled}, \code{:enabled}, \code{:link}/\code{:any-link},
      \code{:optional} and \code{:required}) now identify elements by
      local name, matching the \code{xhtml} translator's rule for type
      selectors. Previously they were built from unprefixed name tests
      and axis steps, so under the \code{xhtml} translator they
      silently matched nothing, or the wrong elements, on a namespaced
      document even when the selector's subject was written correctly.
      The \code{html} translator is unaffected in practice, since
      libxml2's HTML parser produces no namespaces.

      \item \code{querySelectorNS()} and \code{querySelectorAllNS()}
      now search only within the node they are given, as their
      un-namespaced counterparts do. The namespace filter was built as
      an absolute location path (\code{//svg:*}), which is evaluated
      from the document root whatever the context node; it is now
      relative (\code{descendant-or-self::svg:*}). Queries starting
      from a document are unaffected.

      \item A selector with a long chain of combinators (e.g.
      \code{"a > a > ... > a"}) is now translated, and its specificity
      and representation computed, by walking the parse tree in a loop
      rather than recursively; beyond a few hundred combinators these
      previously failed with "evaluation nested too deeply". The
      arguments of functional pseudo-classes are still translated
      recursively, but a selector that nests those too deeply (e.g.
      hundreds of \code{:not()}) now raises a
      \code{selectr_translation_error} saying so, instead of letting
      the base R error escape untranslated.

      \item An unusually long selector is now translated without being
      cached, rather than failing with "variable names are limited to
      10000 bytes": \code{css_to_xpath()} caches translations in an
      environment, whose keys are subject to R's limit on symbol
      length.

      \item A backslash immediately followed by the end of a selector's
      source text is now decoded per css-syntax, instead of raising
      "Unexpected character" (or, inside a string, "Unclosed string"):
      within an identifier or a hash name it becomes the replacement
      character U+FFFD, and inside a still-open string it is dropped,
      with the string then auto-closed as usual.

      \item A unicode escape decoding to a null, surrogate, or
      out-of-range code point (e.g. \code{"\\0"}, \code{"\\D800"},
      \code{"\\110000"}) now decodes to the replacement character
      U+FFFD, as css-syntax requires, instead of producing \code{NA} or
      an empty string.

      \item \code{An+B} expressions are now validated while parsing the
      selector, so a malformed value is reported with a source position
      and names the pseudo-class it occurred in, rather than surfacing
      later as a generic translation failure.

      \item \code{An+B} expressions are now read from the selector's
      tokens rather than from their decoded text, so a digit written as
      an escape is no longer taken for an integer: css-syntax gives an
      escape only to identifiers, making \code{":nth-child(\\32 )"} the
      name \code{"2"}, which matches no \code{An+B} production.
      Selectors such as \code{":nth-child(\\32 )"},
      \code{":nth-child(\\32 n)"} and \code{":nth-child(2n+\\31 )"}
      are therefore rejected, as browsers reject them, instead of
      translating as though the escapes had been written out. An escape
      spelling a letter is unaffected, being an identifier either way:
      \code{":nth-child(\\65 ven)"}, \code{":nth-child(2\\n)"} and
      \code{":nth-child(\\6e-1)"} still translate as \code{"even"},
      \code{"2n"} and \code{"n-1"} do.

      \item An ID selector's hash must now be identifier-shaped:
      \code{"#1"} and other hashes starting with a digit, or with
      \code{-} followed by a digit, are rejected with a hint showing
      how to escape the leading digit; a lone \code{"#-"} is rejected
      as well. Previously these were accepted and silently
      mistranslated.

      \item A lone \code{-} is now a delimiter rather than a name, so
      a selector that leans on it is rejected instead of translated
      into an expression no document can match: \code{"-"} and
      \code{"a - b"} (a combinator someone began and did not finish)
      no longer name an element \code{-}, and \code{".-"} is no
      longer a class selector. A \code{-} that does start a name is
      unaffected, escaped ones included, so \code{"-a"},
      \code{"--a"}, \code{"--"} and \code{"\\-"} translate as
      before, as does the sign of \code{":nth-child(2n - 1)"}.

      \item \code{-->} and \code{<!--} are now read as the single
      CDC and CDO tokens that CSS makes them, and rejected whole at the
      position they start. \code{"-->a"} previously parsed as an
      element named \code{--} followed by a child combinator, and
      \code{"<!--a"} failed on its first character. A \code{-->}
      inside a name is unchanged, so \code{"a-->b"} still selects a
      \code{b} child of an \code{a--}.

      \item An empty \code{:is()} or \code{:where()} argument list
      (e.g. \code{":is()"}) is now accepted, per its
      \code{<forgiving-selector-list>} grammar, and matches nothing,
      instead of raising a parse error.

      \item \code{:lang()} language ranges must now be separated by a
      comma; whitespace alone (e.g. \code{":lang(en fr)"}) is no longer
      accepted as a separator. A wildcard range spanning several tokens
      with no whitespace between them (e.g. \code{":lang(*-CH)"}) is
      unaffected.

      \item \code{:lang(*)} now matches only elements whose language is
      known (i.e. that inherit a non-empty \code{lang} or
      \code{xml:lang} from the nearest ancestor-or-self that declares
      one), instead of matching every element.

      \item The \code{xhtml} translator's \code{:lang()} now also reads
      the \code{xml:lang} attribute, preferring it over \code{lang}
      when both are present on the nearest declaring ancestor-or-self,
      matching HTML's language-determination rules. Previously only
      \code{lang} was consulted.

      \item An \code{<option>} inside a disabled \code{<optgroup>} is
      now \code{:disabled} under the \code{html} and \code{xhtml}
      translators, even without a \code{disabled} attribute of its own,
      matching how a browser determines an option's disabled state. An
      \code{<optgroup>} or \code{<option>} whose nearest ancestor
      \code{<select>} is disabled is likewise \code{:disabled} rather
      than \code{:enabled}, following the current definition of "actually
      disabled" in the HTML standard. Both walks stop at the elements
      the standard stops at (\code{<select>}, \code{<datalist>},
      \code{<hr>} and \code{<option>}), so an \code{<option>} in a
      \code{<datalist>} nested in a disabled \code{<select>} stays
      \code{:enabled}.

      \item An \code{<input type="hidden">} is now \code{:disabled}
      when it carries a \code{disabled} attribute (or sits inside a
      disabled \code{<fieldset>}) and \code{:enabled} otherwise, under
      the \code{html} and \code{xhtml} translators. Previously it was
      neither, a carve-out the HTML standard does not make: the
      \code{disabled} attribute applies to every \code{<input>}. The
      carve-out in \code{:required}/\code{:optional}, where
      \code{required} genuinely does not apply to a hidden input, is
      unchanged.

      \item A \code{<fieldset>} inside a disabled \code{<fieldset>} is
      now \code{:disabled} under the \code{html} and \code{xhtml}
      translators, even without a \code{disabled} attribute of its own,
      following the same rule already applied to the form controls: a
      disabled \code{<fieldset>} disables its descendants except those
      inside its first \code{<legend>} child. Previously a
      \code{<fieldset>} was \code{:disabled} only by its own attribute.

      \item \code{:link} and \code{:any-link} no longer match a
      \code{<link>} element with an \code{href} under the \code{html}
      and \code{xhtml} translators. The HTML standard's hyperlink
      elements for these pseudo-classes are \code{<a>} and
      \code{<area>} only; a \code{<link>} in the document head is
      metadata rather than a hyperlink, so \code{":link"} no longer
      selects it and \code{"link:link"} now matches nothing.

      \item The of-type pseudo-classes (\code{:first-of-type},
      \code{:last-of-type}, \code{:only-of-type},
      \code{:nth-of-type()}, \code{:nth-last-of-type()}) now reject a
      namespaced wildcard subject (e.g. \code{"svg|*:first-of-type"})
      with a translation error, instead of silently counting siblings
      by namespace rather than by expanded name.

      \item An unprefixed element name that cannot be written as an
      XPath name test, and so is compared against \code{name()}, is now
      pinned to the null namespace (e.g. \code{"é"} translates to
      \code{*[name() = 'é' and namespace-uri() = '']}), giving it the
      same meaning as the name test a plain \code{"p"} becomes.
      Previously such a name also matched an element in a default
      namespace, which \code{"p"} does not. The no-namespace form
      \code{"|é"} already had the pin.

      \item An element or attribute name whose \emph{local part} cannot
      be written as an XPath name test now keeps its namespace prefix
      in the node test and compares the local part alone
      (e.g. \code{"svg|di\\[v"} translates to
      \code{svg:*[local-name() = 'di[v']}), so the prefix resolves
      through the namespace map supplied at evaluation time, as every
      other prefixed selector does. Previously the whole qualified name
      was folded into a \code{name()} comparison, which instead matched
      on the prefix the \emph{document} happens to use.

      \item A namespace prefix that is not a valid XPath
      \code{NCName} (e.g. \code{"\\31 ns|div"}, \code{"[\\31 ns|href]"})
      is now rejected with a translation error. Such a prefix cannot be
      named by an XPath expression at all, and the \code{name()}
      comparison previously emitted for it was not an equivalent: it
      tested the document's spelling of the prefix rather than the URI
      the caller bound it to. A prefix that is a name outside ASCII
      (e.g. \code{"äöü|a"}) is now translated rather than folded into
      such a comparison.

      \item \code{NA} and empty-string values in the \code{ns} argument
      of the \code{querySelectorNS()} family are now rejected with an
      error, instead of being used as a namespace URI.

      \item A \emph{name} in the \code{ns} argument of the
      \code{querySelectorNS()} family that is not a valid XML
      \code{NCName} (e.g. \code{c("s p" = "urn:s")}, or one containing
      a colon or starting with a digit) is now rejected with an error.
      Previously such a prefix was interpolated into the generated
      XPath, which then failed to compile: the query raised a libxml2
      warning and returned no matches. The condition is the one applied
      to a prefix written in a selector, so the two agree: a name
      outside ASCII (e.g. \code{c("äöü" = "urn:s")}) is a usable
      binding wherever \code{"äöü|a"} translates.

      \item A zero-length \code{ns} argument (\code{character(0)} or
      \code{list()}) to the \pkg{xml2} \code{querySelector()} family
      now means "no namespaces" and skips the \code{xml_ns()} walk of
      the whole document, rather than being treated the same as
      \code{NULL} (which uses the document's own namespace map).

      \item A \code{selector} or \code{prefix} argument to
      \code{css_to_xpath()} holding bytes that are invalid in their
      declared encoding is now rejected with a
      \code{selectr_argument_error}. Previously the bytes reached the
      tokenizer and surfaced as base R's "invalid multibyte string"
      error, which escaped the guarantee that every error selectr
      signals inherits \code{selectr_error}.

      \item An invalid \code{translator} argument to
      \code{css_to_xpath()} (e.g. \code{translator = "xml"}) now raises
      a message naming the \code{translator} argument and the value
      that was passed, instead of \code{match.arg()}'s "'arg' should be
      one of ...", which named the wrong (internal) argument.
      Unambiguous abbreviations (e.g. \code{"x"} for \code{"xhtml"})
      are still accepted.

      \item \code{p:nth-of-type(n of .a)} and other of-type
      pseudo-classes now report the same "'of' is only allowed in
      :nth-child() and :nth-last-child()" error that
      \code{:nth-child()}/\code{:nth-last-child()} already gave for a
      misplaced \code{of}, instead of a confusing "Expected an
      argument, got <DELIM '.' ...>" pointing at the selector list that
      follows it.

      \item \code{:nth-child(0)} (and other An+B expressions with
      \code{a == 0} and \code{b < 1}) now translates to the same tidy
      \code{[0]} early-exit predicate as \code{:nth-child(-n)}, instead
      of the equivalent but more roundabout
      \code{[count(preceding-sibling::*) = -1]}.

      \item A handful of parser and argument-validation messages have
      been reworded for clarity. Parse errors that embedded a source
      position in their text (e.g. "Unclosed string at 4") no longer
      duplicate the position shown by the caret gutter; where that
      gutter is suppressed (a selector spanning more than one line),
      the position is still given, as "at position N". Four messages
      that pointed at the wrong problem now name the actual one: an
      R-level \pkg{XML} tree, as returned by \code{XML::xmlTreeParse()}
      or \code{XML::htmlTreeParse()}, is reported as one, naming the
      parsers that give a document XPath can search; a functional
      pseudo-element such as \code{::slotted()} or \code{::part()} is
      reported as unsupported rather than as "not at the end of a
      selector"; a class name starting with a digit (\code{div.5}) gets
      the same escape-it-like-this hint that \code{#5} already did; and
      an unquoted numeric attribute value (\code{[data-id=1]}) is told
      to quote the value.

      \item The \code{html} and \code{xhtml} translators' \code{:lang()}
      now implements Selectors 4 / RFC 4647 extended filtering for a
      range naming more than one subtag, so \code{:lang(de-DE)} matches
      \code{lang="de-Latn-DE"} (any subtag may be skipped between the
      ones named), where it previously only matched a literal
      \code{de-DE} prefix. A single-subtag range is unaffected. The
      \code{generic} translator still does Selectors 3 \code{|=}-style
      prefix matching for multi-subtag ranges, since XPath 1.0's
      \code{lang()} cannot express the skipped-subtag case; the two
      differ in this one respect. Separately, \code{:lang("")} now
      matches per spec --- an element whose content language is not
      tagged at all --- rather than only a literal \code{lang=""}.

      \item A \code{:lang()} language range that is not an RFC 4647
      \emph{extended language range} --- one with an empty, over-long
      or non-alphanumeric subtag, or a \code{*} that is not a whole
      subtag, e.g. \code{":lang(en-)"}, \code{":lang(--x)"},
      \code{":lang(en*)"} or \code{":lang(de-*--de)"} --- is now
      rejected, naming the offending range. Previously such a range was
      normalised into a well-formed one and matched elements it should
      not have: \code{":lang(en-)"} selected every English element and
      \code{":lang(en*)"} every tagged one, while \code{":lang(--)"}
      produced an XPath that would not parse.

      \item An empty item in the comma-separated \code{:lang()}
      argument list (e.g. \code{":lang(en, )"} or \code{":lang(, en)"})
      is now rejected rather than silently dropped: the argument is an
      \code{<ident>#} list, in which an empty item is a grammar error.

      \item A \code{:lang()} range of nothing but wildcards
      (\code{":lang(\"*-*\")"}) now matches any element with a known
      language, as \code{":lang(*)"} does, rather than producing an
      empty XPath predicate.

      \item An escaped \code{*} before a \code{|} is now rejected as
      the namespace prefix it is, rather than read as the
      any-namespace wildcard. In an \code{<ns-prefix>} only the
      delimiter \code{*} means "any namespace"; an identifier that
      merely decodes to the same character names a prefix \code{*},
      which no \code{@namespace} rule can bind because it is not an
      XML \code{NCName}. So \code{"\\2a|a"}, \code{"\\*|a"} and
      \code{"[\\2a|href]"} now raise "The namespace prefix '*' is not
      an XPath name, so it cannot be translated", joining
      \code{"\\31 ns|div"} and the other prefixes XPath cannot name,
      where they previously translated exactly as \code{"*|a"} and
      \code{"[*|href]"} did and so matched every namespace.
      \code{"*|a"} and \code{"[*|href]"} themselves are unchanged.

      \item \code{"*|\\2a"} now selects an element named \code{*} in
      any namespace, as \code{"ns|\\2a"} and \code{"\\2a"} already
      selected one in a given namespace and in none. The local name
      was compared against the character \code{*} rather than tested
      for having been the universal selector's delimiter, so the
      selector was translated to a bare \code{*} and matched every
      element.

    }
  }
}

\section{Changes in version 0.6-0}{

  This release is a fairly major change. It comes about via a port to
  a Rust implementation which uses a robust browser-quality selector
  parsing engine. With behavioural comparison tests, several bug fixes,
  features, and performance enhancements were identified and resolved.

  \subsection{NEW FEATURES}{
    \itemize{

      \item Added support for case-sensitivity flags in attribute
      selectors: \code{[attr="value" i]} matches the attribute value
      ASCII case-insensitively, while \code{[attr="value" s]}
      explicitly requests the default case-sensitive matching.

      \item The \code{:required} and \code{:optional} pseudo-classes
      are now supported. With the \code{html} or \code{xhtml}
      translator they match form elements (\code{input} other than
      \code{type="hidden"}, \code{select}, \code{textarea}) by the
      presence or absence of the \code{required} attribute; with the
      \code{generic} translator they translate to a never-matching
      expression, as \code{:checked} does.

      \item The \code{:focus-within} and \code{:focus-visible}
      pseudo-classes are now accepted and, like the rest of the
      user-action family (\code{:focus}, \code{:hover}, \code{:active},
      ...), translate to a never-matching expression, since a static
      document has no such dynamic state. They previously raised "The
      pseudo-class ... is unknown".

      \item Added support for a leading \code{:scope} pseudo-class,
      anchoring the selector at the queried node:
      \code{querySelectorAll(node, ":scope > a")} returns only the
      \code{a} children of \code{node}. \code{css_to_xpath()}
      translates such selectors with the XPath \code{self} axis in
      place of the \code{prefix} argument (\code{":scope > a"} becomes
      \code{"self::*/a"}). A \code{:scope} anywhere else in a selector
      cannot be expressed in XPath 1.0 and is rejected with a clear
      error.

      \item \code{:not()} may now appear inside the arguments of
      functional pseudo-classes (e.g. \code{:not(:not(a))},
      \code{:is(:not(.x))}, \code{:nth-child(2 of :not(.foo))}).

      \item \code{:has()} now supports leading combinators in its
      arguments (e.g. \code{e:has(> img)}, \code{e:has(~ p)},
      \code{e:has(+ p)}).

      \item Functional pseudo-class arguments may now be complex
      selectors (contain combinators): \code{:is(a b)},
      \code{:not(a > b)}, \code{e:has(> a b)}, and
      \code{li:nth-child(2 of ol li)} previously produced an
      "Expected an argument" error and are now translated.

    }
  }
  \subsection{MINOR CHANGES}{
    \itemize{

      \item The performance of selectr has been improved roughly 2x.
      There are several smaller changes that have contributed to this.

      \item The unsupported Selectors 4 column combinator now raises
      "The column combinator '||' is not supported" instead of a raw
      tokenizer error, and an unknown functional pseudo-class is
      reported with the user's hyphenated spelling
      (\code{:nth-col()}, previously \code{:nth_col()}).

      \item Generated XPath expressions now include parentheses only
      when precedence requires them (an or-expression joined with
      another condition): \code{e[id][title]} translates to
      \code{e[@id and @title]} rather than
      \code{e[(@id) and (@title)]}. The expressions are semantically
      unchanged, but code comparing \code{css_to_xpath()} output
      against stored strings will need updating.

      \item Attribute blocks, functional pseudo-classes, and strings
      left unclosed at the end of a selector are now auto-closed, as
      css-syntax requires: \code{[rel=stylesheet} and \code{:lang(fr}
      parse and translate exactly as their closed forms, as does a
      string missing its closing quote.

      \item The \pkg{stringr} and \pkg{methods} dependencies have been
      dropped. \pkg{R6} is now the only package \pkg{selectr}
      imports, shrinking the install footprint.

      \item \code{css_to_xpath()} now translates each distinct
      combination of selector, prefix, and translator only once per
      call, so duplicates in a vectorized call are not re-translated.

      \item The adjacent sibling combinator no longer emits a
      tautological \code{[self::*]} predicate when the right-hand side
      does not name an element: \code{h1 + *[rel=up]} now translates to
      \code{h1/following-sibling::*[1][@rel = 'up']}.

      \item The minimum required version of R has been increased from
      \code{3.0} to \code{3.3}, matching what the code (which uses
      \code{startsWith()}) has in fact required for some time.

    }
  }
  \subsection{REMOVED FEATURES}{
    \itemize{

      \item The non-standard extensions inherited from the Python
      'cssselect' package --- the \code{:contains("text")} pseudo-class
      and the \code{[attr!=value]} attribute operator --- have been
      removed and now produce an error. Standard alternatives:
      \code{:not([attr=value])} for the former operator, and an XPath
      \code{contains(., 'text')} predicate applied outside of
      \pkg{selectr} for \code{:contains()}.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item With the \code{html} or \code{xhtml} translator,
      \code{:any-link} now matches the same elements as \code{:link}.
      \code{:any-link} means ":link or :visited", a superset of
      \code{:link}, but it previously translated to a never-matching
      expression while \code{:link} matched - the subset relation
      inverted. (In a static document no link is visited, so the two
      pseudo-classes coincide.)

      \item Escape sequences in identifiers, hashes, and strings are
      now decoded in a single left-to-right pass, as css-syntax
      requires, so an escaped backslash followed by hex digits is no
      longer decoded twice: \code{e[foo="x\\\\79 z"]} now matches
      \code{x\\79 z} rather than \code{xyz}.

      \item A prefixed wildcard inside a pseudo-class argument
      (e.g. \code{:is(svg|*)}) now translates to the node test
      \code{self::svg:*} rather than the never-matching comparison
      \code{name() = 'svg:*'}.

      \item When \code{:lang()} is given an invalid argument after
      valid ones (e.g. \code{:lang(en, 5)}), the error now reports the
      offending argument rather than the first one.

      \item Strings containing a raw newline are now rejected with
      "Unclosed string", as the CSS grammar requires.

      \item The namespace argument of the \code{querySelector()} family
      of functions now signals an error when given a list containing an
      element that is not a single string; previously the prefix-to-URI
      pairing was silently corrupted.

      \item \code{:only-child} and \code{:only-of-type} now match the
      root element, consistent with \code{:first-child:last-child},
      which Selectors defines \code{:only-child} to be equivalent to.

      \item The HTML translator's \code{:enabled} and \code{:disabled}
      pseudo-classes now match \code{input} elements that have no
      \code{type} attribute (which default to \code{type=text}).

      \item \code{:dir()} now enforces its CSS Selectors Level 4
      argument grammar of exactly one identifier, rejecting the
      strings, wildcards, and lists (e.g. \code{:dir(ltr, rtl)}) it
      previously accepted via the shared \code{:lang()} grammar.

      \item Prefixed element names inside pseudo-class arguments
      (e.g. \code{:is(svg|g)}, \code{:nth-child(2 of svg|g)}) are now
      matched with an XPath name test (\code{self::svg:g}, or the path
      step \code{.//svg:g} for \code{:has()}) instead of a
      \code{name()} string comparison, so the prefix resolves through
      the namespace map supplied at evaluation time (URI-based), just
      as at the top level of a selector.

      \item The no-namespace form \code{|e} now retains its namespace
      constraint when the element name cannot be written as an XPath
      name test (e.g. a Unicode name such as \code{|é}); previously
      such names also matched in a default namespace.

      \item The of-type pseudo-classes now work on element names that
      cannot be written as an XPath name test
      (e.g. \code{é:only-of-type}, \code{*|e:first-of-type});
      previously these failed with the misleading error
      "*:only-of-type is not implemented".

      \item The \code{querySelector()} and \code{querySelectorAll()}
      methods for \pkg{xml2} documents now accept a named list as the
      \code{ns} argument, consistent with the methods for \pkg{XML}
      documents.

      \item The \code{querySelector()} family of functions now signals
      an error when the \code{selector} argument is not a single
      character string; previously all but the first selector were
      silently ignored when querying the document.

      \item \code{css_to_xpath()} now signals an error when any of its
      arguments contain \code{NA} values; previously \code{NA}s were
      removed before recycling, silently shifting the pairing of the
      remaining values.

      \item \code{An+B} expressions are now matched ASCII
      case-insensitively, as required by CSS Syntax, so
      \code{:nth-child(2N)}, \code{:nth-child(ODD)}, and
      \code{:nth-child(EVEN)} are no longer rejected.

      \item \code{An+B} expressions now only permit whitespace around
      the sign that separates the \code{B} value, so an invalid
      selector such as \code{:nth-child(3 7)} is rejected rather than
      silently treated as \code{:nth-child(37)}.

      \item \code{An+B} expressions now reject non-integer values
      (e.g. \code{:nth-child(1.9)}, \code{:nth-child(2e1)}) rather
      than silently truncating them.

      \item Computing the specificity of a \code{:has()} selector with
      a single argument (e.g. \code{e:has(img)}) no longer fails with
      "incorrect number of dimensions".

      \item Computing the specificity of a single-argument
      \code{:is()} or \code{:matches()} selector no longer fails, and
      the specificity of the compound the pseudo-class is attached to
      is no longer dropped: \code{div:is(.foo)} now reports (0, 1, 1)
      rather than (0, 1, 0).

      \item Nesting \code{:has()} inside \code{:has()}
      (e.g. \code{section:has(article:has(div))}) is now rejected, as
      required by CSS Selectors Level 4 and matching browsers; sibling
      uses such as \code{e:has(a):has(b)} remain valid.

      \item \code{:lang()} and \code{:dir()} no longer accept a lone
      \code{-}, which is not a valid CSS identifier.

      \item Attribute selectors with an empty value
      (e.g. \code{[attr=""]}) no longer throw an error for the
      \code{=} and \code{|=} operators.

      \item The HTML translator no longer lowercases attribute
      \emph{values}, only attribute names, so
      \code{[data-state="Active"]} no longer silently misses matches.

      \item The \emph{any namespace} selector \code{*|e} and the
      \emph{no namespace} selector \code{|e} no longer both collapse
      to the bare name \code{e}: \code{*|e} now translates to
      \code{*[local-name() = 'e']} so it matches \code{e} in any
      namespace, and similarly for \code{[*|attr]}.

      \item Unicode escapes are now supported in identifiers and ID
      selectors, not just strings, and are decoded to the characters
      they represent: \code{#\\31 23} (an ID starting with a digit) no
      longer fails to tokenize, and \code{"\\E9"} matches \code{é}
      rather than the literal value \code{E9}.

      \item The alternatives of \code{:is()}, \code{:matches()}, and
      \code{:where()} are now grouped as a single condition:
      \code{div.foo:is(.a, .b)} translates to \code{foo and (a or b)}
      rather than \code{foo or a or b}, and stacked uses such as
      \code{e:is(.a):is(.b)} now require both conditions rather than
      either.

      \item The universal selector \code{*} is no longer silently
      dropped when it appears alongside other arguments in a selector
      list: \code{:is(div, *)} now matches every element,
      \code{:not(div, *)} matches nothing, and
      \code{:nth-child(2 of div, *)} counts all siblings.

      \item Wildcard language ranges such as \code{:lang(en-*)} now
      match under the generic (XML) translator; previously they
      translated to \code{lang('en-')}, which can never match.

    }
  }
}

\section{Changes in version 0.5-1}{
  \subsection{BUG FIXES}{
    \itemize{

      \item Fixed handling of CSS unicode escapes in attribute values.
      This would be observed when the attribute value contained hexadecimal
      sequences like \code{(abcdef)} where the characters inside the parentheses
      were not properly escaped. This fix ensures that such sequences are correctly
      translated to their XPath equivalents. Thanks to André Veríssimo for
      reporting the issue.

    }
  }
}

\section{Changes in version 0.5-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Added support for CSS Selectors Level 4 pseudo-classes
      \code{:is()}, \code{:where()}, and \code{:has()}. The \code{:is()}
      pseudo-class matches elements against a list of selectors, taking the
      maximum specificity from its arguments. The \code{:where()} pseudo-class
      works similarly to \code{:is()} but always has zero specificity. The
      \code{:has()} pseudo-class represents an element if any of the relative
      selectors match when anchored against that element, with specificity
      calculated from the maximum of its arguments.

      \item Added support for complex selectors in \code{:nth-child()} and
      \code{:nth-last-child()} using the \code{of S} syntax
      (e.g., \code{:nth-child(2 of .foo)}). This allows matching the nth
      child that matches a specific selector or selector list.

      \item Extended \code{:not()} to accept multiple selectors separated by
      commas (e.g., \code{:not(.foo, #bar)}), following CSS Selectors Level 4.
      Specificity is now calculated as the maximum specificity among all
      arguments, rather than the sum.

      \item Added support for additional CSS Selectors Level 4 pseudo-classes:
      \code{:any-link}, \code{:target-within}, and \code{:local-link}. These
      pseudo-classes do not match any elements in static XML/HTML documents
      and translate to XPath expressions that always evaluate to false.
      For now, most of the new Level 4 pseudo-classes that depend on dynamic
      document state e.g. \code{:user-valid} and \code{:placeholder-shown} are
      not implemented, but may be at a future date to be non-matching selectors.

      \item The \code{:lang()} and \code{:dir()} pseudo-classes now support
      multiple comma-separated arguments (e.g., \code{:lang(en, fr, de)}).

      \item Added \code{:matches()} as a backwards-compatible alias for
      \code{:is()}.

    }
  }
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Improved sibling selector translation to use a more compact form.
      For the adjacent sibling combinator \code{a + b}, the generated XPath now
      uses \code{a/following-sibling::*[1][self::b]} instead of
      \code{a/following-sibling::*[(name() = 'b') and (position() = 1)]}.

      \item The descendant combinator \code{a b} now uses
      \code{a//b} instead of \code{a/descendant::b} for a more concise XPath.
      Unfortunately a similar optimisation cannot be applied to in general
      when attempting to replace \code{descendant-or-self::} with \code{.//a}
      as it would prevent root nodes being matched correctly.

      \item Improved validation of CSS selector arguments. Better error
      messages are now provided when pseudo-elements appear inside functional
      pseudo-classes where they are not permitted (e.g., inside \code{:is()},
      \code{:matches()}, \code{:where()}, or \code{:has()}).

      \item Enhanced input validation for \code{:lang()} and \code{:dir()}
      pseudo-classes to ensure proper argument formatting and to reject
      invalid or empty language tags.

      \item Improved handling of edge cases in selector parsing, including
      better validation of class selector syntax and more robust handling of
      null or missing element components.

      \item Simplified method registration for XML and xml2 objects. No longer
      necessary to hook into package load/unload events.

    }
  }
}

\section{Changes in version 0.4-2}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Improve handling of vectors of length > 1 in logical comparison.
      Contributed by Garrick Aden-Buie.

      \item Minor improvements to error message construction.
      Contributed by Michael Chirico.

    }
  }
}

\section{Changes in version 0.4-1}{
  \subsection{BUG FIXES}{
    \itemize{

      \item When the \pkg{R.oo} package is attached, the use of class
      selectors no longer worked. This is due to the use of the \code{Class}
      name for \pkg{R.oo}'s base class object, where \pkg{selectr} was also
      using it (but not exporting) the same name of \code{Class} for
      representing a class selector. Consequently, \pkg{selectr}'s code was
      changed to rename the class to avoid any clashing. Because it was not
      exported, this is purely an internal change. Thanks to
      Francois Lemaire-Sicre for reporting the issue.

    }
  }
}

\section{Changes in version 0.4-0}{
  \subsection{MINOR CHANGES}{
     \itemize{

      \item Large rewrite of internals to use the R6 OO system instead of
      Reference Classes. This does not affect any external facing code as the
      results should be identical to the previous implementation, which is why
      this change is marked as minor. Initial and crude performance testing
      (by running the test suite) indicates that the R6 implementation is
      approximately twice as fast at generating XPath as the Reference Classes
      implementation.

      \item The minimum required version of R for \pkg{selectr} has been
      increased from \code{2.15.2} to \code{3.0} as that is the minimum
      required version of \pkg{R6}.

      \item Minor performance enhancements have been made. Not only is
      \pkg{R6} faster than Reference Classes, the use of string formatting
      has been replaced with string concatenation. Additionally dynamic
      calling of methods via \code{do.call()} has been replaced with direct
      method calls.

     }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item The issues in previous releases where methods can sometimes be
      missing should now be resolved. The bug appeared to lie in core
      Reference Classes code. By switching to \pkg{R6}, this type of issue
      should no longer be possible.

    }
  }
}


\section{Changes in version 0.3-2}{
  \subsection{MINOR CHANGES}{
     \itemize{

      \item Improved method registration for \pkg{XML} and \pkg{xml2} 
      objects. Avoids checks on each use and is only performed once per
      dependent package load/unload.

     }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item In some environments, reference class methods were missing at
      runtime. This appears to be due to some internal behaviour in them
      \pkg{methods} package where methods are registered on an objects when
      the \code{$} operator is used for a field or method. Instead, when
      a method is missing, they are manually bound to the object.

    }
  }
}

\section{Changes in version 0.3-1}{
  \subsection{MINOR CHANGES}{
     \itemize{

      \item Enabled partial matching on the translator argument to
      \code{css_to_xpath()}. Instead of defaulting to a generic translator,
      a non-matching argument will be returned with an error.

      \item Introduced many more unit tests via the \pkg{covr} package.
      This enabled dead code to be trimmed and also identified areas of code
      which needed improvement. Minor enhancements include: tolerate
      whitespace within a \code{:not()}, more consistent results returned
      from parser methods, improvements to argument parsing.

     }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item The \code{|=} attribute matching operator was not being parsed
      correctly for the generic translator.

      \item Handle scenario where a CSS comment is unclosed. Results in
      everything after the comment start to be removed (which may or may
      not result in a valid selector).

    }
  }
}

\section{Changes in version 0.3-0}{
  \subsection{MAJOR CHANGES}{
    \itemize{

      \item Added support for documents from the \pkg{xml2} package.

      \item selectr now also does not strictly depend on the XML
      package. If either the \pkg{XML} or \pkg{xml2} packages are
      present (which are required for the \code{querySelector} methods
      to work) then \code{querySelector} will begin to work for them.
      This also enables selectr to be used for translation-only.

    }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item Improve support for nth-*(an+b) selectors. Ported from
      cssselect.

    }
  }
}

\section{Changes in version 0.2-3}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Code cleanup contributed by Kun Ren (#1).

      \item Updated DESCRIPTION to include URL and BugReports fields.
      Also update email address.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Fix behaviour for nth-*(an+b) pseudo-class selectors for
      negative a's. Contributed to cssselect by Paul Tremberth,
      ported to R.

      \item Escape delimiting characters to support new version of the
      stringr package. Probably should have been done in the first place.
      Reported by Hadley Wickham (#5).

    }
  }
}

\section{Changes in version 0.2-2}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Corrected licence to BSD 3 clause. This was the licence in
      use previously, but has now been made more explicit.

      \item Removed 'Enhances' field because we import functions from
      \pkg{XML}. This choice is made because \pkg{XML} is a required
      package, rather than an optional package that can be worked with.
      This and the previous change have been made to keep up with recent
      changes in R-devel.

    }
  }
}

\section{Changes in version 0.2-1}{
  \subsection{MINOR FEATURES}{
    \itemize{

      \item Added a 'CITATION' file which cites a technical report on
      the package.

      \item \code{show()} methods are now available on internal objects,
      making interactive extensibility and bug-fixing easier. This is
      simply wrapping the \code{repr()} methods (mirroring the Python
      source) that the same objects have.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Use the session character encoding to determine whether to
      run unicode tests. Tests break in non-unicode sessions otherwise.

    }
  }
}

\section{Changes in version 0.2-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Introduced new functions \code{querySelectorNS()} and
      \code{querySelectorAllNS()} to ease the use of namespaces within a
      document. Previously this would have required knowledge of XPath.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Fix meaning of \code{:empty}, whitespace is not empty.

      \item Use \code{lang()} for XML documents with the \code{:lang()}
      CSS selector.

      \item \code{|ident} no longer produces a parsing error, but is now
      equivalent to just 'ident'.

    }
  }
}

\section{Changes in version 0.1-1}{
  \subsection{BUG FIXES}{
    \itemize{

      \item Now testing unicode only in non-Windows platforms on package
      check. Output should still be consistent, just depends on the
      current charset being unicode.

    }
  }
}

\section{Changes in version 0.1-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Initial port of the Python 'cssselect' package. Code is very
      literally ported, including the test suite.

      \item Wrapped translation functionality into a single function,
      \code{css_to_xpath()}.

      \item Created two convenience functions, \code{querySelector()} and
      \code{querySelectorAll()}. These mirror the behaviour of the same
      functions present in a web browser. \code{querySelector()} returns a
      node, while \code{querySelectorAll()} returns a list of nodes.

    }
  }
}
