\name{NEWS}
\title{NEWS file for the selectr package}
\encoding{UTF-8}

\section{Changes in version 0.6-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Added support for the CSS Selectors Level 4 case-sensitivity
      flags in attribute selectors. \code{[attr="value" i]} matches the
      attribute value ASCII case-insensitively, while
      \code{[attr="value" s]} explicitly requests the default
      case-sensitive matching.

      \item \code{:not()} may now appear inside the argument list of
      functional pseudo-classes, as permitted by CSS Selectors Level 4.
      Selectors such as \code{:not(:not(a))}, \code{:is(:not(.x))},
      \code{:has(:not(a))}, and \code{:nth-child(2 of :not(.foo))}
      previously produced a "Got nested :not()" error and are now
      translated. Note that nesting \code{:has()} inside \code{:has()}
      remains an error, as required by the specification.

      \item \code{:has()} now supports leading combinators in its
      arguments, as permitted by CSS Selectors Level 4 relative
      selectors. \code{e:has(> img)} matches an \code{e} with a child
      \code{img}, \code{e:has(~ p)} one with a subsequent sibling
      \code{p}, and \code{e:has(+ p)} one whose next sibling is a
      \code{p}; previously all three were parse errors. The omitted
      combinator retains its implied-descendant meaning, and leading
      combinators remain invalid in other functional pseudo-classes
      such as \code{:is()}.

      \item Functional pseudo-class arguments may now be complex
      selectors (contain combinators), as permitted by CSS Selectors
      Level 4: \code{:is()}, \code{:where()}, and \code{:not()} take a
      \code{<complex-selector-list>}, \code{:has()} a
      \code{<relative-selector-list>}, and the \code{of S} form of
      \code{:nth-child()}/\code{:nth-last-child()} a
      \code{<complex-selector-list>}. Selectors such as \code{:is(a b)},
      \code{:not(a > b)}, \code{e:has(a + b)}, \code{e:has(> a b)}, and
      \code{li:nth-child(2 of ol li)} previously produced an
      "Expected an argument" error and are now translated. For
      \code{:has()} the argument extends the search path forwards from
      the candidate element; for the other pseudo-classes the rightmost
      compound applies to the candidate element itself and the rest of
      the selector is matched through reversed axes (ancestors,
      parent, and preceding siblings).

    }
  }
  \subsection{MINOR CHANGES}{
    \itemize{

      \item The minimum required version of R has been increased from
      \code{3.0} to \code{3.3}, matching what the code has in fact
      required for some time: \code{anyNA()} needs R 3.1,
      \code{lengths()} R 3.2, and \code{startsWith()} R 3.3. On older
      versions the package installed but failed at runtime with
      "could not find function".

    }
  }
  \subsection{REMOVED FEATURES}{
    \itemize{

      \item The non-standard extensions inherited from the Python
      'cssselect' package have been removed: the \code{:contains("text")}
      pseudo-class and the \code{[attr!=value]} attribute operator. Both
      now produce an error. Neither has ever been part of any CSS
      Selectors specification, so removing them makes \pkg{selectr}
      reject the same selectors a spec-conformant parser rejects.
      Equivalent standard alternatives: \code{[attr!=value]} can be
      written as \code{:not([attr=value])}, and \code{:contains()} as an
      XPath \code{contains(., 'text')} predicate applied outside of
      \pkg{selectr}.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item The namespace argument of the \code{querySelector()} family
      of functions now signals an error when given a list containing an
      element that is not a single string
      (e.g. \code{list(a = c("u1", "u2"), b = "u3")}). A prefix cannot
      map to more than one namespace URI, and the prefix-to-URI pairing
      was silently corrupted: names were re-assigned after the list was
      flattened, shifting every prefix after the multi-element value
      (\code{"u2"} above became namespace \code{b}, and \code{"u3"} lost
      its prefix entirely, producing an \code{//NA:*} filter in
      \code{querySelectorNS()}).

      \item \code{:only-child} and \code{:only-of-type} now match the
      root element, consistent with \code{:first-child:last-child},
      which Selectors defines \code{:only-child} to be equivalent to.
      The previous translation counted the children of the parent
      element (\code{count(parent::*/child::*) = 1}), but the root
      element's parent is the document node, not an element, so the
      count was 0 and the root never matched. The translation now
      requires the absence of preceding and following siblings instead.

      \item The HTML translator's \code{:enabled} and \code{:disabled}
      pseudo-classes now match \code{input} elements that have no
      \code{type} attribute. Such an input defaults to \code{type=text}
      per HTML and should participate in both pseudo-classes, but the
      previous translation used the XPath comparison
      \code{@type != 'hidden'}, which is false when the attribute is
      absent, so an un-typed \code{<input disabled>} matched neither
      \code{:disabled} nor (when enabled) \code{:enabled}. The
      translation now uses \code{not(@type = 'hidden')}; inputs with
      \code{type="hidden"} continue to match neither pseudo-class. This
      bug was inherited from the Python 'cssselect' package.

      \item \code{:dir()} now enforces its argument grammar from CSS
      Selectors Level 4: exactly one identifier. It previously shared
      the \code{:lang()} grammar, accepting strings, the \code{*}
      wildcard, and comma- or space-separated lists (e.g.
      \code{:dir(ltr, rtl)} or \code{:dir("ltr")}); these are now
      rejected as parse errors. As before, identifiers other than
      \code{ltr}/\code{rtl} are valid but never match, and \code{:dir()}
      always translates to a never-matching condition since static XPath
      cannot determine directionality.

      \item Element names inside pseudo-class arguments (e.g.
      \code{:is(svg|g)}, \code{:not(svg|g)}, \code{:has(svg|g)},
      \code{:nth-child(2 of svg|g)}) are now matched with a self-axis
      name test (\code{self::svg:g}) instead of a \code{name()} string
      comparison. Namespace prefixes therefore keep resolving through
      the namespace map supplied at evaluation time (URI-based), just
      as they do at the top level of a selector; previously a document
      binding the same URI to a different prefix matched \code{svg|g}
      at the top level but not inside a pseudo-class argument. This
      also means an unqualified argument such as \code{:is(e)} now
      matches only elements in no namespace, consistent with a
      top-level \code{e}; previously it also matched an \code{e} in a
      default namespace. Names that cannot be written as an XPath name
      test continue to use the \code{name()} comparison.

      \item The no-namespace form \code{|e} now retains its namespace
      constraint when the element name cannot be written as an XPath
      name test (e.g. a Unicode name such as \code{|é}). Previously
      such names fell back to a bare \code{name()} comparison, which
      also matched an element of that name in a \emph{default}
      namespace, since its \code{name()} carries no prefix; the
      translation now tests \code{namespace-uri() = ''} alongside
      \code{local-name()}.

      \item The of-type pseudo-classes (\code{:first-of-type},
      \code{:last-of-type}, \code{:only-of-type}, \code{:nth-of-type()},
      and \code{:nth-last-of-type()}) now work on element names that
      cannot be written as an XPath name test, such as Unicode names
      (e.g. \code{é:only-of-type}) and names in any namespace
      (e.g. \code{*|e:first-of-type}). Previously these failed with the
      misleading error "*:only-of-type is not implemented" even though
      an explicit element was given; siblings of the same type are now
      counted with a \code{name()} (or \code{local-name()}) node test.
      The of-type pseudo-classes on the universal selector itself
      (e.g. \code{*:only-of-type}) remain unimplemented.

      \item The \code{querySelector()} and \code{querySelectorAll()}
      methods for \pkg{xml2} documents now accept a named list as the
      \code{ns} argument, consistent with the methods for \pkg{XML}
      documents and with \code{querySelectorNS()} and
      \code{querySelectorAllNS()}. Previously a named list was rejected
      with "A namespace object must be comprised of characters".

      \item The \code{querySelector()}, \code{querySelectorAll()},
      \code{querySelectorNS()}, and \code{querySelectorAllNS()} functions
      now signal an error when the \code{selector} argument is not a
      single character string, as the documentation has always required.
      Previously a multi-element selector vector was translated to
      multiple XPath expressions, of which all but the first were
      silently ignored when querying the document. To match against a
      group of selectors in one query, use a comma-separated selector
      group, e.g. \code{"a, b"}.

      \item \code{css_to_xpath()} now signals an error when any of its
      arguments contain \code{NA} values. Previously \code{NA}s were
      removed from each argument independently before recycling, which
      silently shifted the pairing of the remaining values: in
      \code{css_to_xpath(c("a", "b", "c"), prefix = c("p1//", NA, "p3//"))}
      the selector \code{"b"} was paired with \code{"p3//"} and
      \code{"c"} wrapped around to \code{"p1//"}.

      \item The \code{An+B} expressions used by the \code{:nth-child()}
      family of pseudo-classes are now matched ASCII case-insensitively,
      as required by the CSS Syntax specification. Previously
      \code{:nth-child(2N)}, \code{:nth-child(ODD)}, and
      \code{:nth-child(EVEN)} were rejected as invalid; they are now
      equivalent to their lowercase forms.

      \item \code{An+B} expressions now only permit whitespace around the
      \code{+}/\code{-} sign that separates the \code{B} value, as
      required by the CSS Syntax specification. Previously whitespace
      between tokens was silently discarded, so an invalid selector such
      as \code{:nth-child(3 7)} was treated as \code{:nth-child(37)} and
      silently matched the wrong elements. Such selectors are now
      rejected as invalid; spec-legal forms like \code{:nth-child(2n + 1)}
      are unaffected.

      \item \code{An+B} expressions now reject non-integer \code{A} and
      \code{B} values, as required by the CSS Syntax specification.
      Previously these were coerced with \code{as.integer()} and silently
      truncated, so \code{:nth-child(1.9)} matched the first child and
      \code{:nth-child(2e1)} matched the twentieth. Such selectors are
      now rejected as invalid; signed integers and leading zeros
      (e.g. \code{:nth-child(+05)}) remain valid.

      \item Computing the specificity of a \code{:has()} selector with a
      single argument (e.g. \code{e:has(img)}) no longer fails with
      "incorrect number of dimensions"; multi-argument forms were
      unaffected.

      \item Computing the specificity of an \code{:is()} (or
      \code{:matches()}) selector with a single argument
      (e.g. \code{:is(.foo)}) no longer fails with "incorrect number of
      dimensions". In addition, the specificity of the compound the
      pseudo-class is attached to is no longer dropped:
      \code{div:is(.foo)} now reports (0, 1, 1) rather than (0, 1, 0).

      \item Nesting \code{:has()} inside \code{:has()} (at any depth,
      e.g. \code{section:has(article:has(div))}) is now rejected, as
      required by CSS Selectors Level 4, whose \code{:has()} argument
      grammar excludes \code{:has()}. This matches the behaviour of
      browsers. Sibling uses such as \code{e:has(a):has(b)} and
      \code{:has()} inside other functional pseudo-classes
      (e.g. \code{e:is(:has(b))}) remain valid; the previously
      accepted nested form can be expressed with a descendant
      argument instead (e.g. \code{section:has(div)}).

      \item \code{:lang()} and \code{:dir()} no longer accept a lone
      \code{-} as an argument. A lone \code{-} is not a valid CSS
      identifier (an identifier may start with \code{-} only when
      followed by an identifier-start character or a second \code{-}),
      but the tokenizer lexed it as one and it passed argument
      validation. Valid identifiers such as \code{:lang(--x)} and
      \code{:lang(en--)}, and the wildcard form \code{:lang(en-*)},
      are unaffected.

      \item Attribute selectors with an empty value no longer throw an
      error for the \code{=} and \code{|=} operators
      (e.g. \code{[attr=""]}). These now translate to XPath matching an
      attribute whose value is the empty string.

      \item The HTML translator no longer lowercases attribute
      \emph{values}, only attribute names. Previously a selector such as
      \code{[data-state="Active"]} was translated to match the value
      \code{'active'}, silently missing matches because HTML attribute
      values are case-sensitive.

      \item Namespace selectors are now translated faithfully. Previously
      both the \emph{any namespace} selector \code{*|e} and the
      \emph{no namespace} selector \code{|e} collapsed to the bare name
      \code{e}, which in XPath only matches elements with no namespace.
      \code{*|e} now translates to \code{*[local-name() = 'e']} so that
      it matches \code{e} in any namespace, and similarly \code{[*|attr]}
      translates to a \code{local-name()} test on attributes.

      \item Unicode escapes are now supported in identifiers and ID
      selectors, not just strings, and are decoded to the characters
      they represent. Previously \code{#\\31 23} (the only valid way to
      write an ID starting with a digit, here \code{123}) was a
      tokenization error, and escapes that did tokenize kept their hex
      digits rather than the escaped character, so \code{"\\E9"} matched
      the literal value \code{E9} instead of \code{é}.

      \item The alternatives of \code{:is()}, \code{:matches()}, and
      \code{:where()} are now grouped as a single condition. Previously
      each alternative was OR-ed onto the compound selector's whole
      condition chain, so \code{div.foo:is(.a, .b)} translated to
      \code{foo or a or b} (matching any \code{div.foo}) instead of
      \code{foo and (a or b)}, and conditions following the
      pseudo-class, as in \code{:is(.a, .b):first-child}, bound only to
      the last alternative. Stacked pseudo-classes such as
      \code{e:is(.a):is(.b)} now correctly require both conditions
      rather than either.

      \item The universal selector \code{*} is no longer silently
      dropped when it appears alongside other arguments in a selector
      list. A list containing \code{*} matches everything, so
      \code{:is(div, *)} now matches every element rather than only
      \code{div}, \code{:not(div, *)} now matches nothing rather than
      excluding only \code{div}, and \code{:nth-child(2 of div, *)} now
      counts all siblings rather than only \code{div} siblings. Selector
      lists whose only argument is \code{*} (e.g. \code{:not(*)}) were
      already translated correctly and are unchanged.

      \item Wildcard language ranges such as \code{:lang(en-*)} now
      match under the generic (XML) translator. Previously only the
      \code{*} was stripped, producing \code{lang('en-')}, which can
      never match: XPath's \code{lang()} function only extends its
      argument at a \code{-} boundary, so \code{xml:lang="en-US"}
      matches \code{lang('en')} but not \code{lang('en-')}. The
      trailing \code{-*} is now stripped so \code{:lang(en-*)} matches
      \code{en}, \code{en-US}, etc., per RFC 4647 extended filtering.
      The HTML translator was unaffected.

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
