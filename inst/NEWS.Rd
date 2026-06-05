\name{NEWS}
\title{NEWS file for the selectr package}
\encoding{UTF-8}

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
