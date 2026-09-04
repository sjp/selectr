\name{selectors}
\alias{selectors}
\title{
  Which CSS selectors selectr supports, and what they translate to
}
\description{
  A reference table of every combinator, simple selector, attribute
  operator and pseudo-class selectr recognises: whether it is
  supported, restricted to the \code{html}/\code{xhtml} translators,
  never matches (a static-document limitation), or is rejected as an
  error, plus the XPath a representative selector translates to. This
  complements the prose in \code{\link{css_to_xpath}}, which explains
  \emph{why} the divergences from CSS Selectors Level 4 below exist;
  this page is the flat list to check "is X supported?" against.

  Every example on this page is exercised by
  \code{tests/testthat/test-selectors-reference.R} against a live
  \code{\link{css_to_xpath}} call, and that test also asserts every
  \code{xpath_*_pseudo}, \code{xpath_*_function} and
  \code{xpath_*_combinator} method of \code{GenericTranslator} and
  \code{HTMLTranslator} is represented on this page, so this table
  cannot silently drift from the translators' code.
}
\section{Combinators}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Meaning} \tab \bold{Example XPath (\code{"e ? f"}, generic)} \cr
    \code{e f} \tab descendant \tab \code{descendant-or-self::e//f} \cr
    \code{e > f} \tab child \tab \code{descendant-or-self::e/f} \cr
    \code{e + f} \tab direct adjacent sibling \tab \code{descendant-or-self::e/following-sibling::*[1][self::f]} \cr
    \code{e ~ f} \tab indirect (general) sibling \tab \code{descendant-or-self::e/following-sibling::f} \cr
    \code{e || f} \tab column (Selectors 4) \tab error - not supported, see below \cr
  }
}
\section{Simple selectors}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Meaning} \tab \bold{Example XPath} \cr
    \code{*} \tab universal \tab \code{descendant-or-self::*} \cr
    \code{e} \tab type (no namespace) \tab \code{descendant-or-self::e} \cr
    \code{.class} \tab class \tab \code{...[contains(concat(' ', normalize-space(@class), ' '), ' class ')]} \cr
    \code{#id} \tab ID \tab \code{descendant-or-self::*[@id = 'id']} \cr
  }

  Class matching splits \code{@class} on XML whitespace
  (space/tab/CR/LF) via \code{normalize-space()}; HTML's own
  "set of space-separated tokens" also treats U+000C form feed as a
  separator, which this does not - negligible in practice, since form
  feed in a \code{class} attribute is vanishingly rare.
}
\section{Attribute selectors}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Meaning} \tab \bold{Example XPath (\code{"[attr ? val]"})} \cr
    \code{[attr]} \tab has attribute \tab \code{descendant-or-self::*[@attr]} \cr
    \code{[attr=val]} \tab equals \tab \code{descendant-or-self::*[@attr = 'val']} \cr
    \code{[attr~=val]} \tab includes a whitespace-separated token \tab \code{...[contains(concat(' ', normalize-space(@attr), ' '), ' val ')]} \cr
    \code{[attr|=val]} \tab equals, or a \code{"val-"} prefix \tab \code{...[@attr = 'val' or starts-with(@attr, 'val-')]} \cr
    \code{[attr^=val]} \tab starts with \tab \code{descendant-or-self::*[starts-with(@attr, 'val')]} \cr
    \code{[attr$=val]} \tab ends with \tab \code{...[substring(@attr, string-length(@attr)-2) = 'val']} \cr
    \code{[attr*=val]} \tab contains substring \tab \code{descendant-or-self::*[contains(@attr, 'val')]} \cr
  }

  Every operator above accepts a trailing Selectors 4 case-sensitivity
  flag, \code{i} (ASCII case-insensitive) or \code{s} (case-sensitive,
  the default and so a no-op): \code{[attr=val i]} translates to
  \code{...[translate(@attr, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
  'abcdefghijklmnopqrstuvwxyz') = 'val']}. Any other flag is a parse
  error.
}
\section{Structural pseudo-classes}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Status} \tab \bold{Example XPath} \cr
    \code{:root} \tab supported \tab \code{descendant-or-self::*[not(parent::*)]} \cr
    \code{:first-child} \tab supported \tab \code{...[count(preceding-sibling::*) = 0]} \cr
    \code{:last-child} \tab supported \tab \code{...[count(following-sibling::*) = 0]} \cr
    \code{:only-child} \tab supported \tab preceding- and following-sibling counts both 0 \cr
    \code{e:first-of-type} \tab supported (needs a named element) \tab \code{...e[count(preceding-sibling::e) = 0]} \cr
    \code{e:last-of-type} \tab supported (needs a named element) \tab \code{...e[count(following-sibling::e) = 0]} \cr
    \code{e:only-of-type} \tab supported (needs a named element) \tab preceding- and following-sibling \code{e} counts both 0 \cr
    \code{*:first-of-type} etc. \tab error \tab needs each sibling's own name; not expressible in XPath 1.0 \cr
    \code{:nth-child(An+B)} \tab supported \tab \code{:nth-child(2n+1)} gives \code{...[count(preceding-sibling::*) mod 2 = 0]}; a \code{B} outside the first cycle adds a \code{>= B-1} bound and an offset in the \code{mod} \cr
    \code{:nth-child(An+B of S)} \tab supported \tab preceding-siblings filtered to \code{S}, plus \code{self::} test \cr
    \code{:nth-last-child()} \tab supported \tab as \code{:nth-child()}, counting from the end \cr
    \code{e:nth-of-type()}, \code{e:nth-last-of-type()} \tab supported (needs a named element) \tab as above, restricted to siblings named \code{e} \cr
    \code{:empty} \tab supported, Selectors \bold{3} semantics \tab \code{...[not(*) and not(string-length())]} - see "Divergences" below \cr
    \code{:scope} \tab supported, leading position only \tab \code{self::*} (replaces the \code{prefix}); errors elsewhere in a selector \cr
  }
}
\section{Selector-list pseudo-classes}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Meaning} \tab \bold{Example XPath} \cr
    \code{:not(e)} \tab none of the arguments match \tab \code{descendant-or-self::*[not(self::e)]} \cr
    \code{:is(e, f)} (alias \code{:matches()}) \tab any argument matches \tab \code{descendant-or-self::*[self::e or self::f]} \cr
    \code{:where(e, f)} \tab any argument matches (zero specificity) \tab same XPath as \code{:is()} \cr
    \code{:has(> e)} \tab a descendant/relative match exists \tab \code{descendant-or-self::*[child::e]} \cr
  }

  Each accepts a full selector list, but a \code{:scope} inside any of
  them is an error (see \code{:scope} above), and the column
  combinator inside them is likewise unsupported.
}
\section{Linguistic and directionality pseudo-classes}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Status} \tab \bold{Notes} \cr
    \code{:lang(range)} \tab supported, translator-dependent \tab generic: XPath \code{lang()}, prefix match only. \code{html}/\code{xhtml}: RFC 4647 extended filtering for multi-subtag ranges (a wildcard subtag may skip any position); see \code{\link{css_to_xpath}} for the full rules \cr
    \code{:lang("")} \tab supported, every translator \tab matches an element with no content language anywhere in its ancestor-or-self chain \cr
    \code{:dir()} \tab never matches, every translator \tab \code{descendant-or-self::*[0]}; directionality needs a live DOM (\code{dir="auto"}, \code{bdi}, form controls) \cr
  }
}
\section{Link and interaction-state pseudo-classes}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{generic} \tab \bold{html / xhtml} \cr
    \code{:link}, \code{:any-link} \tab never matches \tab matches \code{a} and \code{area} elements with an \code{href} (a \code{link} element is metadata, not a hyperlink) \cr
    \code{:visited} \tab never matches \tab never matches (no browser history in a static document) \cr
    \code{:hover}, \code{:active}, \code{:focus}, \code{:focus-within}, \code{:focus-visible} \tab never matches \tab never matches (runtime UI state) \cr
    \code{:target}, \code{:target-within} \tab never matches \tab never matches (needs the document's URL fragment) \cr
    \code{:local-link} \tab never matches \tab never matches (needs the document's URL) \cr
  }
  "Never matches" translates to \code{descendant-or-self::*[0]}: valid
  CSS, always zero results, rather than an error. In a larger compound
  the always-false \code{0} absorbs the compound's other conditions,
  which cannot change the outcome, so \code{"a.external:visited"}
  translates to \code{descendant-or-self::a[0]} as well.
}
\section{HTML form-state pseudo-classes}{
  These are only meaningfully supported by the \code{html} and
  \code{xhtml} translators (under \code{generic} they never match,
  listed above as the general runtime-state case). Every one matches
  by local name regardless of
  namespace, so \code{"*|input:disabled"} works the same as
  \code{"input:disabled"} on an unnamespaced document. \code{:enabled}
  and \code{:disabled} match only the elements listed below - in
  particular a hyperlink is not \code{:enabled}; use \code{:link} or
  \code{:any-link} for links.

  \tabular{ll}{
    \bold{Selector} \tab \bold{Elements and condition (html/xhtml)} \cr
    \code{:enabled} / \code{:disabled} \tab \code{button}, \code{input}, \code{select}, \code{textarea}, \code{optgroup}, \code{option}, \code{fieldset}; a disabled ancestor \code{fieldset} disables descendants except inside its first \code{legend}, and a disabled \code{select} or \code{optgroup} disables the \code{optgroup}s and \code{option}s below it \cr
    \code{:checked} \tab checked \code{checkbox}/\code{radio} \code{input}s and selected \code{option}s; does \emph{not} infer the implicit default selection of an unadorned single-select or radio group \cr
    \code{:required} / \code{:optional} \tab \code{input} (not type \code{hidden}), \code{select}, \code{textarea}, by presence of \code{required} \cr
    \code{:read-write} \tab an editable textual/numeric/date-time \code{input} or \code{textarea} that is not \code{readonly}/disabled, or any element with its own non-\code{"false"} \code{contenteditable} (inherited/ambiguous \code{contenteditable} is not resolved) \cr
    \code{:read-only} \tab the negation of \code{:read-write} (matches everything else, e.g. a checkbox or a plain \code{div}) \cr
    \code{:placeholder-shown} \tab \code{input}/\code{textarea} with a \code{placeholder} and an empty current value \cr
    \code{:default} \tab a selected \code{option}, a checked \code{checkbox}/\code{radio}, or the first submit button in its nearest ancestor \code{form} (does not follow a \code{form=} attribute) \cr
  }

  Because HTML's \code{type} is an enumerated attribute, these match
  its keywords ASCII case-insensitively (\code{<input type="RADIO">}
  is \code{:checked}). An \code{input} with no \code{type} defaults to
  text.
}
\section{Column combinator and pseudo-classes (unsupported)}{
  The Selectors 4 column combinator (\code{a || b}) and the column
  pseudo-classes \code{:nth-col()} / \code{:nth-last-col()} are
  rejected with an error: which column a cell belongs to depends on
  \code{colspan}/\code{rowspan} table-layout arithmetic that XPath 1.0
  cannot express.
}
\section{Namespaces}{
  \tabular{lll}{
    \bold{Selector} \tab \bold{Meaning} \tab \bold{Example XPath (\code{"? p"})} \cr
    \code{p} \tab \code{p} in no namespace \tab \code{descendant-or-self::p} \cr
    \code{d|p} \tab \code{p} in the namespace prefix \code{d} resolves to via the \code{ns} map \tab \code{descendant-or-self::d:p} \cr
    \code{*|p} \tab \code{p} in any namespace \tab \code{descendant-or-self::*[local-name() = 'p']} \cr
    \code{|p} \tab \code{p} in no namespace, spelled explicitly \tab \code{descendant-or-self::p} \cr
  }
  Prefixes such as \code{d} above are resolved through the \code{ns}
  argument passed to \code{\link[xml2]{xml_find_all}} /
  \code{\link[XML]{getNodeSet}} at query time, not through whatever
  prefix the document itself uses; see \code{\link{querySelectorAll}}.
  \code{HTMLTranslator} additionally lower-cases every element and
  attribute name (including namespaced ones, so
  \code{svg|linearGradient} becomes \code{svg:lineargradient}), which
  matches libxml2's HTML parser but would be wrong against a tree that
  restores camelCase SVG/MathML names (browsers, html5ever).
}
\section{Divergences from CSS Selectors Level 4}{
  \enumerate{
    \item \code{:empty} keeps Selectors \bold{3} semantics: an element
    containing only white space, e.g. \code{<p> </p>}, does not match.
    Selectors 4 loosened this to also match white-space-only content,
    but no browser has shipped that change, so it is treated as not
    implemented, tracking browser behaviour rather than the spec text.
    \item \code{:checked} tests only \code{@checked}/\code{@selected}.
    It does not infer the implicit selectedness of an \code{option}
    with no \code{selected} attribute anywhere in its \code{select}
    (the first option is selected by default), nor a radio group's
    mutual exclusivity - both need a live DOM to resolve.
    \item \code{HTMLTranslator} lower-cases foreign-content element and
    attribute names unconditionally, targeting libxml2-style HTML
    trees (see "Namespaces" above); an HTML5 parser that restores
    camelCase SVG/MathML names would disagree.
    \item Class/token matching (\code{.foo}, \code{[attr~=val]}) does
    not treat U+000C form feed as whitespace, unlike HTML's ASCII
    whitespace definition (see "Simple selectors" above).
  }
}
\references{
  CSS Selectors Level 4 \url{https://www.w3.org/TR/selectors-4/}, XPath
  \url{https://www.w3.org/TR/xpath/}.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{css_to_xpath}} for the full prose explanation of each
  divergence above and the error classes raised for unsupported
  selectors; \code{\link{querySelectorAll}} for namespace and chaining
  semantics when querying a document.
}
