\name{css_to_xpath}
\alias{css_to_xpath}
\title{
  Translate a CSS selector to an equivalent XPath expression.
}
\description{
  This function aims to create an XPath expression equivalent to what
  would be matched by the given CSS selector. The reason the translation
  is required is because the XML and xml2 packages, being a libxml2
  wrappers, can only evaluate XPath expressions.

  Using this function, it is possible to search an XML tree without the
  prerequisite of knowing XPath.
}
\usage{
css_to_xpath(selector,
             prefix = "descendant-or-self::",
             translator = "generic")
}
\arguments{
  \item{selector}{
    A character vector of CSS selectors.
  }
  \item{prefix}{
    The prefixes to apply to the resulting XPath expressions. The
    default or \code{""} are most commonly used.
  }
  \item{translator}{
    The type of translator that will be used. Possible options are
    \code{generic} (the default), or \code{html} or \code{xhtml}.
  }
}
\details{
  Each selector given to this function will be translated to an
  equivalent XPath expression. Each of \code{selector}, \code{prefix}
  and \code{translator} must either have length 1, in which case its
  single value is used for every translation, or the common length of
  the longer arguments. Unlike base R, a length that is merely a
  multiple (or a fraction) of the common length is not recycled but is
  an error, so that a mistyped argument length does not quietly produce
  a plausible-looking result. The resulting XPath expression can be
  given a prefix which determines the scope of the expression. The
  default prefix determines the scope to be the node itself and all
  descendants of the node. Most commonly the prefix is either the
  default or \code{""}, unless it is known what scope a particular XPath
  expression should have.

  A selector starting with the \code{:scope} pseudo-class is anchored
  at the node the expression is evaluated from: the \code{prefix}
  argument is ignored and the expression begins with the XPath
  \code{self} axis instead. For example, \code{":scope > a"} translates
  to \code{"self::*/a"}, matching only the \code{a} children of the
  queried node, and a bare \code{":scope"} translates to
  \code{"self::*"}, matching the queried node itself. \code{:scope}
  anywhere else in a selector (after a combinator, or within a
  functional pseudo-class such as \code{:is()} or \code{:has()}) cannot
  be expressed in XPath 1.0 and is an error.

  A type selector carrying no namespace prefix, such as \code{"p"},
  becomes an XPath name test and so matches elements in no namespace
  only. That is true wherever the name appears in the selector, so
  \code{":is(p)"}, \code{":not(p)"} and \code{":has(p)"} match
  exactly the elements \code{"p"} itself does. An element that is in a
  namespace, a default namespace declared with \code{xmlns} included,
  has to be selected through a prefix, as in \code{"d|p"}: prefixes are
  resolved through the namespace map supplied when the expression is
  evaluated (the \code{ns} argument of
  \code{\link[xml2]{xml_find_all}} or \code{\link[XML]{getNodeSet}}),
  not through the prefix spelled in the document. Two forms need no
  such map: \code{"*|p"} matches \code{p} in any namespace, and
  \code{"|p"} is the explicit spelling of \code{p} in no namespace.

  The of-type pseudo-classes (\code{:first-of-type},
  \code{:last-of-type}, \code{:only-of-type}, \code{:nth-of-type()} and
  \code{:nth-last-of-type()}) are only supported when their compound
  selector names an element, as in \code{"p:first-of-type"}. Applied to
  the universal selector, as in \code{"*:first-of-type"}, they would
  have to compare each sibling's name against the matched element's own
  name, which XPath 1.0 cannot express, so the translation is an
  error. The Python \sQuote{cssselect} library, from which selectr is
  ported, has the same limitation.

  The Selectors 4 column combinator (\code{"a || b"}) and the column
  pseudo-classes \code{:nth-col()} and \code{:nth-last-col()} are also
  not supported: which column a cell belongs to depends on table-layout
  arithmetic (\code{colspan}/\code{rowspan} carry-over) that XPath 1.0
  cannot express. Both are rejected with an error.

  \code{:empty} deliberately keeps the Selectors 3 semantics that all
  current browsers implement: an element containing only white space,
  such as \code{<p> </p>}, does not match. (The Selectors 4
  specification loosened \code{:empty} to also match
  white-space-only elements, but no browser has shipped that change.)

  \code{:lang()} ranges are matched as RFC 4647 language ranges, so a
  wildcard may appear as a whole range (\code{:lang(*)}), as a trailing
  subtag (\code{:lang(en-*)}), or in a non-trailing position
  (\code{:lang(*-CH)}, \code{:lang(de-*-DE)}; quoted or not), the last of
  these being RFC 4647 extended filtering. The \code{html} and
  \code{xhtml} translators approximate a non-trailing wildcard from the
  nearest language-attributed ancestor (an element matches
  \code{:lang(*-CH)} when its language tag carries a \code{CH} subtag in
  any position). The generic translator has only XPath's \code{lang()}
  function, which cannot express extended filtering, so there a
  non-trailing wildcard is rejected with an error rather than silently
  mis-matching.

  Which attribute supplies that language differs by translator: the
  \code{html} translator reads \code{lang}, while the \code{xhtml}
  translator reads \code{xml:lang} or \code{lang}, preferring
  \code{xml:lang} where an element carries both, as the HTML language
  determination does. The generic translator uses XPath's \code{lang()}
  function, which is defined in terms of \code{xml:lang} alone. With
  every translator the language comes from the nearest ancestor-or-self
  that declares one, and an empty value there resets the language to
  unknown.

  \code{:dir()} translates to a never-matching expression with every
  translator, including \code{html}: an element's resolved
  directionality also depends on \code{dir="auto"}, \code{bdi}, and
  form-control rules that a static document cannot answer, so unlike
  \code{:lang()} it is not approximated from ancestor attributes.

  The translator used is usually unnecessary to specify as the default
  is sufficient for most cases. However, it is of use when creating
  expressions relating to (X)HTML pseudo elements and languages. In
  particular it qualifies the following pseudo selectors to apply only
  to relevant (X)HTML elements: \code{:checked}, \code{:disabled},
  \code{:enabled}, \code{:link}, \code{:optional} and \code{:required}.
  \code{:enabled} and \code{:disabled} partition the elements HTML
  allows to be \dQuote{actually disabled} - \code{button},
  \code{input}, \code{select}, \code{textarea}, \code{optgroup},
  \code{option} and \code{fieldset} - and match nothing outside that
  set. In particular a hyperlink is not \code{:enabled}: an early
  draft said otherwise, but no browser matches \code{a:enabled}, so
  use \code{:link} or \code{:any-link} for links. \code{:checked}
  matches checked \code{checkbox} and \code{radio} inputs and
  selected \code{option}s. The obsolete \code{command} and
  \code{keygen} elements are matched by none of the three.
  Because \code{type} is an HTML enumerated attribute, the
  form-state pseudo-classes match its keywords ASCII
  case-insensitively, so \code{<input type="RADIO">} is \code{:checked}
  and \code{<input type="HIDDEN">} is excluded from \code{:disabled}
  just as their lower-case spellings are. An \code{<input>} with no
  \code{type} defaults to text and so participates in
  \code{:enabled}/\code{:disabled}. A disabled \code{<fieldset>}
  disables its descendant form controls for \code{:disabled} and
  \code{:enabled}, except those inside its first \code{<legend>} child,
  which stay \code{:enabled}; nested disabled fieldsets are handled
  correctly.

  When the translator is set to \code{html}, all elements and
  attributes will be converted to lower case. This restriction is
  removed when the translator is \code{xhtml} (or the default
  \code{generic} translator).
}
\value{
  A character vector of XPath expressions.
}
\references{
  CSS Selectors Level 4 \url{https://www.w3.org/TR/selectors-4/}, XPath
  \url{https://www.w3.org/TR/xpath/}.
}
\author{
  Simon Potter
}
\examples{
  css_to_xpath(".testclass")
  css_to_xpath("#testid", prefix = "")
  css_to_xpath("#testid .testclass")
  css_to_xpath(":scope > .testclass")
  css_to_xpath(":checked", translator = "html")
}
