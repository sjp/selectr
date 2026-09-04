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
  See \code{\link{selectors}} for a table of every combinator,
  attribute operator and pseudo-class this function supports, along
  with an example translation for each; the rest of this section
  explains the reasoning behind the more surprising entries in that
  table.

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
  (\code{:lang(*-CH)}, \code{:lang(de-*-DE)}; quoted or not). The
  \code{html} and \code{xhtml} translators implement RFC 4647 extended
  filtering for any range naming more than one subtag, approximated
  from the nearest language-attributed ancestor: a wildcard, explicit
  or not, may skip any subtag, so \code{:lang(de-DE)} - with no
  wildcard at all - matches \code{lang="de-Latn-DE"}. The first
  subtags of range and tag are always paired, though, so a leading
  wildcard \emph{consumes} the tag's primary subtag rather than
  skipping over it: \code{:lang(*-CH)} matches \code{lang="de-CH"} and
  \code{lang="fr-Latn-CH"}, but neither \code{lang="ch"} nor
  \code{lang="ch-DE"}. A single-subtag range
  (\code{:lang(en)}, \code{:lang(en-*)}) is a plain prefix test, as
  before. The generic translator has only XPath's \code{lang()}
  function, which does Selectors 3 \code{|=}-style prefix matching and
  cannot express extended filtering: a non-trailing wildcard is
  rejected there with an error rather than silently mis-matching, and a
  multi-subtag range with no wildcard (e.g. \code{:lang(de-DE)}) is
  matched as a prefix only - it does \emph{not} skip subtags the way
  the \code{html}/\code{xhtml} translators do.

  Every range must be an RFC 4647 \emph{extended language range}:
  each subtag is either a whole \code{*} or one to eight
  alphanumeric characters (letters only, for the first), so an
  empty, over-long or non-alphanumeric subtag, or a \code{*} glued to
  a subtag, is rejected with an error naming the range
  (\code{:lang(en-)}, \code{:lang(--x)}, \code{:lang(en*)},
  \code{:lang(de-*--de)}). The specification says such a range simply
  matches nothing while leaving the selector valid; XPath has no
  never-matching form that survives being combined with the rest of the
  expression, so the range is refused rather than translated into a
  well-formed one that would select the wrong elements. An empty item
  of the comma-separated list (\code{:lang(en, )}) is likewise an
  error.

  \code{:lang("")} matches an element whose content language is not
  tagged at all: no \code{lang}/\code{xml:lang} (as applicable to the
  translator) anywhere in its ancestor-or-self chain, or only an empty
  one. This holds for every translator.

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
  particular it qualifies a number of pseudo-classes - \code{:checked},
  \code{:default}, \code{:disabled}, \code{:enabled}, \code{:link},
  \code{:optional}, \code{:placeholder-shown}, \code{:read-only},
  \code{:read-write} and \code{:required} - to apply only to relevant
  (X)HTML elements, identified by local name regardless of namespace so
  that, for example, \code{"*|input:disabled"} and
  \code{"d1|input:disabled"} apply \code{:disabled} exactly as
  \code{"input:disabled"} does on an unnamespaced HTML document. See
  \code{\link{selectors}} for exactly which elements and attributes
  each of these matches.

  When the translator is set to \code{html}, all element and attribute
  names will be converted to lower case (\code{A}-\code{Z} only, as an
  HTML parser folds them), and the attributes HTML defines as ASCII
  case-insensitive - \code{type}, \code{rel}, \code{lang} and the
  rest of the list in \code{\link{selectors}} - also compare their
  \emph{values} without regard to case. Both are removed when the
  translator is \code{xhtml} (or the default \code{generic}
  translator), neither of which serves HTML documents.
}
\value{
  A character vector of XPath expressions.
}
\section{Errors}{
  Every error \code{css_to_xpath} raises is a classed condition
  inheriting \code{selectr_error} (itself an \code{error}), so a caller
  can catch the whole family with one handler, or narrow to a specific
  class when it needs to react differently:

  \describe{
    \item{\code{selectr_parse_error}}{
      \code{selector} is malformed CSS, e.g. \code{"div >"} with
      nothing following the combinator. Carries \code{selector} (the
      offending input) and \code{pos}, the 1-based character position
      within \code{selector} at which the parser gave up
      (\code{NULL} if no position applies).
    }
    \item{\code{selectr_translation_error}}{
      \code{selector} is valid CSS but cannot be expressed in XPath
      1.0 by the requested \code{translator}, e.g. \code{:scope}
      nested inside \code{:is()}, or an unknown pseudo-class such as
      \code{":frobnicate"}. Carries \code{selector} and \code{feature},
      a short description of the unsupported construct.
    }
    \item{\code{selectr_argument_error}}{
      An R-level argument is invalid: the wrong type, length, or an
      \code{NA} or unrecognised value where one is not allowed. Carries
      no extra fields.
    }
  }

  \preformatted{
  tryCatch(
    css_to_xpath("div >"),
    selectr_parse_error = function(e) {
      cat(conditionMessage(e), "\n")
      cat("Position:", e$pos, "\n")
    }
  )
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
  \code{\link{selectors}} for the full selector-support reference;
  \code{\link{querySelectorAll}}, which propagates the same conditions.
}
\examples{
  css_to_xpath(".testclass")
  css_to_xpath("#testid", prefix = "")
  css_to_xpath("#testid .testclass")
  css_to_xpath(":scope > .testclass")
  css_to_xpath(":checked", translator = "html")

  # The selectr_parse_error, selectr_translation_error and
  # selectr_argument_error conditions (see 'Errors' below) all inherit
  # 'selectr_error', so callers can catch the family or a specific class.
  tryCatch(
    css_to_xpath("div >"),
    selectr_parse_error = function(e) {
      cat(conditionMessage(e), "\n")
      cat("Position:", e$pos, "\n")
    }
  )
}
