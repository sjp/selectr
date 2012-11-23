\name{NEWS}
\title{NEWS file for the selectr package}
\encoding{UTF-8}

\section{Changes in version 0.2-0}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Introduced new functions 'querySelectorNS()' and
      'querySelectorAllNS()' to ease the use of namespaces within a
      document. Previously this would have required knowledge of XPath.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Fix meaning of ':empty', whitespace is not empty.

      \item Use 'lang()' for XML documents with the ':lang() CSS
      selector'.

      \item '|ident' no longer produces a parsing error, but is now
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
      'css_to_xpath()'.

      \item Created two convenience functions, 'querySelector()' and
      'querySelectorAll()'. These mirror the behaviour of the same
      functions present in a web browser. 'querySelector()' returns a
      node, while 'querySelectorAll()' returns a list of nodes.

    }
  }
}
