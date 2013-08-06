\name{antitheticContainer-class}
\Rdversion{1.1}
\docType{class}
\alias{antitheticContainer-class}

\title{Class \code{"antitheticContainer"}}

\description{
 This class allows one to store a collection of objects
  that are all of class "\code{\linkS4class{antitheticSampling}}."
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("antitheticContainer", ...)}. However, the actual constructor
method (\code{\link{antitheticContainer}}) need never be called
directly; instead, use the \code{\link{antitheticSampling}} constructor
to create container objects of this class.  }

\section{Slots}{

  \describe{
	
    \item{\code{mcsObjs}:}{Object of class \code{"list"}: A list of
      objects that must be of class "\code{\linkS4class{antitheticSampling}}." }
	
    \item{\code{stats}:}{Object of class \code{"matrix"}:  Summary
      statistics for the objects in slot \code{mcsObjs}.   }
	
    \item{\code{description}:}{Object of class \code{"character"} A
    character description of the contents if desired. }
  }
}

\section{Extends}{
Class \code{"\linkS4class{mcsContainer}"}, directly.
}

\section{Methods}{
No methods defined with class "antitheticContainer" in the
signature. However, \code{plot}, \code{show}, \code{summary} and
\code{hist} methods will work via inheritance.
}

\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
}
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\note{ Please note that this is not a completely functional container
  class in the traditional sense at present as it does not have
  replacement, deletion, or addition functions. If you need to do any of
  these operations, perform them on the \code{list} object (in the
  \code{mcsObjs} slot) and then recreate the container. If the object is
  not re-built after, e.g., deletion, the summary statistics will be
  incorrect.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{antitheticContainer}}, \code{\link{antitheticSampling}},
\code{\linkS4class{mcsContainer}}.
}

\examples{
showClass("antitheticContainer")
}
\keyword{classes}
