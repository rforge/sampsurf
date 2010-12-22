\name{heapIZ}
\alias{heapIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ \sQuote{Heap} Up a Sampling Surface }

\description{ This generic has only one method as detailed in
  \code{\link{heapIZ-methods}}. It is used to \dQuote{heap} an object of
  class "\code{\linkS4class{InclusionZoneGrid}}" onto one of class
  "\code{\linkS4class{Tract}}" to build an object of class
  "\code{\linkS4class{sampSurf}}." Note that the heaping would be
  applied to several objects of the first class to ultimately build a
  surface. }

\usage{
heapIZ(izgObject, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{izgObject}{ An object of class \dQuote{InclusionZoneGrid} or subclass. }

  \item{tract}{ An object of class \dQuote{Tract}.  }

  \item{\dots}{ See methods for other arguments. }
}

\details{ This method is really not meant to be used stand-alone. It is
  called within a \code{\link{sampSurf}} constuctor in order to
  \sQuote{heap} or  \sQuote{pile} the values with inclusion zones to
  build a sampling surface. See these routines for details.
}

\value{ An object of class \dQuote{Tract} or subclass. }

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{ \code{\link{heapIZ-methods}} }

\examples{
#
# see sampSurf constructor source code for details.
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
