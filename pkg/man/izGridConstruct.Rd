\name{izGridConstruct}
\alias{izGridConstruct}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Common code for constructing objects of class
  \dQuote{InclusionZoneGrid} }

\description{
  This routine is called by all \code{\link{izGrid}} methods that assign
  a constant surface within an object's inclusion zone. It should not,
  therefore, be used for sampling protocols such as the chainsaw method,
  where a variable surface is computed within the inclusion zone.
}

\usage{
izGridConstruct(izObject, tract,
                description = "sausageIZ inclusion zone grid object",
                wholeIZ = TRUE, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{izObject}{ An object of class
	"\code{\linkS4class{InclusionZone}}", with the exceptions stated
	above.}
		 
  \item{tract}{ An object of class "Tract" or subclass.}
		 
  \item{description}{ A description of the object as a character
	string.} 
		 
  \item{wholeIZ}{ \code{TRUE}: make a background grid covering
	the entire object including the stem and the inclusion zone;
	\code{FALSE}: make the grid cover just the inclusion zone. }
	  
  \item{\dots}{ Other arguments to be passed along (not currently used).}

}


\details{
  This routine should never need to be called by the user at the command
  line. It is meant to be used with calls set up in the
  \code{\link{izGrid-methods}}. It simply collects code that is common
  to all methods that assign a constant surface height with a given
  inclusion zone. The inclusion zone can be of any shape commonly
  encountered in areal sampling methods. As long as it is a legal
  "\code{\linkS4class{InclusionZone}}" object, it should work.
}

\value{
A valid object of class "\code{\linkS4class{InclusionZoneGrid}}."
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\link{izGrid-methods}}, class "\code{\linkS4class{InclusionZoneGrid}}"
}

%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
