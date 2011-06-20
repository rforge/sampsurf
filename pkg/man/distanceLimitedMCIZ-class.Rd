\name{distanceLimitedMCIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{distanceLimitedMCIZ-class}
%\alias{izGrid,distanceLimitedMCIZ,Tract-method}
%\alias{perimeter,distanceLimitedMCIZ-method}
%\alias{plot,distanceLimitedMCIZ,missing-method}
\alias{summary,distanceLimitedMCIZ-method}

\title{Class \code{"distanceLimitedMCIZ"}}

\description{This class holds the inclusion zone definition for the
\sQuote{distance limited Monte Carlo sampling} method for sampling down
coarse woody debris.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("distanceLimitedMCIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{distanceLimitedMCIZ}} to create new objects.
}

\section{Slots}{

  This class is a direct descendent (subclass) of
  \code{"distanceLimitedIZ"} and adds no new slots to that
  definition. Please see the \code{"\linkS4class{distanceLimitedIZ}"}
  class definition for details.

}

\section{Extends}{
Class \code{"\linkS4class{distanceLimitedIZ}"}, directly.\cr
Class \code{"\linkS4class{downLogIZ}"}, by class "distanceLimitedIZ", distance 2.\cr
Class \code{"\linkS4class{dlsIZNull}"}, by class "distanceLimitedIZ", distance 2.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "distanceLimitedIZ", distance 3.\cr
}

\section{Methods}{

  See other methods as defined for the direct superclass.
  
  \describe{
    \item{izGrid}{\code{signature(izObject = "distanceLimitedMCIZ",
		tract = "Tract")}:  \dQuote{InclusionZoneGrid} constructor }
  }
}  

\references{
A vignette comparing the different variants is available from the
author.

%\describe{
%
%\item{}{
  Gove, J. H., Ducey, M. J. and Valentine, H. T. 2012. A distance
  limited method for sampling downed coarse woody debris. \emph{In Prep.} %}
%}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("distanceLimitedMCIZ")
}
\keyword{classes}
