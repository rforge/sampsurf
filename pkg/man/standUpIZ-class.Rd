\name{standUpIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{standUpIZ-class}
%\alias{izGrid,standUpIZ,Tract-method}
%\alias{perimeter,standUpIZ-method}
%\alias{plot,standUpIZ,missing-method}
\alias{summary,standUpIZ-method}

\title{Class "standUpIZ"}

\description{ This class holds the inclusion zone definition for the
\dQuote{stand-up} method (Gove and Van Deusen, 2011) for sampling down
coarse woody debris.  }

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("standUpIZ", ...)}. As usual in this class, however, this
is not recommended because the objects are fairly complex. Instead, one
can use the object constructor \code{\link{standUpIZ}} to create new
objects.  }

\section{Slots}{
  Most of the slots are described in the superclasses (see below),
  please see their help pages for more information. Essentially, this
  class adds only one slot to the "downLogIZ" class specification.
  
  \describe{
	
    \item{\code{circularPlot}:}{Object of class \code{"circularPlot"}:
	  An object from the  "\code{\linkS4class{ArealSampling}}" subclass
	  "\code{\linkS4class{circularPlot}}". }
  }
  }

\section{Extends}{
Class \code{"\linkS4class{downLogIZ}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "downLogIZ", distance 2.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "standUpIZ", tract =
      "Tract")}: \dQuote{InclusionZoneGrid} generic constructor } 
    \item{perimeter}{\code{signature(object = "standUpIZ")}: Return the
      object perimeter }
    \item{plot}{\code{signature(x = "standUpIZ", y = "missing")}: Plot
      the object  }
    \item{summary}{\code{signature(object = "standUpIZ")}: Object summary }
	 }
}

\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. (Accepted).\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\linkS4class{chainSawIZ}}, \code{\linkS4class{sausageIZ}}; and
  the \code{\linkS4class{downLogIZs}} container class.
}

\examples{
showClass("standUpIZ")
}
\keyword{classes}
