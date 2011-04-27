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
  
  Most of the slots are described in the superclasses (see below),
  please see their help pages for more information. This class adds a
  few slots to the "downLogIZ" class specification.
  
  \describe{
	
    \item{\code{dls}:}{Object of class \code{"distanceLimited"}: This
      supplies  the distance limit for establishing the inclusion zone. }
  
    \item{\code{izPerim}:}{Object of class \code{"matrix"}: A matrix
      representation of the inclusion zone in homogeneous
      coordinates.  This can be manipulated and plotted as desired for
      easy access to the inclusion zone where needed. }

    \item{\code{area}:}{Object of class \code{"numeric"}: The exact area
      of the inclusion zone.  }

    \item{\code{perimeter}:}{Object of class \code{"SpatialPolygons"}: This is the
      inclusion zone perimeter as a \dQuote{SpatialPolygons} object.}

    \item{\code{pgArea}:}{Object of class \code{"numeric"}: This is
      the area of the  inclusion zone as calculated from the
      polygon in the \code{perimeter} slot using the \dQuote{SpatialPolygons}
      object. As such, it is an approximation of the true area of the
      inclusion zone, which is given in the area slot. This just enables
      us to see how close the graphic representation is to the real
      area, and adjust if necessary the number of points defining the
      inclusion area perimeter. }

   }
}
\section{Extends}{
Class \code{"\linkS4class{downLogIZ}"}, directly.\cr
Class \code{"\linkS4class{dlmcIZNull}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "downLogIZ", distance 2.
}

\section{Methods}{
  
  \describe{
    \item{izGrid}{\code{signature(izObject = "distanceLimitedMCIZ",
		tract = "Tract")}:  \dQuote{InclusionZoneGrid} constructor }
	
    \item{perimeter}{\code{signature(object = "distanceLimitedMCIZ")}:
       Return the object perimeter }
	
    \item{plot}{\code{signature(x = "distanceLimitedMCIZ", y =
		"missing")}: Plot the object }
	
    \item{summary}{\code{signature(object = "distanceLimitedMCIZ")}: Object summary }
	 }
}

\references{
Notes available from the author.
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
