\name{perpendicularDistanceIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{perpendicularDistanceIZ-class}
%\alias{izGrid,perpendicularDistanceIZ,Tract-method}
%\alias{perimeter,perpendicularDistanceIZ-method}
%\alias{plot,perpendicularDistanceIZ,missing-method}
\alias{summary,perpendicularDistanceIZ-method}

\title{Class \code{"perpendicularDistanceIZ"}}

\description{This class holds the inclusion zone definition for the
\sQuote{perpendicular distance sampling} method (Williams & Gove, 2003) for sampling down
coarse woody debris.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("perpendicularDistanceIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{perpendicularDistanceIZ}} to create new objects.
}

\section{Slots}{
  
  Most of the slots are described in the superclasses (see below),
  please see their help pages for more information. This class adds a
  few slots to the "downLogIZ" class specification.
    
  \describe{
    \item{\code{pds}:}{Object of class \code{"perpendicularDistance"}:
      This supplies the information for the \emph{Kpds} factor, etc. for
      establishing the inclusion zone. }
  
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

    \item{\code{pdsType}:}{Object of class \code{"character"}: Specifies
      the type of perpendicular distance sampling used with regard to
      the selection (i.e., probability proportional to\ldots) of the
      log. It can be one of "\code{volume}", "\code{surfaceArea}" or
      "\code{coverageArea}". (See also \code{.StemEnv$pdsTypes} for
      legal types.)  }

  }
}

\section{Extends}{
Class \code{"\linkS4class{downLogIZ}"}, directly.\cr
Class \code{"\linkS4class{pdsIZNull}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "downLogIZ", distance 2.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "perpendicularDistanceIZ",
		tract = "Tract")}: \dQuote{InclusionZoneGrid} constructor }
	
    \item{perimeter}{\code{signature(object = "perpendicularDistanceIZ")}: Return the object perimeter }

    \item{plot}{\code{signature(x = "perpendicularDistanceIZ", y = "missing")}: Plot the object }

    \item{summary}{\code{signature(object = "perpendicularDistanceIZ")}: Object summary }
	 }
   }
   
\references{
\describe{
%
\item{}{Williams, M. S. and Gove, J. H. 2003. Perpendicular distance sampling:
  an alternative method for sampling downed coarse woody
  debris. \emph{Canadian Journal of Forest Research}
  \bold{33}:1564--1579.}
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("perpendicularDistanceIZ")
}
\keyword{classes}
