\name{sausageIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{sausageIZ-class}
%\alias{izGrid,sausageIZ,Tract-method}
%\alias{perimeter,sausageIZ-method}
%\alias{plot,sausageIZ,missing-method}
\alias{summary,sausageIZ-method}

\title{Class "sausageIZ"}

\description{This class holds the inclusion zone definition for the
\sQuote{sausage} method (Gove and Van Deusen, 2011) for sampling down
coarse woody debris.
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("sausageIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{sausageIZ}} to create new objects.  }

\section{Slots}{
  Most of the slots are described in the superclasses
  (see below), please see their help pages for more information. This
  class adds a few slots to the "downLogIZ" class specification.

  \describe{
	
    \item{\code{sausage}:}{Object of class \code{"matrix"}: A matrix
      representation of the sausage inclusion zone in homogeneous
      coordinates.  This can be manipulated and plotted as desired for
      easy access to the inclusion zone where needed.  }
	
    \item{\code{radius}:}{Object of class \code{"numeric"}: The radius
      for the fixed-area plot that determines the sausage inclusion zone
      area. This will be in units of feet for "English" or meters for
      "metric." }
	
    \item{\code{area}:}{Object of class \code{"numeric"}: The exact area
      of the inclusion zone.  }
	
    \item{\code{perimeter}:}{Object of class \code{"SpatialPolygons"} This is the
      inclusion zone perimeter as a \dQuote{SpatialPolygons} object.   }
   
    \item{\code{pgSausageArea}:}{Object of class \code{"numeric"}: This
      is the area of the sausage inclusion zone as calculated from the
      polygon in the perimeter slot using the \dQuote{SpatialPolygons}
      object. As such, it is an approximation of the true area of the
      inclusion zone, which is given in the area slot. This just enables
      us to see how close the graphic representation is to the real
      area.  }
   
  }
}

\section{Extends}{
Class \code{"\linkS4class{downLogIZ}"}, directly.
Class \code{"\linkS4class{InclusionZone}"}, by class "downLogIZ", distance 2.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "sausageIZ", tract =
		"Tract")}: \dQuote{InclusionZoneGrid} generic constructor }
	
    \item{perimeter}{\code{signature(object = "sausageIZ")}: Return the
      object perimeter }
	
    \item{plot}{\code{signature(x = "sausageIZ", y = "missing")}: Plot
      the object }
	
    \item{summary}{\code{signature(object = "sausageIZ")}: Object summary }
	 }
   }
   
\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. (Accepted).\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  \code{\linkS4class{standUpIZ}}, \code{\linkS4class{chainSawIZ}}; and
  the \code{\linkS4class{downLogIZs}} container class.
}

\examples{
showClass("sausageIZ")
}
\keyword{classes}
