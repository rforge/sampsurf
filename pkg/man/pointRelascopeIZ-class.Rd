\name{pointRelascopeIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{pointRelascopeIZ-class}
%\alias{izGrid,pointRelascopeIZ,Tract-method}
%\alias{perimeter,pointRelascopeIZ-method}
%\alias{plot,pointRelascopeIZ,missing-method}
\alias{summary,pointRelascopeIZ-method}

\title{Class "pointRelascopeIZ"}

\description{This class holds the inclusion zone definition for the
\sQuote{point relascope} method (Gove et. al, 1999) for sampling down
coarse woody debris.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("pointRelascopeIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{pointRelascopeIZ}} to create new objects.
}

\section{Slots}{
  
  Most of the slots are described in the superclasses (see below),
  please see their help pages for more information. This class adds a
  few slots to the "downLogIZ" class specification.
  
  \describe{
    \item{\code{prs}:}{Object of class \code{"pointRelascope"}: This
    supplies the information on the relascope angle, etc., used for
    constructing the inclusion zone. }
	
    \item{\code{dualCircle}:}{Object of class \code{"matrix"}: A matrix
      representation of the dual circle inclusion zone in homogeneous
      coordinates.  This can be manipulated and plotted as desired for
      easy access to the inclusion zone where needed. }
	
    \item{\code{radius}:}{Object of class \code{"numeric"}: The radius
      of each of the dual circles in the inclusion zone. }
	
    \item{\code{area}:}{Object of class \code{"numeric"}: The exact area
      of the inclusion zone.  }
	
    \item{\code{perimeter}:}{Object of class \code{"SpatialPolygons"}: This is the
      inclusion zone perimeter as a \dQuote{SpatialPolygons} object.  }
   
    \item{\code{pgDualArea}:}{Object of class \code{"numeric"}: This is
      the area of the dual circle inclusion zone as calculated from the
      polygon in the perimeter slot using the \dQuote{SpatialPolygons}
      object. As such, it is an approximation of the true area of the
      inclusion zone, which is given in the area slot. This just enables
      us to see how close the graphic representation is to the real
      area, and adjust if necessary the number of points defining the
      inclusion area perimeter. }
   
    \item{\code{dualCenters}:}{Object of class \code{"matrix"} The
      center points of each of the dual circles as a
      \dQuote{SpatialPoints} object. }
    
  }  
}

\section{Extends}{
Class \code{"\linkS4class{downLogIZ}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "downLogIZ", distance 2.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "pointRelascopeIZ", tract =
		"Tract")}: \dQuote{InclusionZoneGrid} generic constructor }
	
    \item{perimeter}{\code{signature(object = "pointRelascopeIZ")}:
      Return the object perimeter }
	
    \item{plot}{\code{signature(x = "pointRelascopeIZ", y = "missing")}:
      Plot the object }
	
    \item{summary}{\code{signature(object = "pointRelascopeIZ")}: Object summary }
	 }
   }
   
\references{
J. H. Gove, A. Ringvall, G. Stahl, and M. J. Ducey. 1999. Point
relascope sampling of downed coarse woody debris. \emph{Canadian Journal
  of Forest Research} \bold{29}(11):1718--1726.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{downLogIZs}} container class
}

\examples{
showClass("pointRelascopeIZ")
}
\keyword{classes}
