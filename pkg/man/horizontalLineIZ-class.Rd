\name{horizontalLineIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{horizontalLineIZ-class}
%\alias{izGrid,horizontalLineIZ,Tract-method}
%\alias{perimeter,horizontalLineIZ-method}
%\alias{plot,horizontalLineIZ,missing-method}
\alias{summary,horizontalLineIZ-method}

\title{Class \code{"horizontalLineIZ"}}

\description{This class holds the inclusion zone definition for the
horizontal line sampling method for standing trees.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("horizontalLineIZ", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{horizontalLineIZ}} to create new objects.
}

\section{Slots}{
  
  This class adds several slots to the
  "\code{\linkS4class{standingTreeIZ}}" base class specification, please
  see the superclasses (see below) for more slot definitions.

  \describe{
	
    \item{\code{angleGauge}:}{Object of class \code{"angleGauge"}: A
      subclass object from the "\code{\linkS4class{ArealSampling}}"
      class. Please see the help for the
      "\code{\linkS4class{angleGauge}}" class for more information on
      the slots associated with angle gauge objects. } 
	
    \item{\code{lineSegment}:}{Object of class \code{"lineSegment"}:
      Also a subclass object from the
      "\code{\linkS4class{ArealSampling}}" class. Please see the help
      for the "\code{\linkS4class{lineSegment}}" class for more
      information on the slots associated with line segment objects. }
	
    \item{\code{width}:}{Object of class \code{"numeric"}: This is the
      width of the inclusion zone in the appropriate units (feet for
      \dQuote{English}, meters for \dQuote{metric}). }
	
    \item{\code{length}:}{Object of class \code{"numeric"}: This is the
      length of the inclusion zone (line segment) in appropriate units (feet for
      \dQuote{English}, meters for \dQuote{metric}). }
	
    \item{\code{izPerim}:}{Object of class \code{"matrix"}: A matrix
      representation of the inclusion zone in homogeneous
      coordinates.  This can be manipulated and plotted as desired for
      easy access to the inclusion zone where needed. }
	
    \item{\code{area}:}{Object of class \code{"numeric"}: The area of
      the inclusion zone in the correct units.  }
	
    \item{\code{perimeter}:}{Object of class \code{"SpatialPolygons"}:
      The \code{"SpatialPolygons"} object corresponding to the perimeter of the
      inclusion zone.}

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
Class \code{"\linkS4class{standingTreeIZ}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "standingTreeIZ", distance 2.
}

\section{Methods}{
  \describe{
	
    \item{izGrid}{\code{signature(izObject = "horizontalLineIZ", tract =
		"Tract")}: \dQuote{InclusionZoneGrid} constructor }
	
    \item{perimeter}{\code{signature(object = "horizontalLineIZ")}:
      Return the object perimeter }
	
    \item{plot}{\code{signature(x = "horizontalLineIZ", y = "missing")}:
      Plot the object }
	
    \item{summary}{\code{signature(object = "horizontalLineIZ")}: Object summary }
	 }
}
   
\references{
\describe{
\item{}{T. G. Gregroire and H. T. Valentine. 2009. \emph{Sampling
	strategies for natural resources and the environment}. Chapman and
  Hall/CRC, 496p.}
%
\item{}{\dQuote{\emph{The InclusionZone Class}} vignette.}
}
}

\author{
Jeffrey H. Gove 
}


\seealso{
See the "\code{\linkS4class{standingTreeIZs}}" container class and the
object constructor \code{\link{horizontalLineIZ}} 
}

\examples{
showClass("horizontalLineIZ")
}

\keyword{classes}
