\name{circularPlotIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{circularPlotIZ-class}
\alias{area,circularPlotIZ-method}
%\alias{izGrid,circularPlotIZ,Tract-method}
%\alias{perimeter,circularPlotIZ-method}
%\alias{plot,circularPlotIZ,missing-method}
\alias{summary,circularPlotIZ-method}

\title{Class \code{"circularPlotIZ"}}

\description{This class holds the inclusion zone definition for the
fixed-area circular plot method for sampling standing trees.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("circularPlotIZ",
  ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{circularPlotIZ}} to create new objects.
}

\section{Slots}{
  
  This class adds one slot to the "standingTreeIZ" class specification,
  please see the superclasses (see below) for more definitions.
  
  \describe{
	
    \item{\code{circularPlot}:}{Object of class \code{"circularPlot"}:
	  An object from the "\code{\linkS4class{ArealSampling}}" subclass
	  "\code{\linkS4class{circularPlot}}". Please see the help for this
	  class for more information on the slots associated with circular
	  plot objects. }
	
 }
}

\section{Extends}{
Class \code{"\linkS4class{standingTreeIZ}"}, directly.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "standingTreeIZ", distance 2.
}

\section{Methods}{
  \describe{
    \item{area}{\code{signature(x = "circularPlotIZ")}: Return the area
      of the circular plot inclusion zone. }
	
    \item{izGrid}{\code{signature(izObject = "circularPlotIZ", tract =
      "Tract")}: Create the raster grid object associated with the inclusion
    zone. }
  
    \item{perimeter}{\code{signature(object = "circularPlotIZ")}: Return
      the object perimeter as a SpatialPolygons object. }
  
    \item{plot}{\code{signature(x = "circularPlotIZ", y = "missing")}:
      Plot the object. }
	
    \item{summary}{\code{signature(object = "circularPlotIZ")}: Summary
	 report for the object. }
	 }
   }
   

\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{standingTreeIZs}} container class
}

\examples{
showClass("circularPlotIZ")
}
\keyword{classes}
