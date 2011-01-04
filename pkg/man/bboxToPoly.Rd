\name{bboxToPoly}
\alias{bboxToPoly}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Function to Convert an Object's bbox to a
  \dQuote{SpatialPolygons} Object}

\description{ This simply grabs the \code{bbox} specification for an
  object in the \pkg{sampSurf} package, if available, and translates it
  into a spatial polygon object of class
  "\code{\linkS4class{SpatialPolygons}}". 
}

\usage{
bboxToPoly(object, ...)
}
\arguments{
  \item{object}{ Some object with a valid \code{bbox} slot from one of
	the classes in \pkg{sampSurf}.}
  \item{\dots}{ Black hole for everything else.}
}

\details{ One can plot the result of this function to quickly get an
  idea of the overall spatial extent of an object. In many cases the
  result from this object is the same as that returned by
  \code{\link{perimeter}}, but not always; e.g., see the example below.
  }

\value{
  A "\code{\linkS4class{SpatialPolygons}}" object that can be plotted directly.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\examples{
cp = circularPlot(10, center=c(x=20,y=20))
cp.perim = perimeter(cp)
plot(cp.perim, axes=TRUE, lty='dashed')              #the perimeter of the plot
plot(bboxToPoly(cp.perim), add=TRUE, border='blue')  #the bounding box
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
