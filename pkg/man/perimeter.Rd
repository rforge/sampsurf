\name{perimeter}
\alias{perimeter}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Function to Return the Graphical Perimeter of an Object in
  Package \sQuote{sampSurf} }

\description{ Most classes in the \pkg{sampSurf} package have some kind
  of spatial representation that conforms to a class in \pkg{sp} (for
  polygons) or \pkg{raster} (for grids). This generic has been defined
  to return graphical polygon object that most nearly matches the
  perimeter. For some objects this means returning the bounding box for,
  perhaps, a collection of logs, or for a \dQuote{Tract} object.
}

\usage{
perimeter(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{  Signature object, which differs for each method.}

  \item{\dots}{ See methods. }
}

\details{ The methods defined for this generic are described in
  \code{\link{perimeter-methods}}. The function is quite simple, and
  works essentially the same for each type of object. Again, some leeway
  in exactly what is returned is taken because we can have individual
  objects, collections, or grid rather than polygonal objects. In the
  latter two cases, the perimeter normally would be the minimal bounding
  box. For other objects in classes that have a well-defined perimeter,
  such as a downLog, or a circular plot, these are returned. One can
  always plot their bounding box separately with the help of
  \code{link{bboxToPoly}}.  }

\value{
  A "\code{\linkS4class{SpatialPolygons}}" object that can be plotted directly.
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\link{bbox}}
}
\examples{
showMethods("perimeter")
dlogs = downLogs(15, xlim=c(0,20), ylim=c(0,20), buttDiams=c(25,35))
dlogs.perim = perimeter(dlogs)
plot(dlogs.perim, axes=TRUE)
plot(dlogs, add=TRUE)
bbox(dlogs.perim)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
