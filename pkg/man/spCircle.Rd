\name{spCircle}
\alias{spCircle}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Create a Circular Spatial Object }

\description{
This function simply creates a circular polygon object that is of class
"\code{\linkS4class{SpatialPolygons}}". It also creates a
"\code{\linkS4class{SpatialPoints}}" object holding the center point of
the circle. 
}

\usage{
spCircle(radius,
         spUnits = CRS(projargs = as.character(NA)),
         centerPoint = c(x = 0, y = 0),
         nptsPerimeter = 100,
         spID = paste("circle", .StemEnv$randomID(), sep = ":"),
         ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{radius}{ The radius of the circle in the appropriate units.}

  \item{spUnits}{ Object of class \code{"CRS"}: A legal proj.4
	  coordinate system; the default is to have user-defined
	  coordinates.}

  \item{centerPoint}{ The circle's center location in the appropriate
        units.  This is a vector of length two with names "x" and "y". }
	
  \item{nptsPerimeter}{ The number of points forming the perimeter of
	the polygon. }

  \item{spID}{  A unique identifier that will be used in
	    displaying the spatial polygon. }

  \item{\dots}{ Not used currently. }
}

\details{ The polygon created for the circle will always have the
  individual internal "\code{\linkS4class{Polygon}}" object named
  \sQuote{\code{pgCircle}}, while the internal
  "\code{\linkS4class{Polygons}}" object will be named
  \sQuote{\code{pgsCircle}}. These may need to be renamed in the
  returned object to make more sense. See the \pkg{sp} package for more
  details.  }

\value{

  A list with the following components\ldots
  
  \item{spCircle }{ The "\code{\linkS4class{SpatialPolygons}}" polygon
	object.}
  
  \item{location }{The "\code{\linkS4class{SpatialPoints}}" point
	object.}
}



\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
The \code{\link{standingTree}} constructor.
}
\examples{
dbh = 20
sp.dbh = spCircle(dbh/2, centerPoint=c(x=30,y=80), spID='tree.1') 
plot(sp.dbh$spCircle)
plot(sp.dbh$location, add=TRUE, pch=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
