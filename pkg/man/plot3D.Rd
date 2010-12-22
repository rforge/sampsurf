\name{plot3D}
\alias{plot3D}
\alias{plot3D-method}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Interactive 3D plot of objects from package \dQuote{sampSurf} }

\description{
  This routine uses the \code{\link{rgl}} package to display objects of
  class "\code{\linkS4class{Tract}}" or subclass, and
  "\code{\linkS4class{sampSurf}}" objects in a three dimensional
  representation of the surface.
}

\usage{
plot3D(x, ...)
}

\arguments{
  \item{x}{ An object of one of the classes mentioned above, or their
  subclasses, if any.}

  \item{\dots}{ Please see the Details section that follows.}
}

\details{ This is not the generic, that is defined in
  raster::\code{\link[raster:plot3d]{plot3D}} and should be referred to for
  more details on both possible arguments and functionality. Because the
  \code{plot3D} methods in the \dQuote{sampSurf} package are all the
  same in their arguments as the above, they are not detailed elsewhere.  }

\value{ Nothing returned.}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\note{
  One can generate a hardcopy of a plot using the appropriate commands
  in the \code{\link{rgl}} package. }

\seealso{
raster::\code{\link[raster:plot3d]{plot3D}}
}

\examples{
#
# create a buffered tract, sampling surface, and then display...
#
bufftr = bufferedTract(10, Tract(c(x=100,y=100),cellSize=0.5,units='metric'))
ss = sampSurf(25,bufftr,iZone='sausageIZ',plotRadius=4)
require(rgl)
plot3D(ss)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
