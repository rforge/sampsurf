\name{sampSurf}
\alias{sampSurf}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{sampSurf}}"
}

\description{ This is the generic definition for generating objects of
class "sampSurf." There are several methods corresponding to this
generic that may be found in \code{\link{sampSurf-methods}}. One of the
constructor methods in particular, averts the necessity for being
familiar with the other classes in the package and can simply be run
from scratch, requiring only an extant "\code{\linkS4class{Tract}}"
object on which to draw the sample and compute the surface.  More
details and examples are found in \emph{"The sampSurf Class"}
vignette.  }

\usage{
sampSurf(object, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ Signature object, which differs for each method.}
  
  \item{tract}{ Signature object of class "\code{\linkS4class{Tract}}"
	or subclass.}
  
  \item{\dots}{ See methods.}
}

\details{ The generation of objects of this class depends on the
  signature of the method used. One common thread is that all methods
  require an existing object of class "\code{\linkS4class{Tract}}" or
  one of its subclasses to be available. This will be the grid area over
  which the population is to be simulated and sampling surface
  determined. It is often convenient to set this area up to be equal to
  one acre or one hectare. However, one could match each "tract" to the
  size of a sample plot where logs or trees have been measured in the
  field, for example. And there is no reason why the tract must be
  restricted to a smaller size, except for the limitations of computer
  memory. The \pkg{raster} package routines should help with this
  because they will store large rasters (tracts) on disk, but I have not
  had the chance to test this out sufficiently yet. In any case, one can
  start from scratch and make all of the intervening objects necessary
  to create a sampling surface with the appropriate constructor
  method. Alternatively, one can bypass all the intermediate steps and
  use the constructor that takes the number of individual stems and a
  tract object, and let it do all of the work. These are both explained
  in the vignette sited above and in the \code{\link{sampSurf-methods}}.  }

\value{
A valid object of class "\code{\linkS4class{sampSurf}}."
}

\references{
  \emph{"The sampSurf Class"} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\link{sampSurf-package}}, \code{\link{sampSurf-class}}, \code{\link{sampSurf-methods}}
}

\examples{
#
#  create a sampling surface with 3 logs using the "sausage" method...
#
tr = Tract(c(x=50,y=50), cellSize=0.5)
btr = bufferedTract(10, tr)
ssSA = sampSurf(3, btr, iZone = 'sausageIZ', plotRadius=3,
       buttDiam=c(30,50), startSeed=1001)
plot(ssSA, axes=TRUE)
summary(ssSA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
