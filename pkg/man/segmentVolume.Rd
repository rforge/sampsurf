\name{segmentVolume}
\alias{segmentVolume}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Determine segment volumes within a \code{"\linkS4class{Stem}"}
  Object }

\description{

This generic will allow the determination of segment volumes within
individual \code{"\linkS4class{downLog}"} or
\code{"\linkS4class{standingTree}"} objects. 

}

\usage{
segmentVolume(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{See methods for the possibilites of this signature argument.}

  \item{\dots}{ Just gobbled for now. }
}

\details{
  If the object's \code{solidType} slot is not \code{NULL}, then
  the default taper equation is used to determine the volume based on
  the object's \code{solidType}. Otherwise, the segment volume is
  determined by fitting a spline to the object's taper data. Please see
  the methods for more information on usage: \code{\link{segmentVolume-methods}}

}

\value{
  The scalar volume of the segment chosen.
}


\author{ Jeffrey H. Gove}


\seealso{
The \code{wbVolume} and \code{splineVolume} code within \link{.StemEnv}.
}

\examples{
dl = downLog(buttDiam=12, topDiam=2, logLen=10, units="English")
segmentVolume(dl, segBnds=c(3,8))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
