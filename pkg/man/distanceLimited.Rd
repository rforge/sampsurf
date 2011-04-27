\name{distanceLimited}
\alias{distanceLimited}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{distanceLimited}}"
}

\description{ This generic function has only one method used as a
constructor function for objects that are of class
"\code{\linkS4class{distanceLimited}}". This method should be used in
preference to \code{\link{new}} to insure a valid object.
}

\usage{
distanceLimited(distanceLimit, ...)
}

\arguments{
  \item{distanceLimit}{ Simply the distance limit in feet or meters,
  depending upon the \code{units} used.
}
  \item{\dots}{ Arguments that are defined in \code{\link{distanceLimited-methods}}
}
}

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{distanceLimited-methods}}.  
}

\value{
A valid object of class "\code{\linkS4class{distanceLimited}}"
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\link{distanceLimited-methods}} and the "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
#
# this will produce a distance limit of 4 feet...
#
(dlsEng = distanceLimited(4, units="English"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
