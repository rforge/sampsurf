\name{perpendicularDistance}
\alias{perpendicularDistance}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{perpendicularDistance}}"
}

\description{ This generic function has only one method used as a
constructor function for objects that are of class
"\code{\linkS4class{perpendicularDistance}}". This method should be used in
preference to \code{\link{new}} to insure a valid object.
}

\usage{
perpendicularDistance(kpds, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kpds}{ This is the signature argument: the \emph{Kpds} factor in
	the appropriate units.
}
  \item{\dots}{ Arguments that are defined in \code{\link{perpendicularDistance-methods}}
  }
  }

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{perpendicularDistance-methods}}.
}

\value{
A valid object of class "\code{\linkS4class{perpendicularDistance}}"
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\link{perpendicularDistance-methods}} and the "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
#
# this will have a volume factor of 5445 cubic feet...
#
(pdsEng = perpendicularDistance(4, units="English"))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
