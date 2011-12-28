\name{angleGauge}
\alias{angleGauge}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{angleGauge}}"
}

\description{ This generic function has only one method used as a
constructor function for objects that are of class
"\code{\linkS4class{angleGauge}}". This method should be used in
preference to \code{\link{new}} to insure a valid object.
}

\usage{
angleGauge(baf, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baf}{ The basal area factor of the angle gauge (prism) in the
	appropriate \code{units}. }

  \item{\dots}{ See the methods for other arguments. }
}

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{angleGauge-methods}}.
}

\value{
A valid object of class "\code{\linkS4class{angleGauge}}"
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\link{angleGauge-methods}} and the "\code{\linkS4class{ArealSampling}}" class.
}

\examples{
#
#  create an object with an English 10 baf angle gauge...
#
ag = angleGauge(10, units='English')
ag
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
