\name{pointRelascope}
\alias{pointRelascope}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{pointRelascope}}"
}

\description{This generic function has only one method used as a
constructor function for objects that are of class
"\code{\linkS4class{pointRelascope}}". This method should be used in
preference to \code{\link{new}} to insure a valid object.
}

\usage{
pointRelascope(angleDegrees, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{angleDegrees}{ This is the signature argument: the relascope
	angle in degrees.
}

  \item{\dots}{ Arguments that are defined in \code{\link{pointRelascope-methods}}
}
}

\details{
Only one method currently exists for object generation. Its arguments
are documented in \code{\link{pointRelascope-methods}}.
}

\value{
A valid object of class "\code{\linkS4class{pointRelascope}}"
}


\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}



\seealso{
\code{\link{pointRelascope-methods}}, the "\code{\linkS4class{ArealSampling}}" class.

}

\examples{
#
#  create an object with a with 4:1 reach:width factor angle...
#
(angle = .StemEnv$rad2Deg(2*atan(1/4)))
prs = pointRelascope(angle, units='English')
prs
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
