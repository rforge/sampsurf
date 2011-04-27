\name{taperInterpolate}
\alias{taperInterpolate}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Interpolate Diameters or Lengths in a
  \code{"\linkS4class{downLog}"} Object }

\description{ Taper information is stored in a data frame within the
  \code{taper} slot of a \dQuote{downLog} object. This function lets one
  interpolate diameters at given lengths, or vice versa from the taper
  data. This is done using either the built-in taper function or
  splines, depending on the value of the \code{solidType} slot in the object.
}

\usage{
taperInterpolate(dlog, whichSense = c("diameter", "length"), pts = NULL, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlog}{ A valid object of class \dQuote{downLog}
}
  \item{whichSense}{ Either "diameter" to interpolate diameters, or
	"length" to interpolate lengths.
}
  \item{pts}{ If \code{whichSense = "diameters"} then this is a vector
	of lengths at which to interpolate the diameters. Alternatively, if
	\code{whichSense = "lengths"} then this is a vector of diameters at
	which to interpolate the lengths.
}
  \item{\dots}{ Just gobbled for now. }
}

\details{ Please note that the routine will throw an error if the points
  you select for interpolation are not found in the log.
}

\value{ A numeric vector with the interpolated diameters or lengths.}


\author{ Jeffrey H. Gove
}

\seealso{ \code{"\linkS4class{downLog}"}
}
\examples{
dl = downLog(buttDiam=12, topDiam=2, logLen=10, units="English")
dl@taper
taperInterpolate(dl, pts=c(2.6, 4.1, 7.2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
