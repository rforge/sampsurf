\name{taperInterpolate}
\alias{taperInterpolate}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{ Interpolate Diameters or Lengths in a
  \code{"\linkS4class{Stem}"} Object }

\description{ Taper information is stored in a data frame within the
  \code{taper} slot of a \code{"\linkS4class{downLog}"} or
  \code{"\linkS4class{standingTree}"} object. This function lets one
  interpolate diameters at given lengths/heights, or vice versa from the
  taper data. This is done using either the built-in taper function or
  splines, depending on the value of the \code{solidType} slot in the
  object.  Please see the methods in
  \code{\link{taperInterpolate-methods}} for more details.  }

\usage{
taperInterpolate(object, ...) }

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ See methods for the possibilites of this signature argument. }
  
  \item{\dots}{ Just gobbled for now. }
  
}

\details{ Please note that the routine will throw an error if the points
  you select for interpolation are not found in the stem.

  If \code{solidType = NULL} then the user has supplied their own taper
  data, and splines are used for the interpolation. Otherwise, the
  built-in taper function is used since the taper data were generated
  from it.
}

\value{ A numeric vector with the interpolated diameters or lengths/heights.}


\author{ Jeffrey H. Gove
}

\seealso{ \code{"\linkS4class{downLog}"},
  \code{"\linkS4class{standingTree}"}, \emph{The Stem Class} vignette.}

\examples{
dl = downLog(buttDiam=12, topDiam=2, logLen=10, units="English")
dl@taper
taperInterpolate(dl, pts=c(2.6, 4.1, 7.2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
