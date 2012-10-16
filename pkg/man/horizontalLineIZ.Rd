\name{horizontalLineIZ}
\alias{horizontalLineIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{horizontalLineIZ}}"
}

\description{ This is the generic function for class
  \dQuote{horizontalLineIZ}. Please see the associated method in
  \code{\link{horizontalLineIZ-methods}} for more details.
}

\usage{
horizontalLineIZ(standingTree, angleGauge, lineLength, ...)
}

\arguments{
  
  \item{standingTree}{ Signature object of class
  "\code{\linkS4class{standingTree}}".
}

  \item{angleGauge}{ Signature object of class
    "\code{\linkS4class{angleGauge}}". }

  \item{lineLength}{ The length of the line segment in the appropriate
	units (feet for \dQuote{English}, meters for \dQuote{metric}).  }

  \item{\dots}{ See methods. }

}


\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above. Please see
  \code{\link{horizontalLineIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{horizontalLineIZ}}."
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
Class "\code{\linkS4class{horizontalLineIZ}}", and \code{\link{horizontalLineIZ-methods}}.
}
\examples{
st = standingTree(dbh=25, solidType=2.5, height=15) #metric
ag = angleGauge(baf=4) #metric
hls.iz = horizontalLineIZ(st, angleGauge=ag, lineLength=50, orientation=345)
summary(hls.iz)
plot(hls.iz, axes=TRUE, cex=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
