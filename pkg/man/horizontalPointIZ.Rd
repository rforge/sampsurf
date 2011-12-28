\name{horizontalPointIZ}
\alias{horizontalPointIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{horizontalPointIZ}}"
}

\description{ This is the generic function for class
  \dQuote{horizontalPointIZ}. Please see the associated method in
  \code{\link{horizontalPointIZ-methods}} for more details.
}

\usage{
horizontalPointIZ(standingTree, angleGauge, ...)
}

\arguments{
  \item{standingTree}{ Signature object of class
  "\code{\linkS4class{standingTree}}".
}

  \item{angleGauge}{ Signature object of class "\code{\linkS4class{angleGauge}}".
}

  \item{\dots}{ See methods. }
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above. Please see
  \code{\link{horizontalPointIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{horizontalPointIZ}}."
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
Class "\code{\linkS4class{horizontalPointIZ}}", and \code{\link{horizontalPointIZ-methods}}.
}

\examples{
st = standingTree(dbh=50, solidType=4, height=25)
ag = angleGauge(baf=4)
iz.hps = horizontalPointIZ(st, ag)
summary(iz.hps)
plot(iz.hps, axes=TRUE, cex=2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
