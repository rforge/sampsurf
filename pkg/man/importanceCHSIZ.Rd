\name{importanceCHSIZ}
\alias{importanceCHSIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{importanceCHSIZ}}"
}

\description{ This is the generic function for class
  \dQuote{importanceCHSIZ}. Please see the associated method in
  \code{\link{importanceCHSIZ-methods}} for more details.
}

\usage{
importanceCHSIZ(standingTree, angleGauge, ...)
}

\arguments{
  \item{standingTree}{  Signature object of class
  "\code{\linkS4class{standingTree}}". }

  \item{angleGauge}{ Signature object of class "\code{\linkS4class{angleGauge}}". }

  \item{\dots}{ See methods. }

}

\details{ Since only one method exists for this generic, its signature
  arguments coincide with the above. Please see
  \code{\link{importanceCHSIZ-methods}} for more details.
}

\value{
  A valid object of class "\code{\linkS4class{importanceCHSIZ}}."
}

\author{
  Jeffrey H. Gove 
}

\seealso{
Class "\code{\linkS4class{importanceCHSIZ}}", and \code{\link{importanceCHSIZ-methods}}.
}

\examples{
st = standingTree(dbh=50, solidType=4, height=25)
ag = angleGauge(baf=4)
iz.ichs = importanceCHSIZ(st, ag)
summary(iz.ichs)
plot(iz.ichs, axes=TRUE, cex=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
