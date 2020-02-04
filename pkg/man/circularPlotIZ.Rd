\name{circularPlotIZ}
\alias{circularPlotIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{circularPlotIZ}}"
}

\description{ This is the generic function for class
  \dQuote{circularPlotIZ}. Please see the associated method in
  \code{\link{circularPlotIZ-methods}} for more details.
}

\usage{
circularPlotIZ(standingTree, plotRadius, ...)
}

\arguments{
  \item{standingTree}{  Signature object of class
  "\code{\linkS4class{standingTree}}".
}

  \item{plotRadius}{  Signature object for plot radius. }

  \item{\dots}{ See methods.}
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above. Please see
  \code{\link{circularPlotIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{circularPlotIZ}}."
}

\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  Class "\code{\linkS4class{circularPlotIZ}}", and \code{\link{circularPlotIZ-methods}}.
}

\examples{
st = standingTree(dbh=50, solidType=4, height=25)
iz.cp = circularPlotIZ(st, 5)
summary(iz.cp)
plot(iz.cp, axes=TRUE, cex=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
