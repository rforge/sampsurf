\name{fullChainSawIZ}
\alias{fullChainSawIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{fullChainSawIZ}}"
}

\description{ This is the generic function for class
  \dQuote{fullChainSawIZ}. Please see the associated method in
  \code{\link{fullChainSawIZ-methods}} for more details on how to
  construct objects of this class.
}

\usage{
fullChainSawIZ(downLog, plotRadius, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{downLog}{ Signature object of class
	"\code{\linkS4class{downLog}}". }

  \item{plotRadius}{ Signature object for plot radius. }

  \item{\dots}{ See methods.}
}

\details{
The actual inclusion zone is the same as that for
"\code{\linkS4class{sausageIZ}}." See the reference below for more
details on how the actual sampling method works. Also \emph{The
  \dQuote{InclusionZone} Class} and \emph{The \dQuote{InclusionZoneGrid} Class}
vignettes provide examples.
}

\value{
A valid object of class "\code{\linkS4class{fullChainSawIZ}}."
}

\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry} \bold{84}:109--117.\cr
}

\author{
Jeffrey H. Gove 
}

\note{ Note in particular that the \code{puaEstimates} slot is not valid
  in the sense that there is no one estimate for each attribute under
  this method as the surface will be variable from point to point;
  therefore, the attributes in this slot will all be assign \code{NA} by
  the constructor.  }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  Class "\code{\linkS4class{fullChainSawIZ}}", and \code{\link{fullChainSawIZ-methods}}.
}
\examples{
dl = downLog(buttDiam=40, solidType=4, logAngle=4*pi/3, logLen=6)
iz.fcs = fullChainSawIZ(dl, plotRadius=3)
summary(iz.fcs)
plot(iz.fcs, axes=TRUE, showPlotCenter=TRUE, cex=2, showNeedle=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
