\name{controlVariate}
\alias{controlVariate}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{controlVariate}}"
}

\description{
  This generic has two methods, they are used to apply control variate
  subsampling to an individual \code{"Stem"} object, or
  collections of \code{"Stem"} objects. See
  \code{\link{controlVariate-methods}} for details.
}

\usage{
controlVariate(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ This is the signature argument, see the
  \code{\link{controlVariate-methods}} for possible values.
}

  \item{\dots}{ Arguments that can be passed along to the proxy function.}
}

\details{
  Control variate sampling uses the differences between measured and
  proxy model cross-sectional areas to estimate the volume of a bole
  section. Control variate sampling is sensitive to departures of the
  proxy from the true bole, and can actually produce negative
  volumes. See the examples in the vignette reference below for examples
  illustrating this phenomenon.
}

\value{ A valid object of class "\code{\linkS4class{controlVariate}}" or
"\code{\linkS4class{mcsContainer}}", depending on which method was used.
}

\references{
  \describe{
%
  \item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
    \pkg{sampSurf}. Package vignette.}
  }
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{See \code{\link{controlVariate-methods}} for methods.
  Other similar generics for Monte Carlo methods include: 
  \code{\link{crudeMonteCarlo}},
  \code{\link{importanceSampling}},
  \code{\link{antitheticSampling}}. 
}
\examples{
#
# estimate volume between 10 and 15 m, using 5 random heights...
#
sTree = standingTree(dbh = 40, topDiam = 0, height = 20, solidType = 2.8)
sTree.cv = controlVariate(sTree, n.s = 5, segBnds = c(10,15), startSeed = 114,
           proxy = 'wbProxy', solidTypeProxy = 2.5, truncateProxyStem = FALSE)
sTree.cv
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
