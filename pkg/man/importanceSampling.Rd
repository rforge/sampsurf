\name{importanceSampling}
\alias{importanceSampling}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{importanceSampling}}"
}

\description{
  This generic has five methods, they are used to apply importance
  (sub) sampling to an individual \code{"Stem"} object, or
  collections of \code{"Stem"} objects. See
  \code{\link{importanceSampling-methods}} for details. 
}

\usage{
importanceSampling(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ This is the signature argument, see the
  \code{\link{importanceSampling-methods}} for possible values. }

  \item{\dots}{ Arguments that can be passed along to the proxy function.}
}

\details{ Briefly, with importance sampling for bole volume (or some
segment of the bole) one uses a proxy taper function from which to draw
samples and thereby concentrate the samples in the lower portion of the
bole, where there is more volume and measurements are easier. The
diferent built-in proxy functions and their use are detailed in the
vignette cited below. In addition, one can supply one's own proxy
function if desired.  }

\value{ A valid object of class
"\code{\linkS4class{importanceSampling}}" or
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


\seealso{
See \code{\link{importanceSampling-methods}} for methods.
  Other similar generics for Monte Carlo methods include: 
  \code{\link{crudeMonteCarlo}},
  \code{\link{controlVariate}},
  \code{\link{antitheticSampling}}.}

\examples{
#
# estimate volume between 10 and 15 m, using 5 random heights...
#
sTree = standingTree(dbh = 40, topDiam = 0, height = 20, solidType = 2.8)
sTree.is = importanceSampling(sTree, n.s = 5, segBnds = c(10,15), startSeed = 114,
           proxy = 'wbProxy', solidTypeProxy = 2.5)
sTree.is
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
