\name{crudeMonteCarlo}
\alias{crudeMonteCarlo}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{crudeMonteCarlo}}"
}

\description{
  This generic has two methods, they are used to apply crude
  Monte Carlo subsampling to an individual \code{"Stem"} object, or
  collections of \code{"Stem"} objects. See
  \code{\link{crudeMonteCarlo-methods}} for details.  }

\usage{
crudeMonteCarlo(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ This is the signature argument, see the
  \code{\link{crudeMonteCarlo-methods}} for possible values. }

  \item{\dots}{ Arguments that can be passed along to the proxy function.}
}

\details{
  Crude Monte Carlo is arguably the simplest method for estimating a
  volume integral within a segment of a stem bole. The equations used in
  estimation are given in the reference below, which also points to more
  detailed references. Essentially, the crude Monte Carlo method
  extracts one or more heights uniformly at random, at which
  cross-sectional areas are determined. From these, a volume estimate
  can be made for each subsampled height. 
}

\value{ A valid object of class "\code{\linkS4class{crudeMonteCarlo}}"
or "\code{\linkS4class{mcsContainer}}", depending on which method was
used.  }

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

\seealso{See \code{\link{crudeMonteCarlo-methods}} for methods.
  Other similar generics for Monte Carlo methods include: 
  \code{\link{importanceSampling}},
  \code{\link{controlVariate}},
  \code{\link{antitheticSampling}}. 
}

\examples{
#
# estimate volume between 10 and 15 m, using 5 random heights...
#
sTree = standingTree(dbh = 40, topDiam = 0, height = 20, solidType = 2.8)
sTree.cmc = crudeMonteCarlo(sTree, n.s = 5, segBnds = c(10,15), startSeed = 114)
sTree.cmc
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
