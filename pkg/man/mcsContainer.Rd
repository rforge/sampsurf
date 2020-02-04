\name{mcsContainer}
\alias{mcsContainer}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{mcsContainer}}"
}

\description{ The methods for this function will allow the creation of
  valid "\code{mcsContainer}" class objects. Please note that it is
  preferable to use the constructors named after the Monte Carlo method
  being used, rather than calling this function directly. For example,
  to use importance sampling, call the \code{\link{importanceSampling}}
  constructor with a container of "\code{\linkS4class{Stem}}" subclass
  objects. Please see the vignette below for detailed examples. See also
  \code{\link{mcsContainer-methods}} for method details. }

\usage{
mcsContainer(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ The signature object for the generic. }

  \item{\dots}{ Just gobbled presently. }

}

\details{ The vignette below gives examples on creating "mcsContainer"
  objects for each of the Monte Carlo subsampling methods available in
  \pkg{sampSurf}. The respective methods are simpler to use and more
  intuitive, and their use will ensure that a valid container object is
  returned.  }

\value{A valid object of class "\code{\linkS4class{mcsContainer}}."
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

\note{ Please note that this is not a completely functional container
  class in the traditional sense at present as it does not have
  replacement, deletion, or addition functions. If you need to do any of
  these operations, perform them on the \code{list} object (in the
  \code{mcsObjs} slot) and then recreate the container. If the object is
  not re-built after, e.g., deletion, the summary statistics will be
  incorrect.  }

\seealso{ Please see the direct constructors:
  \code{\link{crudeMonteCarlo}}, \code{\link{importanceSampling}} and
  \code{\link{controlVariate}} for methods to create collections of
  objects under these subsampling schemes. For antithetic versions of
  these methods, see the \code{\linkS4class{antitheticContainer}} class.
}

\examples{
sTrees = standingTrees(5, startSeed = 12)
sTrees.cmc = crudeMonteCarlo(sTrees, n.s = 10)
sTrees.cmc
print(sTrees.cmc@stats, digits = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
