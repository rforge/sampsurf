\name{antitheticContainer}
\alias{antitheticContainer}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{antitheticContainer}}"
}

\description{ The methods for this function will allow the creation of
  valid "\code{antitheticContainer}" class objects. Please note that it is
  preferable to use the \code{\link{antitheticSampling}} constructor
  method directly with an "\code{\linkS4class{mcsContainer}}" object to
  create a valid collection.  Please see the vignette below for detailed
  examples. See also \code{\link{antitheticContainer-methods}} for
  method details.  }

\usage{
antitheticContainer(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{  The signature object for the generic. }

  \item{\dots}{ Just gobbled presently. }

}

\details{ The vignette below gives examples on creating
  \dQuote{antitheticContainer} objects for each of the Monte Carlo
  subsampling methods available in \pkg{sampSurf} by passing a
  \dQuote{mcsContainer} object to the \code{antitheticSampling}
  constructor. Please use this rather than calling the current
  constructor directly.
}

\value{ A valid object of class "\code{\linkS4class{antitheticContainer}}."
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
  incorrect. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{antitheticSampling}}
}

\examples{
sTrees = standingTrees(5, startSeed = 12)
sTrees.cmc = crudeMonteCarlo(sTrees, n.s = 10)
sTrees.anti = antitheticSampling(sTrees.cmc)
sTrees.anti
print(sTrees.anti@stats, digits = 4) #compare with cmc below
print(sTrees.cmc@stats, digits = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
