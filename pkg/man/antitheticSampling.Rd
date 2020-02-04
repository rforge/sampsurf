\name{antitheticSampling}
\alias{antitheticSampling}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{antitheticSampling}}"
}

\description{ This generic has two methods, they are used to apply
  antithetic sampling to an individual \code{"MonteCarloSampling"}
  subclass object, or collections of \code{"MonteCarloSampling"}
  objects. See \code{\link{antitheticSampling-methods}} for details.  }

\usage{
antitheticSampling(object, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{object}{ An object that is a subclass of
    "\code{\linkS4class{MonteCarloSampling}}". }

  \item{\dots}{ Just gobbled at present.}
}

\details{
  This method samples points on the bole antithetic to those found in
  the \code{object} argument. Note that one can \emph{not} pass arguments on to
  the proxy. All of the information from the \code{object} passed is
  used to determine the proxy and any specific arguments used by the
  proxy in the creation of that object. The constructor functions do not
  allow any of this to be changed by the user.
}

\value{
A valid object of class "\code{\linkS4class{antitheticSampling}}" or
"\code{\linkS4class{antitheticContainer}}", depending on which method
was used.
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

\seealso{See \code{\link{antitheticSampling-methods}} for methods.
  Generics for Monte Carlo methods for the \code{object} are: 
  \code{\link{crudeMonteCarlo}} 
  \code{\link{importanceSampling}},
  \code{\link{controlVariate}}.
}

\examples{
#
# estimate volume between 10 and 15 m, using 5 random heights...
#
sTree = standingTree(dbh = 40, topDiam = 0, height = 20, solidType = 2.8)
sTree.cmc = crudeMonteCarlo(sTree, n.s = 5, segBnds = c(10,15), startSeed = 114)
sTree.anti = antitheticSampling(sTree.cmc)
sTree.anti
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
