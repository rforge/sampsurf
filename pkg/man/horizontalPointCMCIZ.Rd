\name{horizontalPointCMCIZ}
\alias{horizontalPointCMCIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{horizontalPointCMCIZ}}"
}

\description{
  This generic is the base constructor for creating objects of class
  \dQuote{\code{horizontalPointCMCIZ}}. Please see the associated
   \code{\link{horizontalPointCMCIZ-methods}} method for more details.
}

\usage{
horizontalPointCMCIZ(standingTree, angleGauge, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{standingTree}{ Signature object of class
  "\code{\linkS4class{standingTree}}".
  }

  \item{angleGauge}{
   Signature object of class "\code{\linkS4class{angleGauge}}".
  }

  \item{\dots}{ See associated method.}

} %arguments

\details{
  It is very important to note both here and in the method documentation
  that the call to this constructor is the appropriate place to include any
  extra arguments that should be passed on to the
  \code{\link{crudeMonteCarlo}} constructor for control of subsampling
  within the inclusion zone.
}

\value{
  A valid object of class \dQuote{\code{horizontalPointCMCIZ}}.
}

\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
%
\item{}{Gove, J. H. 2013. The \dQuote{InclusionZone} Class. \pkg{sampSurf} Package vignette.}
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\note{
  Please see the two vignettes cited above for more details on this and
  related classes.

  In the following run, it is possible to look at the contents of the
  \code{mcsObj} slot for the object. Please note that any slot within
  this object other than those used to control the CMC subsampling
  (e.g., \code{segBnds}) will not be used to estimate volume for the
  tree. This object will be used to \emph{control} subsampling at each
  grid cell location within the inclusion zone when applied to the
  \code{\link{izGrid}} constructor. Therefore, while the volume and
  variance in this slot's object are applicable to the tree, they are
  not related to the final estimate that will be derived within
  \code{sampSurf}.
}

\seealso{ Other Monte Carlo methods that work with horizontal point
  sampling include: 
  \code{\linkS4class{horizontalPointISIZ}},
  \code{\linkS4class{horizontalPointCVIZ}},
  \code{\linkS4class{criticalHeightIZ}},
  \code{\linkS4class{importanceCHSIZ}},
  \code{\linkS4class{antitheticICHSIZ}},
  \code{\linkS4class{pairedAICHSIZ}}.
}

\examples{
#
# restrict CMC sampling between 10-20m height, with 2
# subsamples to be taken on the tree at each grid cell...
#
st = standingTree(dbh=50, solidType=4, height=25)
ag = angleGauge(baf=4)
cmchps.iz = horizontalPointCMCIZ(st, ag, segBnds=c(10,20),
            n.s=2)
summary(cmchps.iz)
plot(cmchps.iz)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
