\name{horizontalPointCVIZ}
\alias{horizontalPointCVIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{horizontalPointCVIZ}}"
}

\description{
  This generic is the base constructor for creating objects of class
  \dQuote{\code{horizontalPointCVIZ}}. Please see the associated
   \code{\link{horizontalPointCVIZ-methods}} method for more details.
}

\usage{
horizontalPointCVIZ(standingTree, angleGauge, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{standingTree}{ Signature object of class
	"\code{\linkS4class{standingTree}}". }

  \item{angleGauge}{
   Signature object of class "\code{\linkS4class{angleGauge}}". }

  \item{\dots}{ See associated method. }

}

\details{
  It is very important to note both here and in the method documentation
  that the call to this constructor is the appropriate place to include any
  extra arguments that should be passed on to the
  \code{\link{controlVariate}} constructor for control of subsampling
  within the inclusion zone. In addition, different proxy functions can
  have extra arguments associated with them beyond the three required
  ones. Again, this is the place to pass any of these extra arguments to
  the proxy functions. Please see \code{\link{getProxy}} for
  documentation on the built-in proxy functions, and note the different
  arguments associated with each function.
}

\value{
  A valid object of class \dQuote{\code{horizontalPointCVIZ}}.
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
  this object other than those used to control the CV subsampling
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
  \code{\linkS4class{horizontalPointCMCIZ}},
  \code{\linkS4class{horizontalPointISIZ}},
  \code{\linkS4class{criticalHeightIZ}},
  \code{\linkS4class{importanceCHSIZ}},
  \code{\linkS4class{antitheticICHSIZ}},
  \code{\linkS4class{pairedAICHSIZ}}.
}

\examples{
#
# restrict CV sampling between 10-20m height and use the
# "wbProxy" function with solid type = 0.9*4 (close to true taper);
# with 2 subsamples to be taken on the tree at each grid cell...
#
st = standingTree(dbh=50, solidType=4, height=25)
ag = angleGauge(baf=4)
cvhps.iz = horizontalPointCVIZ(st, ag, segBnds=c(10,20),
           proxy='wbProxy', solidTypeProxy=0.9, n.s=2)
summary(cvhps.iz)
plot(cvhps.iz)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
