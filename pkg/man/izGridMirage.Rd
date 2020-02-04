\name{izGridMirage}
\alias{izGridMirage}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{mirageInclusionZoneGrid}}"
}

\description{
  This generic has one method that will create objects of class
  \dQuote{mirageInclusionZoneGrid}. The normal use is through
  generation of a sampling surface, but the routine can be called as is
  to produce results for individual stem inclusion zones on a
  \dQuote{mirageTract}. 
}

\usage{
izGridMirage(izObject, tract, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{izObject}{Signature object of a subclass of
	"\code{\linkS4class{InclusionZone}}". }

  \item{tract}{Signature object of class "\code{\linkS4class{mirageTract}}". }

  \item{\dots}{ See \code{\link{izGridMirage-methods}}. }

}

\details{ Objects of class \dQuote{mirageInclusionZoneGrid} are farily
  complex and can hold information on external overlap areas is
  applicable. There is one method for this generic that can be used to
  create \dQuote{mirageInclusionZoneGrid} object as described in
  \code{\link{izGridMirage-methods}}.
 }

\value{
A valid object of class "\code{\linkS4class{mirageInclusionZoneGrid}}."
}

\references{
   See the \dQuote{Mirage Method} and
\dQuote{InclusionZoneGrid} vignettes for examples. 
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{ \code{\link{izGrid}},
  \code{\linkS4class{InclusionZoneGrid}},\code{\linkS4class{mirageInclusionZoneGrid}},
  \code{\linkS4class{sampSurf}}
}

\examples{ 
tr = Tract(c(x=20,y=20), cellSize=1)
mtr = mirageTract(tr)
st = standingTree(centerOffset=c(x=16,y=16))
ag = angleGauge(4)     #4=baf
hps.iz = horizontalPointIZ(st, angleGauge=ag)
hps.izg = izGridMirage(hps.iz, mtr, truncateOverlap=FALSE)
plot(hps.izg, tract=mtr)
#now show external grid estimates...
plot(hps.izg, tract=mtr, showExtended = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
