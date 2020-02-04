\name{distanceLimitedPDSIZ}
\alias{distanceLimitedPDSIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{distanceLimitedPDSIZ}}"
}

\description{ This is the generic definition for generating objects of
  class "distanceLimitedPDSIZ." There is only one constructor method
  corresponding to this generic: \code{\link{distanceLimitedPDSIZ-methods}}.
}

\usage{
distanceLimitedPDSIZ(downLog, pds, dls, ...)
}

\arguments{
  \item{downLog}{ Signature object of class
	"\code{\linkS4class{downLog}}".}
  
  \item{pds}{Signature object of class "\code{\linkS4class{perpendicularDistance}}" containing
	the pertinent perpendicular distance sampling information.}
  
  \item{dls}{Signature object of class "dlsNumeric" which will accept
	either an object of class "\code{\linkS4class{distanceLimited}}"
	containing the pertinent distance limited sampling information or a
	"numeric" object specifying the distance limit.}
  
  \item{\dots}{ See methods.}
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{distanceLimitedPDSIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{distanceLimitedPDSIZ}}."
}

\references{
\describe{
%
\item{}{Ducey, M. J., Williams, M. S., Roberge, S., Kenning, R. S.,
and Gove ,J. H. 2005. Distance limited per- pendicular distance sampling
for coarse woody material: Theory and field results. Unpublished.}

}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  Class "\code{\linkS4class{distanceLimitedPDSIZ}}", and
  \code{\link{distanceLimitedPDSIZ-methods}}. 
}

\examples{
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
pdsEng = perpendicularDistance(kpds=6, units='English')
dlsEng = distanceLimited(2, units='English')
iz.dlpds = distanceLimitedPDSIZ(dl, pds=pdsEng, dls=dlsEng)
iz.dlpds
is.null(iz.dlpds@pdsPart)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
