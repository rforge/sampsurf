\name{hybridDLPDSIZ}
\alias{hybridDLPDSIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{hybridDLPDSIZ}}"
}

\description{This is the generic definition for generating objects of
  class "hybridDLPDSIZ." There is only one constructor method
  corresponding to this generic: \code{\link{hybridDLPDSIZ-methods}}.
}

\usage{
hybridDLPDSIZ(downLog, pds, dls, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{downLog}{ Signature object of class "\code{\linkS4class{downLog}}".}

  \item{pds}{Signature object of class "\code{\linkS4class{perpendicularDistance}}" containing
	the pertinent perpendicular distance sampling information.}

  \item{dls}{Signature object of class "dlsNumeric" which will accept
	either an object of class "\code{\linkS4class{distanceLimited}}"
	containing the pertinent distance limited sampling information or a
	"numeric" object specifying the distance limit.}

  \item{\dots}{ See methods. }

}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{hybridDLPDSIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{hybridDLPDSIZ}}."
}

\references{
  \emph{"The InclusionZone Class"} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  Class "\code{\linkS4class{hybridDLPDSIZ}}", and
  \code{\link{hybridDLPDSIZ-methods}}. 
}

\examples{
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
pdsEng = perpendicularDistance(kpds=6, units='English')
dlsEng = distanceLimited(2, units='English')
iz.hdlpds = hybridDLPDSIZ(dl, pds=pdsEng, dls=dlsEng)
iz.hdlpds
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
