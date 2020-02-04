\name{distanceLimitedMCIZ}
\alias{distanceLimitedMCIZ}

\title{
  Generate Objects of Class "\code{\linkS4class{distanceLimitedMCIZ}}"
}

\description{ This is the generic definition for generating objects of
  class "distanceLimitedMCIZ." There is only one constructor method
  corresponding to this generic: \code{\link{distanceLimitedMCIZ-methods}}
}

\usage{
distanceLimitedMCIZ(downLog, dls, ...)
}

\arguments{
  \item{downLog}{ Signature object of class
	"\code{\linkS4class{downLog}}". }
  
  \item{dls}{Signature object of class "\code{\linkS4class{distanceLimited}}" containing
	the pertinent distance limit.}

  \item{\dots}{ See methods.}
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{distanceLimitedMCIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{distanceLimitedMCIZ}}."
}

\references{ Notes in vignette form are available from the author. }

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  Class "\code{\linkS4class{distanceLimitedMCIZ}}", and
  \code{\link{distanceLimitedMCIZ-methods}}. 
}

\examples{
#
# generate a simple inclusion zone object with distance limit
# of 6 feet...
#
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
dlsEng = distanceLimited(6, units='English')
iz.dlmcs = distanceLimitedMCIZ(dl, dls=dlsEng)
iz.dlmcs
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
