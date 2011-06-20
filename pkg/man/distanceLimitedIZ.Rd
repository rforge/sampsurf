\name{distanceLimitedIZ}
\alias{distanceLimitedIZ}

\title{
  Generate Objects of Class "\code{\linkS4class{distanceLimitedIZ}}"
}

\description{This is the generic definition for generating objects of
  class "distanceLimitedIZ." There is only one constructor method
  corresponding to this generic: \code{\link{distanceLimitedIZ-methods}}
}

\usage{
distanceLimitedIZ(downLog, dls, ...)
}

\arguments{
  
  \item{downLog}{Signature object of class 	"\code{\linkS4class{downLog}}".}

  \item{dls}{Signature object of class "\code{\linkS4class{distanceLimited}}" containing
	the pertinent distance limit.}

  \item{\dots}{ See methods.}
  
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{distanceLimitedIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{distanceLimitedIZ}}."
}

\references{  Notes in vignette form are available from the author. }

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  Class "\code{\linkS4class{distanceLimitedIZ}}", and
  \code{\link{distanceLimitedIZ-methods}}. 
}

\examples{
#
# generate a simple inclusion zone object with distance limit
# of 6 feet...
#
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
dlsEng = distanceLimited(6, units='English')
iz.dls = distanceLimitedIZ(dl, dls=dlsEng)
iz.dls
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
