\name{omnibusPDSIZ}
\alias{omnibusPDSIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{omnibusPDSIZ}}"
}

\description{ This is the generic definition for generating objects of
  class "omnibusPDSIZ." There is only one constructor method
  corresponding to this generic: \code{\link{omnibusPDSIZ-methods}}.
}

\usage{
omnibusPDSIZ(downLog, pds, ...)
}

\arguments{
  \item{downLog}{ Signature object of class	"\code{\linkS4class{downLog}}".}
  
  \item{pds}{Signature object of class "\code{\linkS4class{perpendicularDistance}}" containing
	the pertinent perpendicular distance sampling information.
}
  \item{\dots}{ See methods. }
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{omnibusPDSIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{omnibusPDSIZ}}."
}

\references{
\describe{
%
\item{}{Ducey, M. J., Williams, M. S., Gove, J. H. and Valentine,
  H. T. 2008. Simultaneous unbiased estimates of multiple downed wood
  attributes in perpendicular distance sampling. \emph{Canadian Journal of
  Forest Research} \bold{38}:2044--2051.}
%
\item{}{Williams, M. S. and Gove, J. H. 2003. Perpendicular distance sampling:
  an alternative method for sampling downed coarse woody
  debris. \emph{Canadian Journal of Forest Research}
  \bold{33}:1564--1579.}
}
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  Class "\code{\linkS4class{omnibusPDSIZ}}", and
  \code{\link{omnibusPDSIZ-methods}}. 
}

\examples{
#
# creates an inclusion zone object for sampling with probability
# proportional to volume by default...
#
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
pdsEng = perpendicularDistance(kpds=6, units='English')
iz.opdsv = omnibusPDSIZ(dl, pdsEng)
iz.opdsv
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
