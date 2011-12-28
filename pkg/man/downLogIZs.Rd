\name{downLogIZs}
\alias{downLogIZs}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{downLogIZs}}"
}

\description{
  This generic function has one method based on the
signature formal argument \code{object}. It is used as a constructor
function for objects that are of class
"\code{\linkS4class{downLogIZs}}". The associated method should be used
in preference to \code{\link{new}} to insure a valid object.  }

\usage{
downLogIZs(object, ...)
}

\arguments{
  \item{object}{ Signature formal argument.  }
  
  \item{\dots}{ Formal arguments that are different for each method, see
	those for details. }
}

\details{
The methods that can be used to generate objects of class
\dQuote{\code{\linkS4class{downLogIZs}}} are detailed here:
\code{\link{downLogIZs-methods}}.  }

\value{
A valid object of class "\code{\linkS4class{downLogIZs}}."
}


\references{
\dQuote{\emph{The InclusionZone Class}} vignette.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
For subclasses of \code{\linkS4class{downLogIZ}}, see: \code{\linkS4class{standUpIZ}},
  \code{\linkS4class{chainSawIZ}},
  \code{\linkS4class{sausageIZ}},
  \code{\linkS4class{pointRelascopeIZ}},
  \code{\linkS4class{perpendicularDistanceIZ}},
  \code{\linkS4class{omnibusPDSIZ}},
  \code{\linkS4class{distanceLimitedPDSIZ}},
  \code{\linkS4class{omnibusDLPDSIZ}}, and
  \code{\linkS4class{distanceLimitedIZ}}. 
  \code{\linkS4class{distanceLimitedMCIZ}}. 
}

\examples{
dls = downLogs(4, units='English', logLens=c(10,30))
dls.su = lapply(dls@logs, 'standUpIZ', plotRadius=5)
izs.su = downLogIZs(dls.su)
bbox(izs.su)
plot(izs.su, axes=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
