\name{pointRelascopeIZ}
\alias{pointRelascopeIZ}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Generate Objects of Class "\code{\linkS4class{pointRelascopeIZ}}"
}

\description{ This is the generic definition for generating objects of
  class "pointRelascopeIZ." There is only one constructor method
  corresponding to this generic: \code{\link{pointRelascopeIZ-methods}}.
  }

\usage{
pointRelascopeIZ(downLog, prs, ...)
}

\arguments{
  
  \item{downLog}{ Signature object of class
	"\code{\linkS4class{downLog}}". }

  \item{prs}{ Signature object of class \code{pointRelascope} containing
	the pertinent point relascope sampling information.
}

  \item{\dots}{ See methods. }
}

\details{
  Since only one method exists for this generic, its signature
  arguments coincide with the above definitions. Please see
  \code{\link{pointRelascopeIZ-methods}} for more details.
}

\value{
A valid object of class "\code{\linkS4class{pointRelascopeIZ}}."
}

\references{
J. H. Gove, A. Ringvall, G. Stahl, and M. J. Ducey. 1999. Point
relascope sampling of downed coarse woody debris. \emph{Canadian Journal
  of Forest Research} \bold{29}(11):1718--1726.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  Class "\code{\linkS4class{pointRelascopeIZ}}", and
  \code{\link{pointRelascopeIZ-methods}}. 
}

\examples{
dl = downLog(buttDiam=15, solidType=4, logAngle=pi/3, logLen=10, units='English')
(angle = .StemEnv$rad2Deg(2*atan(1/4)))
prs = pointRelascope(angle, units='English')
iz.prs = pointRelascopeIZ(dl, prs=prs)
summary(iz.prs)
plot(iz.prs, axes=TRUE, showDualCenters=TRUE, cex=2, showNeedle=TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%% \keyword{ ~kwd1 }
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
