\name{monteNTSample}

\alias{monteNTSample}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{monteNTSample}}"
}

\description{
  This generic will allow the simple creation of objects of
  class "\code{\linkS4class{monteNTSample}}." Method definitions defining
  all arguments are found in \code{\link{monteNTSample-methods}}.
}

\usage{
monteNTSample(population, ...)
}

\arguments{
  \item{population}{ Signature argument, currently only objects of class
	"\code{\linkS4class{montePop}}" are supported.
}

  \item{\dots}{ See methods. }
}

\details{ See the associated methods for this constructor in
  \code{\link{monteNTSample-methods}}. 
}

\value{
  A valid object of class "\code{\linkS4class{monteNTSample}}."
}

\references{
The \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{monteSample}}, \code{\linkS4class{monteBSSample}}    
}


\examples{
mp = montePop(rnorm(100))
mnts = monteNTSample(mp, n=c(10,20,30))
mnts
hist(mnts)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
