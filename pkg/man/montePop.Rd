\name{montePop}
\alias{montePop}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{montePop}}"
}

\description{ This is the generic definition for generating objects of
  class "montePop." There are several methods corresponding to this
  generic with different signatures that may be found in
  \code{\link{montePop-methods}}.
}

\usage{
montePop(popVals, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{popVals}{ Signature object, which differs for each method. }

  \item{\dots}{  See methods.}
}

\details{
  The methods associated with this generic should be used to
  construct objects of class "\code{\linkS4class{montePop}}." Such
  objects contain the population values and the basic population
  parameters (statistics), which will be calculated automatically by the
  constructor.  }

\value{
A valid object of class "\code{\linkS4class{montePop}}."
}

\references{
The \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{monte}}, \code{\linkS4class{monteSample}}    
}

\examples{
mp = montePop(rnorm(100))
mp
hist(mp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
