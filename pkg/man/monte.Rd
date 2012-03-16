\name{monte}
\alias{monte}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Generate Objects of Class "\code{\linkS4class{monte}}"
}

\description{
 This is the generic definition for generating objects of
  class "monte." There are currently several methods corresponding to this
  generic whose documentation may be found in \code{\link{monte-methods}}.}

\usage{
monte(object, ...)
}

\arguments{
  
  \item{object}{ Signature argument, which differs for each method. This
    specifies the population from which samples will be drawn.}
  
  \item{\dots}{  See methods. }
}

\details{   The methods associated with this generic should be used to
  construct objects of class "\code{\linkS4class{monte}}." These objects
  are specifically designed to hold information about Monte Carlo
  experiments where one resamples from a known population to infer
  efficiency and perhaps locate any bias in different sampling
  estimators. The constructor methods can be used to look at traditional
  normal theory and bootstrap confidence intervals in terms of nominal
  catch rates for the population mean.
}

\value{
A valid object of class "\code{\linkS4class{monte}}."
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
#
# from a sampSurf object...
#
smTract = Tract(c(x=30,y=30), cellSize=0.5)
smbuffTr = bufferedTract(8,smTract)
ss.sa = sampSurf(10, smbuffTr, 'sausageIZ', plotRadius=3, estimate='Length')
m.sa = monte(ss.sa, n=c(10,20))
hist(m.sa)

#
# simple population...
#
mp = montePop(rnorm(100), n=c(10,30))
mt = monte(mp, mcSamples=250, R=150)     #takes n from mp object
mt
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
