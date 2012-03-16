\name{monteBSSample-class}
\Rdversion{1.1}
\docType{class}
\alias{monteBSSample-class}
\alias{summary,monteBSSample-method}

\title{Class \code{"monteBSSample"}}

\description{
  This class contains the necessary structure for doing Monte Carlo
  sample size experiments under bootstrap sampling. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("monteBSSample",
  ...)}. However, it is preferable to use the constructor method of the
same name \code{\link{monteBSSample}} to minimize problems with
potentially invalid objects. 
}

\section{Slots}{
  
  \describe{
	
    \item{\code{degenerate}:}{Object of class \code{"numeric"}: It may
      happen that, especially for small \eqn{n}, some of the samples
      drawn from the population can be degenerate (all the same
      value). When this happens, all of the bootstrap resamples will
      also be degenerate, and confidence interval estimation is
      impossible since it is based on the distribution of the bootstrap
      sample means. This slot is a numeric vector with the number of
      degenerate samples for each sample size in the \code{n} slot of
      the object.}
	
    \item{\code{R}:}{Object of class \code{"numeric"}: The number of
      bootstrap sample replications.}
	
    \item{\code{means}:}{Object of class \code{"data.frame"}: The data
      frame contains the overall bootstrap sample means for each of the
      \code{mcSamples} by \code{length(n)} samples drawn from the
      population. The overall bootstrap sample mean is defined here as
      the mean of the \eqn{R} individual (second-stage) bootstrap sample
      means for each case. Taking column means gives the overall mean
      for each of the sample sizes.}
	
    \item{\code{lowerCIs}:}{Object of class \code{"data.frame"}: This is
      the lower ``BCa'' confidence interval endpoint for the
      \eqn{1-\alpha/2} confidence level. It is calculated from the
      distribution of bootstrap sample means that is created in
      bootstrap sampling for each Monte Carlo sample and sample size,
      \eqn{n}. }
	
    \item{\code{upperCIs}:}{Object of class \code{"data.frame"}: This is
      the upper ``BCa'' confidence interval endpoint for the
      \eqn{1-\alpha/2} confidence level. It is calculated from the
      distribution of bootstrap sample means that is created in
      bootstrap sampling for each Monte Carlo sample and sample size,
      \eqn{n}.}
	
  }
}

\section{Extends}{
Class \code{"\linkS4class{monteSample}"}, directly. \cr
Class \code{"\linkS4class{monteBSSampleOrNULL}"}, directly.
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "monteBSSample")}: Object summary. }
	 }
}


\references{
The \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{monte}}, \code{\linkS4class{montePop}}
}

\examples{
showClass("monteBSSample")
}
\keyword{classes}
