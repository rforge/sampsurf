\name{monteNTSample-class}
\Rdversion{1.1}
\docType{class}
\alias{monteNTSample-class}

\title{Class \code{"monteNTSample"}}

\description{
  This class contains the necessary structure for doing normal theory
  sample size experiments under simple random sampling. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("monteNTSample",
  ...)}. However, it is preferable to use the constructor method of the
same name \code{\link{monteNTSample}} to minimize problems with
potentially invalid objects.
}

\section{Slots}{

  Only one new slot is added here from the superclass. In addition, the
  definitions for three other slots that are method-dependent are also given.
  
  \describe{
	
    \item{\code{t.values}:}{Object of class \code{"numeric"}: Student's
      \emph{t} values for each sample size \code{n} with two-tailed
      \eqn{alpha}-level \code{alpha}. }
	
    \item{\code{means}:}{Object of class \code{"data.frame"}: The data
      frame contains the individual means for all \code{mcSamples} by
      \code{length(n)} samples drawn from the population. Taking column
      means gives the overall mean for each of the sample sizes. }
	
    \item{\code{lowerCIs}:}{Object of class \code{"data.frame"}: This is
      the usual normal theory lower limit for each sample: \eqn{\bar{y}
      - t^{1-\alpha/2}_{n-1} s_{\bar{y}}}, where \eqn{t} is Student's
      \eqn{t}-value and \eqn{s_{\bar{y}}} is the standard error of the
      mean for the sample. }
	
    \item{\code{upperCIs}:}{Object of class \code{"data.frame"}: This is
      the usual normal theory upper limit for each sample: \eqn{\bar{y} +
      t^{1-\alpha/2}_{n-1} s_{\bar{y}}}, where \eqn{t} is Student's
      \eqn{t}-value and \eqn{s_{\bar{y}}} is the standard error of the mean for
      the sample.  object. }
	
  }
}

\section{Extends}{
Class \code{"\linkS4class{monteSample}"}, directly. \cr
Class \code{"\linkS4class{monteNTSampleOrNULL}"}, directly.
}

\section{Methods}{
No methods defined with class "monteNTSample" in the signature.
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
showClass("monteNTSample")
}
\keyword{classes}
