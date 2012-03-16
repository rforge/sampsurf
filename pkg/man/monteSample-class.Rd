\name{monteSample-class}
\Rdversion{1.1}
\docType{class}
\alias{monteSample-class}
\alias{hist,monteSample-method}
\alias{show,monteSample-method}
\alias{summary,monteSample-method}

\title{Class \code{"monteSample"}}

\description{
  This is a virtual class defining the base representation
  for objects that hold information about repeated (Monte Carlo)
  sampling from population objects of class
  "\code{\linkS4class{montePop}}". It provides a basic class setup for
  looking at Monte Carlo convergence of as the sample size grows larger.
}

\section{Objects from the Class}{A virtual Class: No objects may be
  created from it. For usable subclasses please see
  \code{\linkS4class{monteNTSample}} and
  \code{\linkS4class{monteBSSample}}.}

\section{Slots}{
  
  \describe{

	\item{\code{mcSamples}:}{Object of class \code{"numeric"}: A scalar
      numeric specifying the number of Monte Carlo samples drawn from
      the population. }
	
    \item{\code{n}:}{Object of class \code{"numeric"}: A numeric vector
      listing the different sample sizes recorded in the object that
      have been drawn from a \dQuote{montePop} population object. So, if
      we have drawn samples of size n = 10,20,30, then this would hold
      \code{c(10,20,30)}, with associated names
      \code{c('n.10','n.20','n.30')}. }
	
    \item{\code{alpha}:}{Object of class \code{"numeric"}: The
      two-tailed alpha level for which confidence intervals have been
      calculated. I.e., for the 95\% confidence level 
      \code{alpha = 0.05}}
	
    \item{\code{replace}:}{Object of class \code{"logical"}: \code{TRUE}
      if the samples have been drawn from the population with
      replacement, \code{FALSE} otherwise. }
	
    \item{\code{ranSeed}:}{Object of class \code{"numeric"}: The random
      number seed as a numeric vector. Please see the \R documentation
      on \code{.Random.seed} for information on the format of this
      slot. Note that it is \emph{not} a simple scalar integer \dQuote{seed},
      but a vector of integers containing the state of the random number
      generator at the beginning of the simulations. }
	
    \item{\code{fpc}:}{Object of class \code{"numeric"}: The finite
      population correction factors for each sample size \code{n}. The
      correction is: f = (N-n)/N. }
	
    \item{\code{means}:}{Object of class \code{"data.frame"}: A data
      frame with \code{mcSamples} rows, and one column for each of the
      sample sizes in the \code{n} slot of the object. What is stored
      here depends on the subclass object type, so please see the
      respective definitions for these slots.  }

	\item{}{\emph{Note: The following six slots have the same dimensions as
	  the \code{means} slot.}}
	
    \item{\code{vars}:}{Object of class \code{"data.frame"}: Contains
      the individual sample variances. }
	
    \item{\code{stDevs}:}{Object of class \code{"data.frame"}: Contains
      the individual sample standard deviations. }
	
    \item{\code{varMeans}:}{Object of class \code{"data.frame"}:
      Contains the individual variance of the mean values. }
	
    \item{\code{stErrs}:}{Object of class \code{"data.frame"}: Contains
      the individual standard errors. }
	
    \item{\code{lowerCIs}:}{Object of class \code{"data.frame"}:
      Contains the individual lower limit for the confidence intervals.
      This is defined differently for the different subclasses. }
	
    \item{\code{upperCIs}:}{Object of class \code{"data.frame"}:
      Contains the individual upper limit for the confidence intervals.
      This is defined differently for the different subclasses. }
	
    \item{\code{caught}:}{Object of class \code{"data.frame"}: Contains
      a flag where \code{TRUE} means the confidence interval caught the
      population mean and \code{FALSE} means it failed to catch the
      population mean. Taking column sums, therefore (since \code{TRUE
      == 1} and \code{FALSE == 0}) will give the number of intervals
      that caught the population mean for each sample size. This is used
      to calculate the next slot below.  }
	
    \item{\code{caughtPct}:}{Object of class \code{"numeric"}: The
      percentage of times the confidence intervals caught the population
      mean as calculated from the data frame in the \code{caught} slot
      of the object. }
	
    \item{\code{stats}:}{Object of class \code{"data.frame"}: A summary
      data frame with rows as the \emph{average} of each column (i.e.,
      over all Monte Carlo samples) from the information in the data frames
      above (\code{means}, \code{vars}, \code{stDevs},
      \code{varMeans}, \code{stErrs}, \code{lowerCIs}, and
      \code{upperCIs}). The interpretation of some of the rows depends
      on the subclass object as has been mentioned above, please see the
      vignette below for more details. }
}}


\section{Methods}{
  
  \describe{
    \item{hist}{\code{signature(x = "monteSample")}: Histogram of the
      means by sample size }
	
    \item{show}{\code{signature(object = "monteSample")}: Object summary. }
	
    \item{summary}{\code{signature(object = "monteSample")}: Object summary. }
	
	 }
}

\references{
The \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{monte}}, \code{\linkS4class{montePop}}; for
subclasses, see: \code{\linkS4class{monteNTSample}} and
\code{\linkS4class{monteBSSample}}.
}
   

\examples{
showClass("monteSample")
}
\keyword{classes}
