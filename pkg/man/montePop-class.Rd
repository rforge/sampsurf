\name{montePop-class}
\Rdversion{1.1}
\docType{class}
\alias{montePop-class}
\alias{hist,montePop-method}
%\alias{monteBSSample,montePop-method}
%\alias{monteNTSample,montePop-method}
\alias{show,montePop-method}
\alias{summary,montePop-method}

\title{Class \code{"montePop"}}

\description{ Holds a population object with its calculated parameters
  for use in Monte Carlo repeated sampling experiments.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("montePop",
  ...)}. However, it is probably better, and certainly easier, to use
the constructor method \code{\link{montePop}} to create objects.
}

\section{Slots}{
  
  \describe{
    \item{\code{mean}:}{Object of class \code{"numeric"}: The population
	  mean. }
	
    \item{\code{var}:}{Object of class \code{"numeric"}: The population
      variance. }
	
    \item{\code{stDev}:}{Object of class \code{"numeric"}: The population
      standard deviation}
	
    \item{\code{N}:}{Object of class \code{"numeric"}: The size of the population. }
	
    \item{\code{total}:}{Object of class \code{"numeric"}: The population
      total.}
	
    \item{\code{popVals}:}{Object of class \code{"numeric"}: The
      individual population values. }
	
    \item{\code{zeroTruncated}:}{Object of class \code{"logical"}:
    \code{TRUE}: The population is zero-truncated; \code{FALSE}
    otherwise. This is used specifically with
    "\code{\linkS4class{sampSurf}}" objects. }
	
    \item{\code{n}:}{Object of class \code{"numeric"}: The desired sample
      sizes for calculation of the following three slots. Note that this
      and the following slots can be \code{NA} if this sample size-based
      information is not desired. }
	
    \item{\code{fpc}:}{Object of class \code{"numeric"}: The finite
    population correction for each sample size, or \code{NA}.  }
	
    \item{\code{varMean}:}{Object of class \code{"numeric"}: The variance
      of the mean for the population at each sample size, or \code{NA}. }
	
    \item{\code{stErr}:}{Object of class \code{"numeric"}: The population
      standard error of the mean for each sample size, or \code{NA}.  }
	
    \item{\code{description}:}{Object of class \code{"character"}: a
      description of the population if desired. }
  }
}

\section{Methods}{
  \describe{
    \item{hist}{\code{signature(x = "montePop")}: population histogram }
	
    \item{monteBSSample}{\code{signature(population = "montePop")}:
      Bootstrap sampling from the population.  }
	
    \item{monteNTSample}{\code{signature(population = "montePop")}:
      Normal threory (simple random) sampling from the population. }
	
    \item{show}{\code{signature(object = "montePop")}: the population. }
	
    \item{summary}{\code{signature(object = "montePop")}: of the population. }
	 }
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
showClass("montePop")
}
\keyword{classes}
