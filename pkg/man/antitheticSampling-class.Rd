\name{antitheticSampling-class}
\Rdversion{1.1}
\docType{class}
\alias{antitheticSampling-class}
\alias{show,antitheticSampling-method}
\alias{summary,antitheticSampling-method}

\title{Class \code{"antitheticSampling"}}

\description{
  This class definition allows for the application of antithetic
  sampling to any \code{"\linkS4class{MonteCarloSampling}"} subclass
  object. Please see the vignette reference below for more details.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("antitheticSampling", ...)}. However, an object constructor
of the same name, \code{\link{antitheticSampling}}, has been provided and
is the preferred method for creating objects that are ensured to be
valid.
}

\section{Slots}{
  
  \describe{
	
    \item{\code{mcsObj}:}{Object of class \code{"MonteCarloSampling"}:
      This is the original "\code{MonteCarloSampling}" subclass object based on
      the \code{u.s} random numbers found within the slot of this name
      within the object.  }
   
    \item{\code{mcsAnti}:}{Object of class \code{"MonteCarloSampling"}:
      This is the antithetic companion object to \code{mcsObj} that has
      been derived from the \code{1-u.s} random numbers.}
	
    \item{\code{volEst}:}{Object of class \code{"numeric"}: The sample
      mean volume estimate for the bole segment. }
	
    \item{\code{volVar}:}{Object of class \code{"numeric"}: The within
      bole variance estimate of \code{volEst}. }
   
    \item{\code{ci.lo}:}{Object of class \code{"numeric"}: The lower
      1-\code{alphaLevel} confidence interval on the bole volume
      estimate. }
	
    \item{\code{ci.up}:}{Object of class \code{"numeric"}: The upper
      1-\code{alphaLevel} confidence interval on the bole volume
      estimate.   }
	
    \item{\code{alphaLevel}:}{Object of class \code{"numeric"}: The
      two-tailed alpha-level for confidence interval construction. }
	
    \item{\code{trueVol}:}{Object of class \code{"numeric"}: The true
      volume for the stem segment being estimated (see \code{segBnds} in
      the \code{"\linkS4class{MonteCarloSampling}"} class definition). }
  
    \item{\code{relErrPct}:}{Object of class \code{"numeric"}: The
      relative error in volume in percent. }
	
    \item{\code{description}:}{Object of class \code{"character"}: A
       description of the object if desired (defaults are given for each
       class). }

  } %describe
  
} %section

\section{Methods}{
  
  \describe{
	
    \item{show}{\code{signature(object = "antitheticSampling")}: print a
    summary of the object.}
    \item{summary}{\code{signature(object = "antitheticSampling")}: print a
    summary of the object. }
  }
}
   
\references{
\describe{
%
\item{}{Gove, J. H. 2013. Monte Carlo sampling methods in
  \pkg{sampSurf}. Package vignette.}
}
}

\author{
  Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
  The following subclasses and related via the \code{mcsObj} slot:
  \code{\linkS4class{MonteCarloSampling}}, 
  \code{\linkS4class{crudeMonteCarlo}},
  \code{\linkS4class{importanceSampling}},
  \code{\linkS4class{controlVariate}},
}

\examples{
showClass("antitheticSampling")
}
\keyword{classes}
