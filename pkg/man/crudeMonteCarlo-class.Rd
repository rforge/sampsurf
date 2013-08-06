\name{crudeMonteCarlo-class}
\Rdversion{1.1}
\docType{class}
\alias{crudeMonteCarlo-class}
%\alias{plot,crudeMonteCarlo,missing-method}
\alias{summary,crudeMonteCarlo-method}

\title{Class \code{"crudeMonteCarlo"}}

\description{ This is the class definition that allows for the
  application of crude Monte Carlo sampling to \code{downLog} or
  \code{standingTree} objects. Examples of the class usage can be found
  in the Monte Carlo sampling vignette noted below.  }

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("crudeMonteCarlo", ...)}. However, an object constructor
of the same name, \code{\link{crudeMonteCarlo}}, has been provided and
is the preferred method for creating objects that are ensured to be
valid.  }

\section{Slots}{

  Please note that diameters below are presumed to be in the
  \emph{same} units as length, i.e., meters for \dQuote{metric}, and
  feet for \dQuote{English} \code{units}. Cross-sectional areas are in
  compatible units.

  In addition to the slots provided by the virtual superclass
  \dQuote{\code{\linkS4class{MonteCarloSampling}}}, the following slots
  are represented\ldots
  
  \describe{
	
    \item{\code{proxy}:}{Object of class \code{"character"}: The name of
      the proxy function used. This should normally be the built-in
      \dQuote{\code{cmcProxy}}, which is the default and the only proxy
      allowed by the constructor.}
	
    \item{\code{diam.s}:}{Object of class \code{"numeric"}: A vector of
      \code{n.s} diameters corresponding to the sampled heights
      \code{hgt.s} (see below). }
	
    \item{\code{rho.s}:}{Object of class \code{"numeric"}: A vector of
      cross-sectional areas corresponding to the diameters in
      \code{diam.s}. }
	
    \item{\code{hgt.s}:}{Object of class \code{"numeric"}: A vector of
      sampled heights at which the \code{diam.s} are taken. }
	
    \item{\code{vol.s}:}{Object of class \code{"numeric"}: A vector of
      volume estimates associated with the sampled \code{hgt.s} and
      associated diameters and cross-sectional areas. }
	
    \item{\code{volEst}:}{Object of class \code{"numeric"}: The sample
      mean volume estimate of the \code{vol.s} for the bole segment. }
	
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
      the base class definition). }
  
    \item{\code{relErrPct}:}{Object of class \code{"numeric"}: The
      relative error in volume in percent. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MonteCarloSampling}"}, directly.
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "crudeMonteCarlo", y = "missing")}:
      Displays a plot of the stem object and sampled points. }
	
    \item{summary}{\code{signature(object = "crudeMonteCarlo")}: Prints
	 a summary of the object. }
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
  \code{\linkS4class{MonteCarloSampling}},
  \code{\linkS4class{importanceSampling}},
  \code{\linkS4class{controlVariate}},
  \code{\linkS4class{antitheticSampling}}. 
}

\examples{
showClass("crudeMonteCarlo")
}
\keyword{classes}
