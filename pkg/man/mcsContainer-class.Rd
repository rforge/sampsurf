\name{mcsContainer-class}
\Rdversion{1.1}
\docType{class}
\alias{mcsContainer-class}
%\alias{antitheticSampling,mcsContainer-method}
%\alias{hist,mcsContainer-method}
%\alias{plot,mcsContainer,missing-method}
\alias{show,mcsContainer-method}
\alias{summary,mcsContainer-method}

\title{Class \code{"mcsContainer"}}

\description{ This class allows one to store a collection of objects
  that are all a subclass of "\code{\linkS4class{MonteCarloSampling}}."
  }

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("mcsContainer", ...)}. However, using the constructor
methods with the same name as the subsampling method used is preferred
(see \code{\link{mcsContainer}}). Please refer to the vignette reference
below for more examples.  }

\section{Slots}{
  
  \describe{
    \item{\code{mcsObjs}:}{Object of class \code{"list"}: Each object in
    the list must be of the same class for a valid object. }
	
    \item{\code{stats}:}{Object of class \code{"matrix"}: Summary
    statistics for the objects in slot \code{mcsObjs}.  }
	
    \item{\code{description}:}{Object of class \code{"character"}: A
    character description of the contents if desired. }
	
  }
}

\section{Methods}{
  
  \describe{
    \item{antitheticSampling}{\code{signature(object =
	  "mcsContainer")}: Apply antithetic sampling to each object in the collection. }
	
    \item{hist}{\code{signature(x = "mcsContainer")}: Generate
      histograms for the collection. }
	
    \item{plot}{\code{signature(x = "mcsContainer", y = "missing")}:
      Plot the container collection. }
	
    \item{show}{\code{signature(object = "mcsContainer")}: Print a summary. }
	
    \item{summary}{\code{signature(object = "mcsContainer")}: Print a summary. }
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

\note{ Please note that this is not a completely functional container
  class in the traditional sense at present as it does not have
  replacement, deletion, or addition functions. If you need to do any of
  these operations, perform them on the \code{list} object (in the
  \code{mcsObjs} slot) and then recreate the container. If the object is
  not re-built after, e.g., deletion, the summary statistics will be
  incorrect.}

\seealso{
\code{\link{mcsContainer}},
\code{\linkS4class{antitheticContainer}}.
}

\examples{
showClass("mcsContainer")
}
\keyword{classes}
