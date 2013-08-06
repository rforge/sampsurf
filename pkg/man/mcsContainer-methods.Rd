\name{mcsContainer-methods}
\docType{methods}
\alias{mcsContainer-methods}
\alias{mcsContainer,list-method}

\title{ Methods for "\code{mcsContainer}" object construction in Package
  \pkg{sampSurf} }

\description{
There is only one method for the \code{\link{mcsContainer}} generic
function. Please note that this method should not be called
directly. Instead, one should use one of the methods named for the
subsampling procedure:  \code{\link{crudeMonteCarlo}}, \code{\link{importanceSampling}} and
  \code{\link{controlVariate}} as described in the generic
  \code{\link{mcsContainer}}. These methods call this function with a
  \code{list} object to actually create the container, ensuring that
  everything is set correctly for the collection and the subsampling
  method used.
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(object = "list")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{mcsContainer(object,
             description = 'Monte Carlo Sampling container object',
             ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\link{list}}".}
		
    } %itemize
  } %item


	  
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
