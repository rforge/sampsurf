\name{antitheticContainer-methods}
\docType{methods}
\alias{antitheticContainer-methods}
\alias{antitheticContainer,list-method}

\title{ Methods for "\code{antitheticContainer}" object creation in Package
  \pkg{sampSurf} }

\description{ There is only one method for the \code{\link{antitheticContainer}} generic
function. Please note that this method should not be called
directly. Instead, one should use the \code{\link{antitheticSampling}}
constructor by passing it an object of class
"\code{\linkS4class{mcsContainer}}" containing a collection of objects
where importance, crude Monte Carlo or control variate sampling have
been applied.
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "list")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{antitheticContainer(object,
                    description = 'Antithetic Sampling container object',
                    ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\link{list}}".}
		
    } %itemize
  } %item


	  
}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
