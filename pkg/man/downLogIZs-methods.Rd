\name{downLogIZs-methods}
\docType{methods}
\alias{downLogIZs-methods}
\alias{downLogIZs,list-method}

\title{ Method for "downLogIZs" object construction in Package `sampSurf' }

\description{ The following is the constructor method for class
 \dQuote{\code{\linkS4class{downLogIZs}}} in Package `sampSurf'. Please
 see \dQuote{\emph{The InclusionZone Class}} vignette for more
 information and examples on the usage of this method, as well as the
 \code{\link{downLogIZs}} generic and class
 \code{\linkS4class{downLogIZs}}.  }

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "list")}}}{ }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         description = '',
         ... )}
	 \itemize{
	   
	   \item{\code{object}:}{ A list object containing the collection of
         inclusion zones for a given subclass of
         "\code{\linkS4class{downLogIZ}}". Note that the list must
         contain objects that are all of the same class, or sampling
         method.}
	   
	   \item{\code{description}:}{  A description of the collection.}
	  
	  \item{... :}{ Other arguments to be passed along (not used currently).}
	} %itemize
  } %item
		 
}}

\keyword{methods}
