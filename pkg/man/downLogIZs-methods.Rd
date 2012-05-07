\name{downLogIZs-methods}
\docType{methods}
\alias{downLogIZs-methods}
\alias{downLogIZs,list-method}
\alias{downLogIZs,downLogs-method}

\title{ Method for "downLogIZs" object construction in Package `sampSurf' }

\description{ The following are the constructor methods for class
 \dQuote{\code{\linkS4class{downLogIZs}}} in Package `sampSurf'. Please
 see \dQuote{\emph{The InclusionZone Class}} vignette for more
 information and examples on the usage of these methods, as well as the
 \code{\link{downLogIZs}} generic and class
 \code{\linkS4class{downLogIZs}}.  }

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "list")}}}{

   This method will create a container object out of a list containing
  "\code{\linkS4class{downLogIZ}}" objects. }
  
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

  
\item{\strong{\code{signature(object = "downLogs")}}}{ 

  Create a container object out of a
  "\code{\linkS4class{downLogs}}" container object and an
  "\code{\linkS4class{InclusionZone}}" subclass specification. }

  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         iZone,
         description = '',
         ... )}
	 \itemize{
	   
	   \item{\code{object}:}{ A valid "\code{\linkS4class{downLogs}}"
         container object. }

	   \item{\code{iZone}:}{ A legal \dQuote{InclusionZone} class
	     specification (constructor name) that is relevant to
	     \dQuote{downLog} objects, as a character string.}
	   
	   \item{\code{description}:}{  A description of the collection.}
	  
	  \item{... :}{ Other arguments to be passed along, e.g., to the
	    \code{iZone} constructor.}
	} %itemize
  } %item
		 
}}

\keyword{methods}
