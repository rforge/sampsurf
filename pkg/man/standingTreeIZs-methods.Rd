\name{standingTreeIZs-methods}
\docType{methods}
\alias{standingTreeIZs-methods}
\alias{standingTreeIZs,list-method}

\title{  Method for "standingTreeIZs" object construction in Package
  \pkg{sampSurf} }

\description{ The following is the constructor method for class
 \dQuote{\code{\linkS4class{standingTreeIZs}}} in Package `sampSurf'. Please
 see \dQuote{\emph{The InclusionZone Class}} vignette for more
 information and examples on the usage of this method, as well as the
 \code{\link{standingTreeIZs}} generic and class
 \code{\linkS4class{standingTreeIZs}}. 
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "list")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         description = '',
         ... )}
	 \itemize{
	   
	   \item{\code{object}:}{ A list object containing the collection of
         inclusion zones for a given subclass of
         "\code{\linkS4class{standingTreeIZ}}". Note that the list must
         contain objects that are all of the same class, or sampling
         method.}
	   
	   \item{\code{description}:}{  A description of the collection.}
	  
	  \item{... :}{ Other arguments to be passed along (not used currently).}
	} %itemize
  } %item

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
