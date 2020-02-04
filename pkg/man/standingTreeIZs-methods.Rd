\name{standingTreeIZs-methods}
\docType{methods}
\alias{standingTreeIZs-methods}
\alias{standingTreeIZs,list-method}
\alias{standingTreeIZs,standingTrees-method}

\title{  Method for "standingTreeIZs" object construction in Package
  \pkg{sampSurf} }

\description{ The following are the constructor methods for class
 \dQuote{\code{\linkS4class{standingTreeIZs}}} in Package `sampSurf'. Please
 see \dQuote{\emph{The InclusionZone Class}} vignette for more
 information and examples on the usage of these methods, as well as the
 \code{\link{standingTreeIZs}} generic and class
 \code{\linkS4class{standingTreeIZs}}. 
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "list")}}}{

   This method will create a container object out of a list containing
  "\code{\linkS4class{standingTreeIZ}}" objects. }
  
  
  \item{}{\bold{usage\ldots}
    \preformatted{standingTreeIZs(object,
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
  
\item{\strong{\code{signature(object = "standingTrees")}}}{ 

  Create a container object out of a
  "\code{\linkS4class{standingTrees}}" container object and an
  "\code{\linkS4class{InclusionZone}}" subclass specification. }

  
  \item{}{\bold{usage\ldots}
    \preformatted{standingTreeIZs(object,
                iZone,
                description = '',
                ... )}
			  
	 \itemize{
	   
	   \item{\code{object}:}{ A valid "\code{\linkS4class{standingTrees}}"
         container object. }

	   \item{\code{iZone}:}{ A legal \dQuote{InclusionZone} class
	     specification (constructor name) that is relevant to
	     \dQuote{standingTree} objects, as a character string.}
	   
	   \item{\code{description}:}{  A description of the collection.}
	  
	  \item{... :}{ Other arguments to be passed along, e.g., to the
	    \code{iZone} constructor.}
	} %itemize
  } %item

}}

\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
