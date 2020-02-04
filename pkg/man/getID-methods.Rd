\name{getID-methods}
\docType{methods}
\alias{getID-methods}
\alias{getID,downLog-method}
\alias{getID,downLogs-method}
\alias{getID,standingTree-method}
\alias{getID,standingTrees-method}

\title{ Methods for Function \code{getID} in Package \pkg{sampSurf} }

\description{ Please see the generic, this is a simple function for
  which each of the methods returns a vector (sometimes of length one)
  of identifiers for the objects passed. The IDs can be buried deeply
  within the object, especially when they are spatial IDs, so it is
  simpler to have these little helper methods.

  Please note that identifiers for "\code{\linkS4class{Stem}}" class
  objects are the spatial IDs used in the polygon representing the
  individual log or tree. 
}

\section{Methods}{
  
\describe{

  \item{\strong{\code{signature(object = "downLog")}}}{

  This will return the spatial ID for the log as the log's identifier.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{getID(object, ...)}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class \dQuote{downLog}. }
	} %itemize
 } %item


  

\item{\strong{\code{signature(object = "downLogs")}}}{

  Returns a vector of spatial IDs for the logs in the collection.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{getID(object, ...)}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class \dQuote{downLogs}. }
	} %itemize
 } %item




 \item{\strong{\code{signature(object = "standingTree")}}}{
   
  This will return the spatial ID for the tree as the tree's identifier.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{getID(object, ...)}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class \dQuote{standingTree}. }
	} %itemize
 } %item


 
\item{\strong{\code{signature(object = "standingTrees")}}}{

  Returns a vector of spatial IDs for the trees in the collection.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{getID(object, ...)}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class \dQuote{standingTrees}. }
	} %itemize
 } %item

}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
