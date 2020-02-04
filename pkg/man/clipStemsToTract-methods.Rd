\name{clipStemsToTract-methods}
\docType{methods}
\alias{clipStemsToTract-methods}
\alias{clipStemsToTract,downLogs,SpatialPolygons-method}
\alias{clipStemsToTract,standingTrees,SpatialPolygons-method}
\alias{clipStemsToTract,StemContainer,bufferedTract-method}
\alias{clipStemsToTract,StemContainer,SpatialPolygons-method}
\alias{clipStemsToTract,StemContainer,Tract-method}

\title{ Methods for Generic Function \code{clipStemsToTract} in Package
  \pkg{sampSurf} }

\description{
  Please see the generic description \code{\link{clipStemsToTract}} for
  the main details. Essentially, using these methods one can
  check for stems that are wholly or paritally outside the tract, and
  can clip  them to the tract boundary if desired so that only those
  portions of stems (e.g., logs) within the tract are preserved.
}

\section{Methods}{

  Only two of the following methods should be called by the user. The
  others are used to set up the checks and clipping details. The two
  methods for you to use are noted below, and make it simpler to use
  this function.
  
\describe{

\item{\strong{\code{signature(stems = "downLogs", tract = "SpatialPolygons")}}}{

	Please do not use this method, it is the "guts" that does all of the
	detailed checking and clipping for \dQuote{\code{downLogs}}
	collections.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{clipStemsToTract(stems,
                 tract,
                 checkOnly = FALSE,
                 runQuiet = TRUE,
                 showPlot = FALSE,
                 ... )}
	   
	\itemize{
		 
	  \item{\code{stems}:}{ A "\code{\linkS4class{downLogs}}" collection
		object.}
		 
	  \item{\code{tract}:}{ A "\code{\linkS4class{Tract}}" subclass
	    object.} 
		 
	  \item{\code{checkOnly}:}{ \code{TRUE}: just check the status of
	    logs, don't delete or clip any; \code{FALSE}: remove and clip as
	    needed.}
		 
	  \item{\code{runQuiet}:}{ \code{TRUE}: No feedback report;
	    \code{FALSE}: a short summary report is printed. }
		 
	  \item{\code{showPlot}:}{ \code{TRUE}: display a plot showing how
	    the logs were clipped. East-west intersections are in green, N-S
	    in red. It will show (by colors) how logs at corners that
	    intersect more than once get clipped.}
		
	  \item{... :}{ Other arguments, just gobbled for now.}
	  
	} %itemize
  } %item


  

\item{\strong{\code{signature(stems = "standingTrees", tract = "SpatialPolygons")}}}{

	Please do not use this method, it is the "guts" that does all of the
	detailed checking and removal for \dQuote{\code{standingTrees}}
	collections.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{clipStemsToTract(stems,
                 tract,
                 checkOnly = FALSE,
                 runQuiet = TRUE,
                 ... )}
	   
	\itemize{
		 
	  \item{\code{stems}:}{ A "\code{\linkS4class{standingTrees}}" collection
		object.}
		 
	  \item{\code{tract}:}{ A "\code{\linkS4class{Tract}}" subclass
	    object.} 

	  All other arguments are as previously defined.
	  	  
	} %itemize
  } %item

  


\item{\strong{\code{signature(stems = "StemContainer", tract = "bufferedTract")}}}{

  This is one of the two methods that should be called by the user. A
  collection of logs or trees can be passed along with a buffered tract
  object. Please note that the internal buffer rectangle is used to clip
  against in this case by default, \emph{not} the external tract boundary.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{clipStemsToTract(stems,
                 tract,
                 checkOnly = FALSE,
                 runQuiet = TRUE,
                 clipToBuffer = TRUE,
                 ... )}
	   
	\itemize{
		 
	  \item{\code{stems}:}{ A "\code{\linkS4class{downLogs}}" or
		"\code{\linkS4class{standingTrees}}" collection object.}
		 
	  \item{\code{tract}:}{ A "\code{\linkS4class{bufferedTract}}"
		object.} 
		 
	  \item{\code{clipToBuffer}:}{ \code{TRUE}: Clip stems to the
	    internal buffer (default); \code{FALSE}: clip to the tract
	    boundary.}
		
	  All other arguments are as previously defined.
	  
	} %itemize
  } %item






\item{\strong{\code{signature(stems = "StemContainer", tract = "SpatialPolygons")}}}{

  This method should not be called by the user. Please use the methods
  for \dQuote{\code{Tract}} or \dQuote{\code{bufferedTract}}
  objects. All this does is a general test of whether the bounding box
  for the collection is within the tract, it is called automatically
  from the other methods. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{clipStemsToTract(stems,
                 tract,
                 checkOnly = TRUE,
                 runQuiet = TRUE,
                 ... )}

	   
	%\itemize{

	All arguments are as previously defined in other methods. Please
	don't use this!
	  
	%} %itemize
  } %item



\item{\strong{\code{signature(stems = "StemContainer", tract = "Tract")}}}{

  This is one of the two methods that should be called by the user. A
  collection of logs or trees can be passed along with a tract subclass
  (though there is a special interface for buffered tracts, see above) object.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{clipStemsToTract(stems,
                 tract,
                 checkOnly = FALSE,
                 runQuiet = TRUE,
                 ... )}
	   
	\itemize{
		 
	  \item{\code{stems}:}{ A "\code{\linkS4class{downLogs}}" or
		"\code{\linkS4class{standingTrees}}" collection object.}
		 
	  \item{\code{tract}:}{ A "\code{\linkS4class{Tract}}"
		object.} 

	  All other arguments are as previously defined.
	  
	} %itemize
  } %item




}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
