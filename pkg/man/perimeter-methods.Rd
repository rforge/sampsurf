\name{perimeter-methods}
\docType{methods}
\alias{perimeter-methods}
\alias{perimeter,chainSawIZ-method}
\alias{perimeter,circularPlot-method}
\alias{perimeter,downLog-method}
\alias{perimeter,downLogIZs-method}
\alias{perimeter,downLogs-method}
\alias{perimeter,sampSurf-method}
\alias{perimeter,sausageIZ-method}
\alias{perimeter,standUpIZ-method}
\alias{perimeter,pointRelascopeIZ-method}
\alias{perimeter,perpendicularDistanceIZ-method}
\alias{perimeter,distanceLimitedMCIZ-method}
\alias{perimeter,Tract-method}

\title{ Methods for Function perimeter in Package `sampSurf' }

\description{ The methods below will extract the spatial perimeter
  in the form of a "\code{\linkS4class{SpatialPolygons}}" object, which
  can then be plotted. In some cases, where the actual perimeter of the
  object makes sense, this is returned; in others, where it does not,
  the perimeter of the bounding box is returned.
}

\section{Methods}{
\describe{

  \item{\strong{\code{signature(object = "downLog")}}}{%\cr

	Returns the spatial object associated with the perimeter of the down
    log.}

  \item{\strong{\code{signature(object = "standingTree")}}}{%\cr

	Returns the spatial object associated with the perimeter of the standing
    tree.}

  \item{\strong{\code{signature(object = "StemContainer")}}}{ %\cr

	Returns the spatial object associated with the bounding box for the
    collection of \dQuote{downLog} or \dQuote{standingTree} objects
    within the appropriate \dQuote{downLogs} or \dQuote{standingTrees}
    container objects, respectively.}

  \item{\strong{\code{signature(object = "circularPlot")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    circular plot. }

  \item{\strong{\code{signature(object = "sausageIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    sausage-shaped inclusion zone. }

  \item{\strong{\code{signature(object = "standUpIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    circular-shaped inclusion zone. }

  \item{\strong{\code{signature(object = "chainSawIZ")}}}{ %\cr

	This will return different inclusion zone objects based on the
    second argument below\dots }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object, 
         whatSense = c('point', 'plot', 'sausage'),
         ... )}
	\itemize{
		 		 
	  \item{\code{object}:}{ An object of one of class "chainSawIZ." }
		 
	  \item{\code{whatSense}:}{ 'point' returns the circular plot center
	  point; 'plot' returns the perimeter of the circular plot; 'sausage'
	  returns the whole-log inclusion zone.}
	  
	  \item{... :}{ Other arguments to be passed along--not used at present.}
	} %itemize
 } %item

  \item{\strong{\code{signature(object = "pointRelascopeIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    dual circle-shaped inclusion zone. }

  \item{\strong{\code{signature(object = "perpendicularDistanceIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    inclusion zone---works on all subclasses. }

  \item{\strong{\code{signature(object = "distanceLimitedIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    inclusion zone. }

  \item{\strong{\code{signature(object = "circularPlotIZ")}}}{ %\cr

	Returns the spatial object associated with the perimeter of the
    inclusion zone. }

  \item{\strong{\code{signature(object = "izContainer")}}}{% \cr

	Returns the spatial object associated with the bounding box for the
    collection of \dQuote{izContainer} subclass objects.}

  \item{\strong{\code{signature(object = "Tract")}}}{ %\cr

   Returns the spatial object associated with the perimeter of the
   \dQuote{Tract}. }

  \item{\strong{\code{signature(object = "sampSurf")}}}{%\cr

   Returns the spatial object associated with the perimeter of the
   \code{tract} slot in the object.}

}} %describe + section
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
