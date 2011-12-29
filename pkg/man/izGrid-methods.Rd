\name{izGrid-methods}
\docType{methods}
\alias{izGrid-methods}
\alias{izGrid,chainSawIZ,Tract-method}
\alias{izGrid,matrix,Tract-method}
\alias{izGrid,sausageIZ,Tract-method}
\alias{izGrid,standUpIZ,Tract-method}
\alias{izGrid,pointRelascopeIZ,Tract-method}
\alias{izGrid,perpendicularDistanceIZ,Tract-method}
\alias{izGrid,omnibusPDSIZ,Tract-method}
\alias{izGrid,distanceLimitedPDSIZ,Tract-method}
\alias{izGrid,distanceLimitedMCIZ,Tract-method}
\alias{izGrid,circularPlotIZ,Tract-method}

\title{ Methods for "InclusionZoneGrid" object construction in Package `sampSurf'}

\description{ The following methods will construct valid objects of
  class "\code{\linkS4class{InclusionZoneGrid}}" for different sampling
  protocols or methods based on the "\code{\linkS4class{InclusionZone}}"
  of the first signature argument. Please see \emph{"The
  InclusionZoneGrid Class"} vignette for more details and examples. }

\section{Methods}{

  The following constructors all share the formal arguments (after
  the two signature arguments) defined in the \code{standUpIZ}
  method. Any additional arguments are listed where appropriate.
	
\describe{

\item{\strong{\code{signature(izObject = "standUpIZ", tract =
	  "Tract")}}}{%\cr

  This constructor is for the stand-up protocol.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(izObject,
         tract,
         description = 'standUpIZ inclusion zone grid object',
         wholeIZ = TRUE,  
         ... )}
	\itemize{
		 
	  \item{\code{izObject}:}{ An object of class "standUpIZ."} 
		 
	  \item{\code{tract}:}{ An object of class "Tract" or subclass.}
		 
	  \item{\code{description}:}{ A description of the object as a
	  character string.}
		 
	  \item{\code{wholeIZ}:}{ \code{TRUE}: make a background grid
	  covering the entire object including the stem and the inclusion
	  zone; \code{FALSE}: make the grid cover just the inclusion zone. } 
	  
	  \item{... :}{ Other arguments to be passed along as described in
        the final constructor method below (matrix signature argumnent).}	  
	} %itemize
 } %item

\item{\strong{\code{signature(izObject = "sausageIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the sausage method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{sausageIZ}}". }

\item{\strong{\code{signature(izObject = "chainSawIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the chainsaw method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{chainSawIZ}}". Now, one
	  important thing to keep in mind is that the inclusion zone for the
	  chainsaw method is a point (within a grid cell), so setting
	  \code{wholeIZ=FALSE} here will give just the one grid cell that
	  corresponds to the plot center point as a background
	  grid. \emph{Note:} If you want to enumerate all cells withing a
	  sausage-shaped inclusion zone under the chainsaw method, see
	  instead the "\code{\linkS4class{csFullInclusionZoneGrid}}"
	  class. }

\item{\strong{\code{signature(izObject = "pointRelascopeIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the point relascope method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{pointRelascopeIZ}}". }

\item{\strong{\code{signature(izObject = "perpendicularDistanceIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the canonical PDS method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{perpendicularDistanceIZ}}". }

	
\item{\strong{\code{signature(izObject = "omnibusPDSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the omnibus PDS method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{omnibusPDSIZ}}" and the
	  addition of the following argument\dots }
	\itemize{
		 
	  \item{\code{runQuiet}:}{ \code{TRUE}: no feedback on progress;
        \code{FALSE}: some feedback on progress} 
    }	

\item{\strong{\code{signature(izObject = "distanceLimitedPDSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited PDS (canonical,
	  omnibus or hybrid) method of sampling down logs. Everything is the
	  same here as for the "omnibusPDSIZ" method above, with the
	  exception of the first argument, which must be an object of class
	  "\code{\linkS4class{distanceLimitedPDSIZ}}". }
	

\item{\strong{\code{signature(izObject = "distanceLimitedIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited method of sampling
	  down logs. Everything is the same here as for the "omnibusPDSIZ"
	  method above, with the exception of the first argument, which must
	  be an object of class "\code{\linkS4class{distanceLimitedIZ}}". }

\item{\strong{\code{signature(izObject = "distanceLimitedMCIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited  Monte Carlo
	  method of sampling down logs. Everything is the same here
	  as for the "distanceLimitedIZ" method above, with the exception of the
	  first argument, which must be an object of class
	  "\code{\linkS4class{distanceLimitedMCIZ}}". }

	
\item{\strong{\code{signature(izObject = "circularPlotIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the circular plot and horizontal point
	  sampling (a subclass of "circularPlotIZ") methods for standing
	  trees.  }


	
\item{\strong{\code{signature(izObject = "matrix", tract =
	  "Tract")}}}{%\cr

      There is no need to use this constructor unless you are perhaps
	  developing a new sampling method for the package. It is a helper
	  function with some standard code that is required for most
	  methods. It is used to construct the encompassing background grid
	  object. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(izObject,  
         tract,
         data = 0,
         useCrop = TRUE, 
         ... )}
	\itemize{
		 
	  \item{\code{izObject}:}{ This must be a \code{\link{bbox}} matrix
	    taken directly from the respective "InclusionZone" object. }
		 
	  \item{\code{tract}:}{ An object of class "Tract" or subclass.}
		 
	  \item{\code{data}:}{ A scalar value for the background cells in
	  the encompassing bounding grid that will be created. }
		 
	  \item{\code{useCrop}:}{ \code{TRUE}: use the \pkg{raster} \code{\link{crop}}
	  function; \code{FALSE}: use the alternative method (see the source
	  code for details).}
	  
	} %itemize
 } %item
		
}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
