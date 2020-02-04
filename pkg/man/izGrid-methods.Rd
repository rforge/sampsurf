\name{izGrid-methods}
\docType{methods}
\alias{izGrid-methods}
%down logs...
\alias{izGrid,chainSawIZ,Tract-method}
\alias{izGrid,sausageIZ,Tract-method}
\alias{izGrid,fullChainSawIZ,Tract-method}
\alias{izGrid,standUpIZ,Tract-method}
\alias{izGrid,pointRelascopeIZ,Tract-method}
\alias{izGrid,perpendicularDistanceIZ,Tract-method}
\alias{izGrid,omnibusPDSIZ,Tract-method}
\alias{izGrid,distanceLimitedPDSIZ,Tract-method}
\alias{izGrid,omnibusDLPDSIZ,Tract-method}
\alias{izGrid,hybridDLPDSIZ,Tract-method}
\alias{izGrid,distanceLimitedIZ,Tract-method}
\alias{izGrid,distanceLimitedMCIZ,Tract-method}
%standing trees...
\alias{izGrid,circularPlotIZ,Tract-method}
\alias{izGrid,horizontalPointIZ,Tract-method} %not required, see cp
\alias{izGrid,horizontalLineIZ,Tract-method}
\alias{izGrid,horizontalPointCMCIZ,Tract-method}
\alias{izGrid,horizontalPointISIZ,Tract-method}
\alias{izGrid,horizontalPointCVIZ,Tract-method}
\alias{izGrid,horizontalPointMonteCarloSamplingIZ,Tract-method}
\alias{izGrid,criticalHeightIZ,Tract-method}
\alias{izGrid,importanceCHSIZ,Tract-method}
\alias{izGrid,antitheticICHSIZ,Tract-method}
\alias{izGrid,pairedAICHSIZ,Tract-method}
%other...
\alias{izGrid,matrix,Tract-method}




\title{ Methods for "InclusionZoneGrid" object construction in Package `sampSurf'}

\description{ The following methods will construct valid objects of
  class "\code{\linkS4class{InclusionZoneGrid}}" (or subclass) for
  different sampling protocols or methods based on the
  "\code{\linkS4class{InclusionZone}}" of the first signature
  argument. Please see \emph{"The InclusionZoneGrid Class"} vignette for
  more details and examples.}

\section{Methods}{

  The following constructors all share the formal arguments (after
  the two signature arguments) defined in the \code{standUpIZ}
  method. Any additional arguments are listed where appropriate. The
  \code{description} argument, will of course be different for each
  method.

  In addition, in what follows the constructor methods are organized by
  those applicable to sampling methods associated with \emph{(i)} down
  logs, \emph{(ii)} standing trees, and \emph{(iii)} other.



%============================== down logs ===================================  
\subsection{Down log constructors\ldots}{  
	
\describe{


%---------------------------------
\item{\strong{\code{signature(izObject = "standUpIZ", tract =
	  "Tract")}}}{%\cr

  This constructor is for the stand-up protocol.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{izGrid(izObject,
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
	    zone; \code{FALSE}: make the grid cover just the inclusion
	    zone. }
	  
	  \item{... :}{ Other arguments to be passed along as described in
        the final constructor method below (matrix signature argumnent).}	  
	} %itemize
 } %item


%---------------------------------
\item{\strong{\code{signature(izObject = "sausageIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the sausage method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{sausageIZ}}". }

	
%---------------------------------
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


%---------------------------------
\item{\strong{\code{signature(izObject = "fullChainSawIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the full chainsaw method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{fullChainSawIZ}}". Objects
	  created by this constructor are of class
	  "\code{\linkS4class{csFullInclusionZoneGrid}}," which is a
	  subclass of "\code{\linkS4class{InclusionZoneGrid}}" }

	
%---------------------------------
\item{\strong{\code{signature(izObject = "pointRelascopeIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the point relascope method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{pointRelascopeIZ}}". }

%---------------------------------
\item{\strong{\code{signature(izObject = "perpendicularDistanceIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the canonical PDS method of sampling down
	  logs. Everything is the same here as for the stand-up method
	  above, with the exception of the first argument, which must be an
	  object of class "\code{\linkS4class{perpendicularDistanceIZ}}". }

	
%---------------------------------
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

%---------------------------------
\item{\strong{\code{signature(izObject = "distanceLimitedPDSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited PDS (canonical,
	  omnibus or hybrid) method of sampling down logs. Everything is the
	  same here as for the "omnibusPDSIZ" method above, with the
	  exception of the first argument, which must be an object of class
	  "\code{\linkS4class{distanceLimitedPDSIZ}}". This method will also
	  work for objects of class
	  "\code{\linkS4class{omnibusDLPDSIZ}}" and
	  "\code{\linkS4class{hybridDLPDSIZ}}". }
	

%---------------------------------
\item{\strong{\code{signature(izObject = "distanceLimitedIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited method of sampling
	  down logs. Everything is the same here as for the "omnibusPDSIZ"
	  method above, with the exception of the first argument, which must
	  be an object of class "\code{\linkS4class{distanceLimitedIZ}}". }

%---------------------------------
\item{\strong{\code{signature(izObject = "distanceLimitedMCIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the distance limited  Monte Carlo
	  method of sampling down logs. Everything is the same here
	  as for the "distanceLimitedIZ" method above, with the exception of the
	  first argument, which must be an object of class
	  "\code{\linkS4class{distanceLimitedMCIZ}}". }
	
  } %describe
} %subsection down logs





%====================================== standing trees =================================
\subsection{Standing tree constructors\ldots}{
	
\describe{
  

	
\item{\strong{\code{signature(izObject = "circularPlotIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the circular plot and is also used directly for
	  "\code{horizontalPointIZ}" objects.  }

	
\item{\strong{\code{signature(izObject = "horizontalLineIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the horizontal line sampling
      ("\code{\linkS4class{horizontalLineIZ}}") method for standing trees.  }

	
\item{\strong{\code{signature(izObject = "horizontalPointMonteCarloSamplingIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for Monte Carlo subsampling coupled with the
      horizontal point sampling method for standing trees. It is
      applicable to objects of class
      "\code{\linkS4class{horizontalPointCMCIZ}}",
      "\code{\linkS4class{horizontalPointISIZ}}", and
      "\code{\linkS4class{horizontalPointCVIZ}}".	  
	}


	
\item{\strong{\code{signature(izObject = "criticalHeightIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the critical height sampling
      ("\code{\linkS4class{criticalHeightIZ}}") method for standing trees.  }

	
\item{\strong{\code{signature(izObject = "importanceCHSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the importance sampling variant to
      critical height sampling ("\code{\linkS4class{importanceCHSIZ}}")
      method for standing trees. }

	
\item{\strong{\code{signature(izObject = "antitheticICHSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the antithetic importance sampling variant to
      critical height sampling ("\code{\linkS4class{antitheticICHSIZ}}")
      method for standing trees. }
	
\item{\strong{\code{signature(izObject = "pairedAICHSIZ", tract =
	  "Tract")}}}{%\cr

      This constructor is for the paired antithetic importance sampling variant to
      critical height sampling ("\code{\linkS4class{pairedAICHSIZ}}")
      method for standing trees. }

  } %describe
} %subsection standing trees












	
%==================== matrix ===================================================
\subsection{Other constructors\ldots}{  
	
\describe{

\item{\strong{\code{signature(izObject = "matrix", tract =
	  "Tract")}}}{%\cr

      There is no need to use this constructor unless you are perhaps
	  developing a new sampling method for the package. It is a helper
	  function with some standard code that is required for most
	  methods. It is used to construct the encompassing background grid
	  object. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{izGrid(izObject,  
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
		
} %describe
} %subsection

} %section

\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
