\name{hist-methods}
\docType{methods}
\alias{hist-methods}
%\alias{hist,ANY-method}
%\alias{hist,downLogs-method}
\alias{hist,izContainer-method}
\alias{hist,InclusionZoneGrid-method}
\alias{hist,RasterLayer-method}
\alias{hist,RasterStackBrick-method}
\alias{hist,sampSurf-method}
\alias{hist,mcsContainer-method}
\alias{hist,antitheticContainer-method}

\title{ Methods for \pkg{graphics} Function \code{hist} in Package
  \pkg{sampSurf} }

\description{
  The methods described here are for the \code{\link[sampSurf]{hist}}
  generic as used in the \pkg{sampSurf} package. Several of the methods
  shown below have different arguments that customize each for a different
  class of objects found in this package. Method dispatch is based on
  the signature \code{x} argument.
}

\section{Methods}{

Some arguments that are in the graphics::\code{\link[graphics]{hist}}
method are given default values in the calls below. Please refer to the
graphics documentation for details.
  
\describe{

\item{\strong{\code{signature(x = "downLogs")}}}{

    Creates a histogram of any one of several down log attributes in the
    container collection as specified by the \code{logAttr} argument.}  
  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x, 
     logAttr = c('logLen','buttDiam','topDiam','logAngle','solidType','logVol',
                 'surfaceArea','coverageArea','biomass','carbon'),
     xlab = logAttr,
     main = NA,
     col = 'gray90',
     ... )}

	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{downLogs}. }
		 
	  \item{\code{logAttr}:}{ Any of these can be used for the histogram.}
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	} %itemize
 } %item


 
\item{\strong{\code{signature(x = "standingTrees")}}}{

    Creates a histogram of any one of several standing tree attributes in the
    container collection as specified by the \code{treeAttr} argument.}  
  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x, 
     treeAttr = c('height','buttDiam','dbh','topDiam','solidType','treeVol',
                  'surfaceArea','biomass','carbon'),
     xlab = treeAttr,
     main = NA,
     col = 'gray90',
     ... )}
	
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{standingTrees}. }
		 
	  \item{\code{treeAttr}:}{ Any of these can be used for the histogram.}
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	} %itemize
 } %item



 \item{\strong{\code{signature(x = "izContainer")}}}{

   Creates a histogram of inclusion zone areas for the objects in the
   container---this will work for all subclasses of \dQuote{izContainer}. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x, 
     xlab = 'Inclusion Zone Area',
     main = NA,
     col = 'gray90',
     ... )}
	
	\itemize{
		 
	  \item{\code{x}:}{ An object that is a subclass of
	    \dQuote{izContainer}, such as \dQuote{downLogIZs}. }
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	} %itemize
 } %item



 \item{\strong{\code{signature(x = "InclusionZoneGrid")}}}{

   Creates a histogram of the desired attribute for the object. Please
   note that it is not uncommon for the surface to be flat, which means
   all grid cells have the same value, so that a histogram is really
   meaningless. Use this on classes of objects where the surface
   (attribute estimate) varies within an individual inclusion zone. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x,
     zeroTrunc = TRUE,
     estimate = 'volume',
     main = NA,
     xlab = NA,
     col = 'gray90',
     ... )}
	
	\itemize{
		 
	  \item{\code{x}:}{ An object that is a subclass of
	    \dQuote{InclusionZoneGrid}. }
	  		 
	  \item{\code{zeroTrunc}:}{ \code{TRUE}: exclude background (zero)
	    cells; \code{FALSE}: include them.}
		 
	  \item{\code{main}:}{ The title is blank by default.}
	  		 
	  \item{\code{xlab}:}{ The x label is taken from the \code{estimate}
	    argument by default.}
	  		 
	  \item{\code{estimate}:}{ The attribute of interest that the
	     histogram will represent. }
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	} %itemize
 } %item


 
 \item{\strong{\code{signature(x = "sampSurf")}}}{
   
   Creates a histogram of the sampling distibution of the surface from
   the individual cell values.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x, 
     zeroTrunc = TRUE,
     xlab = x@estimate,
     main = NA,
     col = 'gray90',
     ... )}
	
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{sampSurf}. }
		 
	  \item{\code{zeroTrunc}:}{ \code{TRUE}: exclude all zero-valued
	    background cells (default); \code{FALSE}: include all cells.}
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	} %itemize
 } %item
 


} %describe

\subsection{Monte Carlo methods}{

  
\describe{

  \item{\strong{\code{signature(x = "mcsContainer")}}}{

	Histograms of the relative error or volume variance over the
	collection are supported. Please note
	that this method applies to objects of class
	\dQuote{antitheticContainer} as well.}

  
  \item{}{\bold{usage\ldots}
    \preformatted{hist(x,
     stat = c('relErrPct', 'volVar'),
     xlab = stat,
     main = NA,
     col = 'gray90',
     ... )}
	
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{mcsContainer}. }
		 
	  \item{\code{stat}:}{ Relative error in percent on volume (default), or
	    variance on volume.}
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{hist}}.}
	  
	} %itemize
  } %item

  } %describe

 } %subsection MC

} %section-methods
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
