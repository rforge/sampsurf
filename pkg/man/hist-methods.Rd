\name{hist-methods}
\docType{methods}
\alias{hist-methods}
%\alias{hist,ANY-method}
%\alias{hist,downLogs-method}
\alias{hist,izContainer-method}
\alias{hist,RasterLayer-method}
\alias{hist,RasterStackBrick-method}
\alias{hist,sampSurf-method}

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
    \preformatted{function(x, 
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
    \preformatted{function(x, 
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
    \preformatted{function(x, 
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


 
 \item{\strong{\code{signature(x = "sampSurf")}}}{
   
   Creates a histogram of the sampling distibution of the surface from
   the individual cell values.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
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
 


}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
