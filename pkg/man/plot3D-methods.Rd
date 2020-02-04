\name{plot3D-methods}
\docType{methods}
\alias{plot3D-methods}
\alias{plot3D,InclusionZoneGrid-method}
%\alias{plot3D,RasterLayer-method}
\alias{plot3D,sampSurf-method}
\alias{plot3D,Tract-method}

\title{ Methods for Function \code{plot3D} in Package \pkg{sampSurf} }

\description{
  
  The methods described here are for classes defined in the
  \pkg{sampSurf} package, based on the generic defined in the
  rasterVis::\code{\link[rasterVis]{plot3D}} package. Please see the
  generic and its methods in the \pkg{rasterVis} package for definitions
  of other arguments that are not described here. Especially please note
  below that the \code{col} argument is a pallette generating
  \emph{function}, not a simple \R color. Also, the \dots argument will pass
  along arguments defined in the generic function as usual.  }

\section{Methods}{
  
\describe{

  
\item{\strong{\code{signature(x = "InclusionZoneGrid")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{plot3D(x,
       estimate = 'volume',
       col = .StemEnv$blue.colors,
       ...)}
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{InclusionZoneGrid}. }

	  \item{\code{estimate}:}{ Any legal attribute estimate that is
	  found in the object's \code{data} slot. This will normally include the
	  \code{.StemEnv$puaEstimates} and \code{.StemEnv$ppEstimates}
	  attributes, though not all may apply to any given sampling
	  method. }
	} %itemize
 } %item
		 

	  
 \item{\strong{\code{signature(x = "sampSurf")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{plot3D(x,
       col = .StemEnv$blue.colors,
       ...)}
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{sampSurf}. }
	} %itemize
 } %item




\item{\strong{\code{signature(x = "Tract")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{plot3D(x,
       col = .StemEnv$blue.colors,
       ...)}
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{Tract} or subclass. }
	} %itemize
 } %item


}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
