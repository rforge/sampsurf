\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ArealSampling,missing-method}
\alias{plot,bufferedTract,missing-method}
\alias{plot,chainSawIZ,missing-method}
\alias{plot,circularPlot,missing-method}
\alias{plot,lineSegment,missing-method}
\alias{plot,downLog,missing-method}
\alias{plot,downLogIZs,missing-method}
\alias{plot,downLogs,missing-method}
\alias{plot,InclusionZone,missing-method}
\alias{plot,InclusionZoneGrid,missing-method}
\alias{plot,sampSurf,missing-method}
\alias{plot,sausageIZ,missing-method}
\alias{plot,pointRelascopeIZ,missing-method}
\alias{plot,perpendicularDistanceIZ,missing-method}
\alias{plot,distanceLimitedPDSIZ,missing-method}
\alias{plot,distanceLimitedIZ,missing-method}
\alias{plot,standUpIZ,missing-method}
\alias{plot,circularPlotIZ,missing-method}
\alias{plot,horizontalPointIZ,missing-method}
\alias{plot,criticalHeightIZ,missing-method}
\alias{plot,importanceCHSIZ,missing-method}
\alias{plot,antitheticICHSIZ,missing-method}
\alias{plot,pairedAICHSIZ,missing-method}
\alias{plot,horizontalLineIZ,missing-method}
\alias{plot,Stem,missing-method}
\alias{plot,StemContainer,missing-method}
\alias{plot,Tract,missing-method}

\title{ Methods for \pkg{graphics} Function \code{plot} in Package \sQuote{sampSurf} }

\description{
  The methods described here are for the \code{\link[sampSurf]{plot}}
  generic as used in the \pkg{sampSurf} package. Each of the methods
  shown below has different arguments that customize it for a different
  class of objects found in this package. Method dispatch is based on
  the signature \code{x,y} arguments, and often, the latter is
  "missing" so only the first argument is necessary to associate the
  appropriate method.
}

\section{Methods}{

The methods below are catagorized by their main signature argument,
which corresponds to one of the classes in package \dQuote{sampSurf}.

The following arguments may be used in all of the routines below. They
share the same definition in each method and are defined as\dots

  \itemize{
		 
	  \item{\code{add}:}{ \code{TRUE}: add the desired graphics to an
	    existing plot; \code{FALSE}: create a new plot.}
		 
	  \item{\code{asp}:}{ The \code{plot} \code{\link{asp}} aspect
	    parameter. Best to leave this at the default, which is good for
	    mapping displays.}
		 
	  \item{\code{axes}:}{ \code{TRUE}: plot axes; \code{FALSE}: no axes.}
	} %itemize

Some of the routines may specify a different default for these depending
upon the desired outcome. Obviously, the defaults can be overridden, and
they can be used in routines where they are not explicitly part of the
argument definition for the most part. 

}%section





%-------------------------------------------------------------------------
\section{ \emph{The \dQuote{ArealSampling} Class}}{

\describe{

\item{\strong{\code{signature(x = "ArealSampling", y =
	  "missing")}}}{%\cr 

    The base method for this class, it only plots the \code{location}
    slot of subclass objects. Note that since \dQuote{ArealSampling} is
    a virtual class, this method just provides very generic capabilities
    that will eventually be shared among all subclasses and will never
    therefore be called outright based on its signature (but only
    through \code{\link{callNextMethod}}).}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         pchIZCenter = 20,
         izCenterColor = .StemEnv$izCenterColor,
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is a subclass of \dQuote{ArealSampling}. }
		 
	  \item{\code{pchIZCenter}:}{ The \code{\link{pch}} parameter for
	  the inclusion zone center. }
		 
	  \item{\code{izCenterColor}:}{ An \R color to be used for the
		inclusion zone center point location.}
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
	    \code{\link[graphics]{plot}} or \code{\link{points}}.}
	} %itemize
 } %item
 


 \item{\strong{\code{signature(x = "circularPlot", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{circularPlot}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE, 
         izColor = .StemEnv$izColor,
         pchPlotCenter = 3, 
         showPlotCenter = FALSE,
         showPerimeter = TRUE,
         borderColor = .StemEnv$izBorderColor, 
         plotCenterColor = .StemEnv$izCenterColor, 
         asp = 1,
         ... )}
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{circularPlot} or any
	    subclass that might eventually be defined. }
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the plot's
	    inclusion zone.}
		 		 
	  \item{\code{pchPlotCenter}:}{ The \code{\link{pch}} value for the
	    plot center.}
		 
	  \item{\code{showPlotCenter}:}{ \code{TRUE}: show the plot center;
	    \code{FALSE}: do not plot it.}
		 
	  \item{\code{showPerimeter}:}{ \code{TRUE}: show the plot perimeter;
	    \code{FALSE}: do not plot it. Please note that the only way to
	    show the whole inclusion zone is to show th perimeter. If you
	    want either the interior of the zone or the perimeter but not
	    both, set the color for the one you do not want displayed to
	    \code{NA}. }
		 		 
	  \item{\code{borderColor}:}{ An \R color for the border/perimeter
	    of the plot's inclusion zone.}
		 		 
	  \item{\code{plotCenterColor}:}{ An \R color for the center point
	    of the plot's inclusion zone.}
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}.}
	  
	} %itemize
 } %item


 \item{\strong{\code{signature(x = "lineSegment", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{lineSegment}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x,
         axes = FALSE, 
         pchLineCenter = 20,
         showLineCenter = FALSE,
         showLineSegment = TRUE,
         lineColor = .StemEnv$izBorderColor, 
         lineCenterColor = .StemEnv$izCenterColor,
         asp = 1,
         ... ) }
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{lineSegment} or any
	    subclass that might eventually be defined. }
		 		 
	  \item{\code{pchLineCenter}:}{ The \code{\link{pch}} value for the
	    line center.}
		 
	  \item{\code{showLineCenter}:}{ \code{TRUE}: show the line center;
	    \code{FALSE}: do not plot it.}
		 		 
	  \item{\code{lineColor}:}{ An \R color for the line.}
		 		 
	  \item{\code{lineCenterColor}:}{ An \R color for the line's center point.}
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}.}
	  
	} %itemize
 } %item
		 
		 		 


	   

} %describe
} %section ArealSampling
%-------------------------------------------------------------------------






%-------------------------------------------------------------------------
\section{\emph{The \dQuote{InclusionZone} Class}}{

\describe{

  
\item{\strong{\code{signature(x = "InclusionZone", y = "missing")}}}{%\cr

    The base method for this class, it does not plot anything visible
    other than axes if desired; its role is to set up the extents of the
    plot from a subclass object's bounding box. Note that since
    \dQuote{InclusionZone} is a virtual class, this method just provides
    very generic capabilities that will eventually be shared among all
    subclasses and will never therefore be called outright based on its
    signature (but only through \code{\link{callNextMethod}}).  }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,        
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is a subclass of \dQuote{InclusionZone}. }
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}.}
	  
	} %itemize
 } %item
		 

\item{\strong{\code{signature(x = "standUpIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{standUpIZ}}". }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE, 
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,  
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{standUpIZ} or
	    any subclass that might be eventualy defined. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item

  

\item{\strong{\code{signature(x = "sausageIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{sausageIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE, 
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{sausageIZ} or
	    any subclass that might be eventualy defined. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item


  

\item{\strong{\code{signature(x = "chainSawIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{chainSawIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         showSliver = TRUE,
         ltySliver = 'dashed',
         sliverBorder = 'black',
         sliverColor = transparentColorBase('coral', .StemEnv$alphaTrans),
         showBolt = TRUE,
         ltyBolt = 'dotted',
         boltBorder =  transparentColorBase('grey20', .StemEnv$alphaTrans),
         add = FALSE,
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{chainSawIZ} or
	    any subclass that might be eventualy defined. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 
	  \item{\code{showSliver}:}{ \code{TRUE}: show the sliver
	    intersection area; \code{FALSE}: do not show it.}
		 
	  \item{\code{ltySliver}:}{ The line type (see: \code{\link{par}})
	    for the delineation of the sliver.}
		 
	  \item{\code{sliverBorder}:}{ An \R color for the border/perimeter
	    of the sliver section.}
		 
	  \item{\code{sliverColor}:}{ An \R color for the interior of the
	    sliver section; remember, this will plot on top of the log
	    interior color.}
		 
	  \item{\code{showBolt}:}{ \code{TRUE}: show the minimal bounding bolt
	    area; \code{FALSE}: do not show it.}
		 
	  \item{\code{ltyBolt}:}{ The line type (see: \code{\link{par}})
	    for the delineation of the bounding bolt.}
		 
	  \item{\code{boltBorder}:}{ An \R color for the border/perimeter
	    of the bounding bolt section.}
			
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item

  

\item{\strong{\code{signature(x = "pointRelascopeIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{pointRelascopeIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,    
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,    
         asp = 1,
         showDualCenters = FALSE, 
         dcColor = .StemEnv$izBorderColor, 
         ... )
}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{pointRelascopeIZ} or
	    any subclass that might be eventualy defined. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 
	  \item{\code{showDualCenters}:}{ TRUE: mark the dual circle
	  centers; FALSE: nothing.}
		 
	  \item{\code{dcColor}:}{ An \R color for the dual circle centers. }
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
  

\item{\strong{\code{signature(x = "perpendicularDistanceIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{perpendicularDistanceIZ}}" and subclasses (e.g.,
    "\code{\linkS4class{omnibusPDSIZ}}").}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,  
         asp = 1,
         ... )
}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{perpendicularDistanceIZ} or
	    subclass. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
  

\item{\strong{\code{signature(x = "distanceLimitedPDSIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{distanceLimitedPDSIZ}}" and subclasses (e.g.,
    "\code{\linkS4class{omnibusDLPDSIZ}}"). Please note that the different
    components of the inclusion zone can be displayed to show the
    transition between protocols (if any). In addition, the overall
    "\code{\linkS4class{perpendicularDistanceIZ}}" zone can also be
    displayed, showing what the inclusion zone would look like if the
    entire log were treated under the PDS protocol. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,  
         asp = 1,
         showFullPDSIZ = FALSE,
         showDLSPart = FALSE,
         showPDSPart = FALSE,
         ... )
}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{distanceLimitedPDSIZ} or
	    subclass. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 
	  \item{\code{showFullPDSIZ}:}{ \code{TRUE}: display the full PDS
	    inclusion zone (as if the log were sampled fully with PDS);
	    \code{FALSE}: do not display it. }
		 
	  \item{\code{showDLSPart}:}{ \code{TRUE}: display the
		  "\code{\linkS4class{distanceLimitedIZ}}" portion of the
		  inclusion zone (if any); \code{FALSE}: do not display it. }
		 
	  \item{\code{showPDSPart}:}{ \code{TRUE}: display the
		  "\code{\linkS4class{perpendicularDistanceIZ}}" portion of the
		  inclusion zone (if any); \code{FALSE}: do not display it. }
		  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
		 		 
  

\item{\strong{\code{signature(x = "distanceLimitedIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{distanceLimitedIZ}}" and subclass
    "\code{\linkS4class{distanceLimitedMCIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,  
         asp = 1,
         ... )
}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{distanceLimitedIZ} or
	    subclass. } 
		 
	  \item{\code{showLog}:}{ \code{TRUE}: plot the log object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
  

\item{\strong{\code{signature(x = "circularPlotIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{circularPlotIZ}}" as well as those of its
    subclass "\code{\linkS4class{horizontalPointIZ}}". In addition, the
    following subclasses derived from \dQuote{horizontalPointIZ} also
    use this method: "\code{\linkS4class{criticalHeightIZ}}",
    "\code{\linkS4class{importanceCHSIZ}}",
    "\code{\linkS4class{antitheticICHSIZ}}", and
    "\code{\linkS4class{pairedAICHSIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showTree = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,  
         asp = 1,
         ... )}
	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{circularPlotIZ} or
	    subclass. } 
		 
	  \item{\code{showTree}:}{ \code{TRUE}: plot the tree object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
		 
  

\item{\strong{\code{signature(x = "horizontalLineIZ", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot objects of class
     "\code{\linkS4class{horizontalLineIZ}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         showLineSegment = TRUE,
         ltyLineSegment = 'dashed',
         showTree = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,
         asp = 1,
         ... ) }

	   
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{horizontalLineIZ}. } 
		 
	  \item{\code{showLineSegment}:}{\code{TRUE}: show the line segment;
	    \code{FALSE}: do not show it. }
		 
	  \item{\code{ltyLineSegment}:}{ A valid \code{lty} value of the
	    \code{plot} parameter.}
	  
	  \item{\code{showTree}:}{ \code{TRUE}: plot the tree object;
	    \code{FALSE}: do not show it.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone.}
		 
	  \item{\code{izBorder}:}{ An \R color for the border/perimeter of
	    the inclusion zone.}
		 	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
		 

 
} %describe
} %section InclusionZone
%-------------------------------------------------------------------------










%-------------------------------------------------------------------------
\section{\emph{The \dQuote{InclusionZoneGrid} Class}}{

\describe{

\item{\strong{\code{signature(x = "InclusionZoneGrid", y =
	  "missing")}}}{%\cr

      The base method for this class, it actually supplies all of the
	  functionality for all existing subclasses, since the internal
	  \dQuote{InclusionZone} object will have its own plotting method
	  (see above), which will automatically be used, and this object is
	  all that really differentiates the objects of class
	  "\code{\linkS4class{InclusionZoneGrid}}". }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = TRUE,
         gridColor = .StemEnv$blue.colors(1000),
         asp = 1,
         izColor = NA,
         gridLines = TRUE,
         gridLineColor = .StemEnv$gridLineColor,
         gridCenters = FALSE, 
         gridCenterColor = .StemEnv$gridCenterColor,
         estimate = names(.StemEnv$puaEstimates),
         lwdGrid = 1, 
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{InclusionZoneGrid} or
	    any subclass that might be eventualy defined. }
		 
	  \item{\code{gridColor}:}{ A valid \R color for the background
	    grid. Note that a single color is fine for most sampling
	    methods, but for an inclusion zone whose surface varies within a
	    single object like the \dQuote{chainSawIZ}, you must supply a
	    \code{\link{colorRampPalette}} in order to see the variation
	    correctly.}
		 		 
	  \item{\code{izColor}:}{ An \R color for the interior of the
	    inclusion zone. Note that the default is set to \code{NA} so that no
	    filling is done within the inclusion zone to show the grid more
	    clearly. }
		 
	  \item{\code{gridLines}:}{ \code{TRUE}: show grid lines delineated
	    for more emphasis on the individual grid cells; \code{FALSE}: no
	    grid lines. Note that this can get busy if the area is
	    too large.}
		 
	  \item{\code{gridLineColor}:}{ An \R color for the grid lines.}
		 
	  \item{\code{gridCenters}:}{ \code{TRUE}: show grid cell centers 
	    within the individual grid cells; \code{FALSE}: no
	    grid cell centers. Note that this can get busy if the area is
	    too large.}
		 
	  \item{\code{gridCenterColor}:}{ An \R color for the grid cell
	    centers.} 
		 
	  \item{\code{estimate}:}{ A character variable corresponding to the
	  desired estimate: either "cubicVolume" (default) or "Density" at present.}
		 
	  \item{\code{lwdGrid}:}{ The graphics parameter line width (see:
	  \code{\link{par}}) for the grid lines.}
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	    \code{\link[graphics]{plot}}, or to the plot methods for the
	    individual objects (i.e., \dQuote{InclusionZone},
	    \dQuote{downLog}).}
			  
	} %itemize
 } %item

	

} %describe
} %section InclusionZoneGrid
%-------------------------------------------------------------------------









%-------------------------------------------------------------------------
\section{\emph{The \dQuote{izContainer} Class}}{

\describe{

	  

\item{\strong{\code{signature(x = "izContainer", y = "missing")}}}{%\cr

    This adds the necessary functionality to plot subclass objects of
    container class "\code{\linkS4class{izContainer}}", including both
    "\code{\linkS4class{downLogIZs}}" and
    "\code{\linkS4class{standingTreeIZs}}" . Note that arguments passed
    in this call that are destined for individual \dQuote{downLogIZ} or
    \dQuote{downLog} objects within the collection will apply to
    \emph{all} objects of that class within the collection; similarly
    for standing tree components.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,  
         add = FALSE, 
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of subclass \dQuote{izContainer}. }
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual objects.}
	  
	} %itemize
 } %item
	  
} %describe
} %section izContainer
%-------------------------------------------------------------------------





  

%-------------------------------------------------------------------------
\section{\emph{The \dQuote{sampSurf} Class}}{

\describe{

  \item{\strong{\code{signature(x = "sampSurf", y = "missing")}}}{%\cr

	This is the only extant method for plotting objects of class
    "\code{\linkS4class{sampSurf}}." It makes use of many of the
    plotting methods for the other objects in the \pkg{sampSurf}
    package, some of which also rely on plot methods from the
    \pkg{raster} package.  }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         showIZs = TRUE,
         izColor = NA,  
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{sampSurf} or
	    any subclass that might be eventualy defined. }
		 
	  \item{\code{showIZs}:}{ \code{TRUE}: plot the inclusion zones on
	    the surface; \code{FALSE}: just plot the surface. }
		 
	  \item{\code{izColor}:}{ This should always be \code{NA} or some
	    very transparent color so the interior of the inclusion zones
	    does not obscure the sampling surface details. }
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	     the plot methods for the individual objects (e.g..,
	     \dQuote{Tract}, \dQuote{InclusionZone}, \dQuote{downLog}).}
			  
	} %itemize
 } %item

	  
} %describe
} %section sampSurf
%-------------------------------------------------------------------------





  

%-------------------------------------------------------------------------
\section{\emph{The \dQuote{Stem} Class}}{

\describe{

  \item{\strong{\code{signature(x = "Stem", y = "missing")}}}{%\cr

	The base method for this class, it only plots the \code{location}
    slot of subclass objects. Note that since \dQuote{Stem} is a virtual
    class, this method just provides very generic capabilities that will
    eventually be shared among all subclasses and will never therefore
    be called outright based on its signature (but only through
    \code{\link{callNextMethod}}). }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         pchStemLocation = 20,    
         stemLocationColor = .StemEnv$logAttributeColor,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{Stem} or
	    any subclass that might be eventualy defined. }
		 
	  \item{\code{pchStemLocation}:}{ The \code{\link{pch}} parameter
	  for the location of the stem center.}
		 
	  \item{\code{stemLocationColor}:}{ The color for the symbol marking
	    the stem/log location.}
	  
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link{points}}.}
	  
	} %itemize
 } %item

	
  
 \item{\strong{\code{signature(x = "downLog", y = "missing")}}}{%\cr

   This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{downLog}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x,
         axes = FALSE, 
         logColor = .StemEnv$logColor,	 
         showLogCenter = FALSE,
         pchLogCenter = 3,     
         logCenterColor = .StemEnv$logAttributeColor,
         showNeedle = FALSE,
         logNeedleColor = .StemEnv$logAttributeColor,
         logBorderColor = .StemEnv$logBorderColor,   
         asp = 1,
         ...
        )}
	\itemize{
		 
	  \item{\code{x}:}{ An object of class \dQuote{downLog} or any
	    subclass that might eventually be defined.}
		 
	  \item{\code{logColor}:}{ An \R color to be used for shading the log.}
		 
	  \item{\code{showLogCenter}:}{ \code{TRUE}: plot log center
	    location; \code{FALSE}: do not plot it.}
		 
	  \item{\code{pchLogCenter}:}{ The \code{\link{pch}} value for the
	    log center.}
		 
	  \item{\code{logCenterColor}:}{ An \R color to be used for the log
		center point location.}
		 
	  \item{\code{showNeedle}:}{ \code{TRUE}: plot log `needle;'
	    \code{FALSE}: do not plot it.}
		 
	  \item{\code{logNeedleColor}:}{ An \R color to be used for the log
		'needle.'}
		 
	  \item{\code{logBorderColor}:}{ An \R color to be used for drawing
	    the perimeter of the log.}
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	    \code{\link[graphics]{plot}}.}
		
	} %itemize
  } %item

	
  
 \item{\strong{\code{signature(x = "standingTree", y = "missing")}}}{%\cr

   This adds the necessary functionality to plot objects of class
    "\code{\linkS4class{standingTree}}".}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x,
         axes = FALSE,  #not a par() so can't be passed to callNextMethod, so separate it
         treeColor = .StemEnv$treeColor,
         showLocation = TRUE,
         pchLocation = 3,       #20 is also good
         locationColor = .StemEnv$treeAttributeColor,
         treeBorderColor = .StemEnv$treeBorderColor,   #log perimeter color
         asp = 1,
         ...
        )}
	\itemize{
		 
	  \item{\code{x}:}{ An object of class \dQuote{downLog} or any
	    subclass that might eventually be defined.}
		 
	  \item{\code{treeColor}:}{ An \R color to be used for shading the tree.}
		 
	  \item{\code{showLocation}:}{ \code{TRUE}: plot tree center
	    location; \code{FALSE}: do not plot it.}
		 
	  \item{\code{pchLocation}:}{ The \code{\link{pch}} value for the
	    tree center.}
		 
	  \item{\code{locationColor}:}{ An \R color to be used for the tree
		center point location.}
		 
	  \item{\code{treeBorderColor}:}{ An \R color to be used for drawing
	    the perimeter of the tree.}
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	    \code{\link[graphics]{plot}}.}
		
	} %itemize
  } %item
  

} %describe
} %section Stem
%-------------------------------------------------------------------------


  

%-------------------------------------------------------------------------
\section{\emph{The \dQuote{StemContainer} Class}}{

\describe{


  \item{\strong{\code{signature(x = "StemContainer", y = "missing")}}}{%\cr

	The base method for this class. Note that since \dQuote{StemContainer} is a virtual
    class, this method just provides very generic capabilities that are
    shared among all subclasses and will never therefore
    be called outright based on its signature (but only through
    \code{\link{callNextMethod}}). Currently, it just sets up the
    overall extents for the plot area. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE, 
         add = FALSE, 
         asp = 1,
         ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is of class \dQuote{Stem} or
	    any subclass that might be eventualy defined. }
	  
	  \item{\dots :}{ Other graphics arguments to be passed on.}
	  
	} %itemize
 } %item

 
  
  \item{\strong{\code{signature(x = "downLogs", y = "missing")}}}{%\cr

	This adds the necessary functionality to plot objects of container
    class "\code{\linkS4class{downLogs}}". Note that arguments passed in
    this call that are destined for individual \dQuote{downLog} objects
    within the collection will apply to \emph{all} logs within the
    collection.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,
         add = FALSE,
         asp = 1,
         ... )}
	\itemize{
		 		 
	  \item{\code{x}:}{ An object that is of class 
	    \dQuote{downLogs} or subclass thereof. }
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual downLog objects (please see above).}
	  
	} %itemize
 } %item
 
  
  \item{\strong{\code{signature(x = "standingTrees", y = "missing")}}}{%\cr

	This adds the necessary functionality to plot objects of container
    class "\code{\linkS4class{standingTrees}}". Note that arguments passed in
    this call that are destined for individual \dQuote{standingTree} objects
    within the collection will apply to \emph{all} trees within the
    collection.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x, 
         axes = FALSE,
         add = FALSE,
         asp = 1,
         ... )}
	\itemize{
		 		 
	  \item{\code{x}:}{ An object that is of class 
	    \dQuote{standingTrees} or subclass thereof. }
		
	  \item{\dots :}{ Other graphics arguments to be passed on to
		\code{\link[graphics]{plot}}, or to the plot methods for the
		individual standingTree objects (please see above).}
	  
	} %itemize
 } %item
  

} %describe
} %section StemContainer
%-------------------------------------------------------------------------


  

%-------------------------------------------------------------------------
\section{\emph{The \dQuote{Tract} Class}}{

\describe{

  \item{\strong{\code{signature(x = "Tract", y = "missing")}}}{%\cr

	This method is the base plot method for objects of class
    "\code{\linkS4class{Tract}}" and subclasses. Because \dQuote{Tract}
    objects are a subclass of "\code{\linkS4class{RasterLayer}}," all of
    the plotting is done through calls to those methods. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x,
         axes = TRUE,  
         gridColor = .StemEnv$blue.colors(1000),
         asp = 1,
         useImage = TRUE,
         ... )}
	\itemize{
		 		 
	  \item{\code{x}:}{ An object that is of class \dQuote{Tract} or
	    subclass thereof. }
	  		 
	  \item{\code{gridColor}:}{ A valid \R color for the background grid. Note
	    that a single color is fine for initial blank grids, but you can
	    also supply a function from \code{\link{colorRampPalette}} in
	    order to see and surface variation that is in the tract
	    correctly; this is the default in shades of blue.}
	  		 
	  \item{\code{useImage}:}{ \code{TRUE}: use the
	    raster::\code{\link[raster]{image}} command to draw the tract
	    (default); \code{FALSE}: use the
	    raster::\code{\link[raster]{plot}} command instead, which gives
	    the gradient legend. This is an option because the latter will
	    not rezise the graphics correctly on a graphics window, though
	    it is fine for a hardcopy and if you do not resize the
	    device. This may be fixed in \code{raster} eventually
	    (Dec-2010).}
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	    raster::\code{\link[raster]{plot}}. In addition, one can add
	    grid lines to the tract via arguments passed to
	    \code{\link{gridCellEnhance}}.}
	  
	} %itemize
 } %item



\item{\strong{\code{signature(x = "bufferedTract", y = "missing")}}}{%\cr

  This is the plot method for objects of class
  "\code{\linkS4class{bufferedTract}}" and subclasses. It essentially
  calls the bas \dQuote{Tract} method for all of the work and adds the
  buffer. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(x,
         bufferColor = transparentColorBase('blanchedalmond', .StemEnv$alphaTrans),
         axes = TRUE, 
         gridColor = .StemEnv$blue.colors(1000),      
         lwd = 2,
         asp = 1,
         ... )}
	\itemize{
		 		 
	  \item{\code{x}:}{ An object that is of class \dQuote{bufferedTract} or
	    subclass thereof. }
		 		 
	  \item{\code{bufferColor}:}{ The color for the buffer area.}
	  		 
	  \item{\code{gridColor}:}{ A valid \R color for the background grid. Note
	    that a single color is fine for initial blank grids, but you can
	    also supply a function from \code{\link{colorRampPalette}} in
	    order to see and surface variation that is in the tract
	    correctly; this is the default in shades of blue.}
		 		 
	  \item{\code{lwd}:}{ The line width for the border (see:
	    \code{\link{par}}).} 
		 
	  \item{\code{\dots}:}{ Other graphical arguments to be passed on to
	    raster::\code{\link[raster]{plot}}. In addition, one can add
	    grid lines to the tract via arguments passed to
	    \code{\link{gridCellEnhance}}.}
	  
	} %itemize
 } %item



} %describe
} %section Tract
%-------------------------------------------------------------------------




\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
