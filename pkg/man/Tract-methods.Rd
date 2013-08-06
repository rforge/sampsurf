\name{Tract-methods}
\docType{methods}
\alias{Tract-methods}
\alias{Tract,matrix-method}
\alias{Tract,missing-method}
\alias{Tract,numeric-method}
\alias{Tract,RasterLayer-method}

\title{   Methods for "Tract" Object Construction in package
  \sQuote{sampSurf} }

\description{ There are several methods available for the
  \code{\link{Tract}} generic function, which will generate valid
  objects of class \code{"\linkS4class{Tract}"}. These constructors are
  the preferred method for generating such objects instead of using
  \code{\link{new}}. Other constructors could be added by extention to
  these method as required.  }

\section{Methods}{
\describe{
	
  \item{\strong{\code{signature(object = "missing")}}}{%\cr

	Several of the constructors listed below end up calling this one for
	final object construction, so it could be considered the base
	constructor. It is patterned after the construction of a
	\code{"\linkS4class{GridTopology}"} object in the
	\sQuote{\code{\link{sp}}} package. Please note that it is best to
	either explicitly name the arguments when using this constructor, or
	to include an initial missing argument (see examples in the generic)
	to invoke it, since supplying an unnamed numeric scalar as the first
	argument will instead, invoke the second constructor below (with an
	error).}
  
  \item{}{\bold{usage\ldots}
    \preformatted{Tract(cellSize,
      cellDims = c(x=100, y=100), 
      cellCenter = NULL,
      units = 'metric',
      data = 0,
      spUnits = CRS(projargs=as.character(NA)),
      description = 'object of class Tract',
      runQuiet = FALSE,
      ... )}
	
	\itemize{
	  \item{\code{cellSize}:}{ The size of the individual grid cells in the
		appropriate units. Note particularly that the restriction has
        been made that grid cells must be square, so this is expecting a 
        numeric scalar.  }
	  
	  \item{\code{cellDims}:}{ The dimensions in terms of number of cells in
        the x and y directions. The to match a desired extent in terms
        of the units of x and y, one must take the \code{cellSize} into
        account when determining these limits. For example, if the
        extent in the x and y directions are to be 10 units, and the
        \code{cellSize} is 0.5 units, then the specify this argument as
        \code{c(x=20,y=20)}. }
	  
	  \item{\code{cellCenter}:}{ This is the center point for the grid cell at
	    the origin of the tract. If the \code{cellSize} is 0.5 units and
	    the minimum in x and y is (0,0), then this would be
	    \code{c(x=0.25, y=0.25)}. Note that we can specify lower limts
	    other than zero for the tract using this method. }
	  
	  \item{\code{units}:}{ Either \dQuote{English} or \dQuote{metric}. These
	    must be conformable with the projection in \code{spUnits}.}
	  
	  \item{\code{data}:}{ The default is to set all grid cells to zero. One
	    can either specify a numeric scalar which will be replicated for
	    all cells, or a numeric vector the same length as the number of
	    grid cells; these will be assigned in sequence to the grid. }

	  \item{\code{spUnits}:}{ A valid \code{\link{CRS}} object
        specifying the Coordinate Reference System. This defaults to
        \code{NA}, which means you want to use your own user-defined
        system, say for a sample plot located in the field.}

	  \item{\code{description}:}{ A character vector description of the
		tract.}

	  \item{\code{runQuiet}:}{ TRUE: suppress any messages; FALSE: echo
	    messages.} 
	  
	  \item{\code{\dots}:}{ Other arguments to be passed along (not used currently).}
	} %itemize
  } %item

	

  \item{\strong{\code{signature(object = "numeric")}}}{%\cr

	In this second constructor, the \code{object} argument should be a
    numeric vector specifying the \emph{maximum extents} of the
    tract. The tract minimum is always assumed to be at (0,0) for this
    method. If some other minima is desired, use one of the other
    constructors.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{Tract(object = c(x=10, y=10),
      cellSize,
      units = 'metric',
      data = 0,
      spUnits = CRS(projargs=as.character(NA)),
      description = 'object of class Tract',
      runQuiet = FALSE,
      ... )}
	
	\itemize{
	  \item{\code{object}:}{ A length two vector with names \code{c("x",
        "y")} specifying the maximal extents of the tract in these
        coordinates.}
	  
	  \item{\code{runQuiet}:}{ TRUE: suppress any messages; FALSE: echo
	    messages.} 
	  
	  \item{\code{\dots}:}{ Other arguments to be passed along.}

	  \item{}{ All other arguments as defined for the other constructors
	    above.} 
	} %itemize
  } %item



  
  
  \item{\strong{\code{signature(object = "matrix")}}}{%\cr

	This construtor allows creation of a \dQuote{Tract} object using a
    bounding box specification.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{Tract(object, 
      cellSize,
      units = 'metric',
      data = 0,
      spUnits = CRS(projargs=as.character(NA)),
      description = 'object of class Tract',
      runQuiet = FALSE,
      ... )}
	 
	\itemize{
	  \item{\code{object}:}{ A 2x2 matrix with rownames
        \code{c("x","y")} and column names \code{c("min","max")}. The
	    matrix therefore specifies the bounding box extents in the
	    plane. The origin can be anywhere.}

	  \item{}{ All other arguments as defined for the other constructors
	    above.} 
	} %itemize
  } %item



  
  \item{\strong{\code{signature(object = "RasterLayer")}}}{%\cr

	Finally this constructor takes an object of class
    \dQuote{RasterLayer}.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{Tract(object, 
      units = 'metric',
      description = 'object of class Tract',
      runQuiet = FALSE,
      ... )}
	 
	\itemize{
	  \item{\code{object}:}{ A valid object of type
	  \code{"\linkS4class{RasterLayer}"}.}

	  \item{}{ All other arguments as defined for the other constructors
	    above.} 

	} %itemize
  } %item

  
}} %section/describe



\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
