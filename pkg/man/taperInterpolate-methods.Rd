\name{taperInterpolate-methods}
\docType{methods}
\alias{taperInterpolate-methods}
\alias{taperInterpolate,downLog-method}
\alias{taperInterpolate,list-method}
\alias{taperInterpolate,standingTree-method}

\title{ Methods for Function \code{taperInterpolate} in Package
  \pkg{sampSurf} }

\description{
  There are three methods for the  \code{taperInterpolate} generic
  function keyed off the one \code{object} signature. Each of them share
  the same argument list, but with one argument having slightly
  different meanings between the methods. The method associated with
  \code{list} objects should \emph{not} be used unless you know what you
  are doing. Please use the wrappers in the other two methods below,
  which set the correct list structure up, and call the \code{list}
  version to do all the calculations.
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(object = "downLog")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{taperInterpolate(object,
                 whichSense = c('diameter', 'length'),
                 pts = NULL,
                 ...) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{downLog}.}
		
      \item{\code{whichSense}:}{ Either "diameter" to interpolate diameters, or
	    "length" to interpolate lengths.  }
  
      \item{\code{pts}:}{ If \code{whichSense = "diameter"} then this
	    is a vector of lengths at which to interpolate the
	    diameters. Alternatively, if \code{whichSense = "length"} then
	    this is a vector of diameters at which to interpolate the
	    lengths.  }
	  
	} %itemize
  } %item
  


  \item{\strong{\code{signature(object = "list")}}}{ %

     Please do not use this method, use the other two which call it correctly\ldots}
  
  \item{}{\bold{usage\ldots}
    \preformatted{taperInterpolate(object,
                 whichSense = c('diameter', 'length', 'height'),
                 pts = NULL,
                 ...) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{list}.}
			  
      \item{\code{whichSense}:}{ Either \dQuote{diameter} to interpolate diameters, 
	    \dQuote{length} to interpolate lengths for \dQuote{downLog}
	    objects, or \dQuote{height} to interpolate heights for
	    \dQuote{standingTree} objects.  }
  
      \item{\code{pts}:}{ If \code{whichSense = "diameter"} then this
        is a vector of lengths/heights at which to interpolate the
        diameters. Alternatively, if \code{whichSense =
        "length"} (or "height") then this is a vector of diameters at which to
        interpolate the lengths/heights.  }
	  
	} %itemize
  } %item



  
\item{\strong{\code{signature(object = "standingTree")}}}{}
  
  \item{}{\bold{usage\ldots}
    \preformatted{taperInterpolate(object,
                 whichSense = c('diameter', 'height'),
                 pts = NULL,
                 ...) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{standingTree}.}
	  
      \item{\code{whichSense}:}{ Either "diameter" to interpolate diameters, or
	    "height" to interpolate heights.  }
  
      \item{\code{pts}:}{ If \code{whichSense = "diameter"} then this is a vector
	    of heights at which to interpolate the diameters. Alternatively, if
	    \code{whichSense = "height"} then this is a vector of diameters at
	    which to interpolate the heights. }
	  
	} %itemize
  } %item
  

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
