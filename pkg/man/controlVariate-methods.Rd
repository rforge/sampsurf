\name{controlVariate-methods}
\docType{methods}
\alias{controlVariate-methods}
\alias{controlVariate,Stem-method}
\alias{controlVariate,StemContainer-method}

\title{ Methods for \code{controlVariate} object construction in Package
  \pkg{sampSurf} }

\description{
 The methods below allow for the construction of individual objects of
 class "\code{\linkS4class{controlVariate}}" when applied to individual
 "\code{\linkS4class{Stem}}" subclass objects. Alternatively, the second
 constructor can be applied to a collection of
 "\code{\linkS4class{Stem}}" subclass objects and will return an
 appropriate collection of "\code{\linkS4class{controlVariate}}"
 objects (see, e.g., the "\code{\linkS4class{mcsContainer}}" class).
}

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(object = "Stem")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{controlVariate(object,
               segBnds = NULL,
               n.s = 1,
               startSeed = NA,
               u.s = NA,
               proxy = 'gvProxy',
               alphaLevel = 0.05,
               description = 'Control Variate Sampling',
               ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\linkS4class{downLog}}" or
        "\code{\linkS4class{standingTree}}".}
			
	  \item{\code{segBnds}:}{ A vector of length two giving the lower and
        upper height/length bounds for volume estimation within the
        bole.  These bounds correspond to the limits of integration
        along the bole. If either of the bounds are \code{NULL} or
        \code{NA}, the entire bole is used (default).}
			
	  \item{\code{n.s}:}{ The number of sampled heights desired within
	    \code{segBnds} for volume estimation.}	  
			
	  \item{\code{startSeed}:}{ The scalar seed for the random number
        generator used in the call to the class constructor. Please see
        the documentation in \code{\link{initRandomSeed}} for possible
        values and their meaning.}
			
	  \item{\code{u.s}:}{ The uniform random numbers used in selecting
        the sampling points along the bole. If this is either
        \code{NULL} or \code{NA}, then \code{n.s} and \code{startSeed}
        will be used to determine the random numbers. If this is a
        numeric vector, then \code{n.s} is set to its length, and
        \code{u.s} is used as the random number stream. No checking is
        done on the bounds of the numbers so \emph{be careful} if using
        the latter option. It is most useful in antithetic sampling
        where the 1-\code{u.s} stream is used (automatically).}
			
	  \item{\code{proxy}:}{ A character name specifying the proxy
	    function to be used in control variate sampling. See the
	    vignette referenced in the generic for details. }
			
	  \item{\code{alphaLevel}:}{ The two-tailed alpha-level for
        confidence interval construction.}
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
			
	  \item{\code{\dots}:}{ Arguments passed on to the proxy function.}
		
    } %itemize
  } %item







%------------------------------------------------------
\item{\strong{\code{signature(object = "StemContainer")}}}{


  See the above definitions for the first method for arguments not shown below.

}

  \item{}{\bold{usage\ldots}
    \preformatted{controlVariate(object,
               segBnds = NULL,
               n.s = 1,
               startSeed = NA,
               u.s = NA,
               proxy = 'gvProxy',
               alphaLevel = 0.05,
               description = 'Control Variate Sampling',
               ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ A collection of \code{"Stem"} class objects
	    in a valid "\code{\linkS4class{StemContainer}}" object.}
			
	  \item{\code{segBnds}:}{ The segment bounds, see the definition for
	    the first method. \emph{Note:} These bounds are used for all
	    stems in the collection, so it is up to you to make sure they
	    are legal for each stem.}
			
	  \item{\code{n.s}:}{ The number of sampled heights desired within
	    \code{segBnds} for volume estimation on each stem in the
	    collection.}
	  
	  \item{\code{startSeed}:}{ By default, the stream is started using
	    this seed (see above for the first method) and the current
	    random number stream is continued for each stem in the
	    collection. This results in a \emph{different} set of random
	    numbers for each stem (but all keyed off this starting value).}
			
	  \item{\code{u.s}:}{ If this is \code{NULL} or \code{NA}, then the
	    \code{n.s} and \code{startSeed} combination are used as
	    above. However, if this is a vector, then it is applied to each
	    stem. Therefore, the \emph{same} set of random numbers will be
	    applied to \emph{each} stem in the collection. }
			
	  \item{\code{\dots}:}{ Arguments passed on to the proxy function;
	    note that these apply to each stem in the collection.}
		
    } %itemize
  } %item


} %describe
} %section
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
