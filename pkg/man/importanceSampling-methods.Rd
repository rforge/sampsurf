\name{importanceSampling-methods}
\docType{methods}
\alias{importanceSampling-methods}
\alias{importanceSampling,downLog-method}
\alias{importanceSampling,downLogs-method}
\alias{importanceSampling,list-method}
\alias{importanceSampling,standingTree-method}
\alias{importanceSampling,standingTrees-method}

\title{ Methods for \code{importanceSampling} object construction in Package
  \pkg{sampSurf} }

\description{

  The methods described below for the construction of objects of class
  "\code{\linkS4class{importanceSampling}}" fall into two categories as
  follows\ldots\cr

  1. The \code{object} passed is a "\code{\linkS4class{Stem}}" subclass
  object: importance sampling is applied to the individual stem.\cr

  2. The \code{object} passed is a collection
  ("\code{\linkS4class{StemContainer}}") of "\code{Stem}" subclass
  objects: importance sampling is applied to each stem in the collection
  based on the other arguments passed.\cr

  In adition, there is a separate method for the case when \code{object}
  is of class "\code{list}". This is the base constructor that really
  performs all of the importance sampling code on the individual
  stems. The other methods are simply wrappers that call this
  method. The \code{list} constructor is detailed below for
  completeness; however, please do not use it, pass one of the other
  types of objects instead to use one of the other methods, this will
  ensure proper results inasmuch as is possible.
    
}

\section{Methods}{

  Each of the methods has either the same argument list, or a subset of
  arguments that correspond to the \code{list} signature method. Refer
  to that method for any arguments that have a universal interpretation
  over all methods.
  
\describe{

%--------------------------------------------------------------------------------------
\item{\strong{\code{signature(object = "downLog")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{importanceSampling(object,
                   segBnds = c(low = 0,  up = object@logLen),
                   n.s = 1,
                   startSeed = NA,
                   u.s = NA,
                   proxy = 'gvProxy',
                   alphaLevel = 0.05,
                   description = 'Importance Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\linkS4class{downLog}}".}
		
    } %itemize
  } %item






  
%--------------------------------------------------------------------------------------
\item{\strong{\code{signature(object = "downLogs")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{importanceSampling(object,
                   segBnds = NULL,
                   n.s = 1,
                   startSeed = NA,
                   u.s = NA,
                   proxy = 'gvProxy',
                   alphaLevel = 0.05,
                   description = 'Importance Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ A container object of class
        "\code{\linkS4class{downLogs}}" with one or more
        "\code{downLog}" objects.}
			
	  \item{\code{segBnds}:}{ The segment bounds, see the definition for
	    the \code{list} method. \emph{Note:} These bounds are used for
	    all logs in the collection, so it is up to you to make sure
	    they are legal for each log.}
	  
	  \item{\code{startSeed}:}{ By default, the stream is started using
	    this seed (see below for the \code{list} method) and the current
	    random number stream is continued for each log in the
	    collection. This results in a \emph{different} set of random
	    numbers for each log (but all keyed off this starting value).}
			
	  \item{\code{u.s}:}{ If this is \code{NULL} or \code{NA}, then the
	    \code{n.s} and \code{startSeed} combination are used as
	    described below for the \code{list} method. However, if this is
	    a vector, then it is applied to each log. Therefore, the
	    \emph{same} set of random numbers will be applied to \emph{each}
	    log in the collection. }
		
    } %itemize
  } %item






%--------------------------------------------------------------------------------------
\item{\strong{\code{signature(object = "list")}}}{

   Please do not use this method directly, use one of the others
   documented here that will ultimately call this method.
   
}

  \item{}{\bold{usage\ldots}
    \preformatted{importanceSampling(object,
                   segBnds = c(low = 0, up = object$height),
                   n.s = 1,
                   startSeed = NA,
                   u.s = NA,
                   proxy = 'gvProxy',
                   alphaLevel = 0.05,
                   controlVariate = FALSE,
                   description = 'Monte Carlo Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\linkS4class{list}}".}
			
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
	    function to be used in importance sampling. See the
	    vignette referenced in the generic for details. }
		
	  \item{\code{alphaLevel}:}{ The two-tailed alpha-level for
        confidence interval construction.}
			
	  \item{\code{controlVariate}:}{ \code{TRUE}: use control variate
	    sampling; \code{FALSE}: either crude Monte Carlo or importance
	    sampling, depending on the \code{proxy} passed.}
		
	  \item{\code{description}:}{ A character vector description of the
		object.}
		
	  \item{\code{...}:}{ Arguments to be passed on to the proxy
	    function. For collections, these apply to each stem in the
	    collection. }

		
    } %itemize
  } %item






%--------------------------------------------------------------------------------------
\item{\strong{\code{signature(object = "standingTree")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{importanceSampling(object,
                   segBnds = c(low = 0,  up = object@height),
                   n.s = 1,
                   startSeed = NA,
                   u.s = NA,
                   proxy = 'gvProxy',
                   alphaLevel = 0.05,
                   description = 'Importance Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ An object of class
        "\code{\linkS4class{standingTree}}".}
		
    } %itemize
  } %item







%--------------------------------------------------------------------------------------
\item{\strong{\code{signature(object = "standingTrees")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{importanceSampling(object,
                   segBnds = NULL,
                   n.s = 1,
                   startSeed = NA,
                   u.s = NA,
                   proxy = 'gvProxy',
                   alphaLevel = 0.05,
                   description = 'Importance Sampling',
                   ... )}
	   
    \itemize{
			
	  \item{\code{object}:}{ A container object of class
        "\code{\linkS4class{standingTrees}}" with one or more
        "\code{standingTree}" objects.}
			
	  \item{\code{segBnds}:}{ The segment bounds, see the definition for
	    the \code{list} method. \emph{Note:} These bounds are used for
	    all trees in the collection, so it is up to you to make sure
	    they are legal for each tree.}
	  
	  \item{\code{startSeed}:}{ By default, the stream is started using
	    this seed (see below for the \code{list} method) and the current
	    random number stream is continued for each tree in the
	    collection. This results in a \emph{different} set of random
	    numbers for each tree (but all keyed off this starting value).}
			
	  \item{\code{u.s}:}{ If this is \code{NULL} or \code{NA}, then the
	    \code{n.s} and \code{startSeed} combination are used as
	    described below for the \code{list} method. However, if this is
	    a vector, then it is applied to each tree. Therefore, the
	    \emph{same} set of random numbers will be applied to \emph{each}
	    tree in the collection. }
		
    } %itemize
  } %item





	  
} %describe
} %section
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
