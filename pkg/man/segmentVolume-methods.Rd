\name{segmentVolume-methods}
\docType{methods}
\alias{segmentVolume-methods}
\alias{segmentVolume,downLog-method}
\alias{segmentVolume,list-method}
\alias{segmentVolume,standingTree-method}

\title{ Methods for Function \code{segmentVolume} in Package
  \pkg{sampSurf} }

\description{
  There are three methods for the  \code{segmentVolume} generic
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
    \preformatted{segmentVolume(object,
              segBnds = c(low=0,  up=object@logLen),
              ... ) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{downLog}.}
  
      \item{\code{segBnds}:}{ The lower and upper segment bounds in
      terms of log length in the correct units.}
	  
	} %itemize
  } %item






\item{\strong{\code{signature(object = "list")}}}{ %
  
     Please do not use this method, use the other two which call it correctly\ldots}

  \item{}{\bold{usage\ldots}
    \preformatted{segmentVolume(object,
              segBnds = NULL,
              ... ) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{list}.}
  
      \item{\code{segBnds}:}{ The lower and upper segment bounds in
      terms of log length in the correct units. The default,
      \code{NULL}, will return total stem volume.}
	  
	} %itemize
  } %item





\item{\strong{\code{signature(object = "standingTree")}}}{}

  \item{}{\bold{usage\ldots}
    \preformatted{segmentVolume(object,
              segBnds = c(low=0,  up=object@height),
              ... ) }
	   
	\itemize{

	  \item{\code{object}:}{ An object of class \dQuote{standingTree}.}
  
      \item{\code{segBnds}:}{ The lower and upper segment bounds in
      terms of tree height in the correct units.}
	  
	} %itemize
  } %item



	  
}} %section & describe

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
