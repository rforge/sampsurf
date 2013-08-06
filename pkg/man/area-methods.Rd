\name{area-methods}
\docType{methods}
\alias{area-methods}
\alias{area,chainSawIZ-method}
\alias{area,InclusionZone-method}
\alias{area,InclusionZoneGrid-method}
\alias{area,standUpIZ-method}
\alias{area,Tract-method}

\title{ Methods for Function \code{area} in Package \pkg{sampSurf} }

\description{The methods described here are for the \code{\link[sampSurf]{area}}
  generic as used in the \pkg{sampSurf} package. Each of the methods
  shown below has different arguments that customize it for a different
  class of objects found in this package.
}

\section{Methods}{

  All of the methods have the same argument form, so details will only
  be given for the \dQuote{InclusionZone} class version. The other
  available method signatures are simply listed.
  
\describe{

  \item{\strong{\code{signature(x = "InclusionZone")}}}{}

  
  \item{}{\bold{usage\ldots}
    \preformatted{area(x, ... )}
	\itemize{
		 
	  \item{\code{x}:}{ An object that is a subclass of \dQuote{InclusionZone}. }
	  \item{\dots :}{ Currently not used---just gobbled by \R.}
	} %itemize	 
} %item 

\item{\strong{\code{signature(x = "InclusionZoneGrid")}}}{}

%these are both really just InclusionZone, so don't need to be listed...
%\item{\code{signature(x = "standUpIZ")}}{
%%  ~~describe this method here~~
%}
%\item{\code{signature(x = "chainSawIZ")}}{
%%  ~~describe this method here~~
%}

\item{\strong{\code{signature(x = "Tract")}}}{}

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
