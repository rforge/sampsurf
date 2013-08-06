\name{montePop-methods}
\docType{methods}
\alias{montePop-methods}
\alias{montePop,numeric-method}
\alias{montePop,sampSurf-method}

\title{ Methods for "\code{\linkS4class{montePop}}" Object Construction in Package
  \pkg{sampSurf} }

\description{The following methods will construct valid objects of class
  "\code{\linkS4class{montePop}}". Please see the
  \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?}
  vignette for more details and examples.  }

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(popVals = "numeric")}}}{

      This method is meant to be used with any population that can be
      represented as a numeric vector.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{montePop(popVals,
         zeroTruncate = FALSE,
         n = NA,
         description = 'Monte Carlo Population Object',
         ... )}
	   
	\itemize{
		 
	  \item{\code{popVals}:}{ A numeric vector containing the population
	    values. } 
		 
	  \item{\code{zeroTruncate}:}{ \code{TRUE}: truncate the zero values
	  from the population; \code{FALSE}: leave it as is.}
		 
	  \item{\code{n}:}{ A vector of sample sizes  that will be used in
	  conjunction with drawing samples from the population; or,
	  \code{NA} if not desired.}
		
	  \item{\code{description}:}{ A description of the object as a
	    character string.}
		
	  \item{... :}{ Other arguments to be passed along--not used at
		present.}
	  
	} %itemize
 } %item


\item{\strong{\code{signature(popVals = "sampSurf")}}}{

      This method is meant to be used with objects of class
	  "\code{\linkS4class{sampSurf}}". All arguments other than the
	  signature argument are as defined for the previous method.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{montePop(popVals,
         zeroTruncate = FALSE,
         n = NA,
         description = 'Monte Carlo Population Object: sampSurf',
         ... )}
	   
	\itemize{
		 
	  \item{\code{popVals}:}{ An object of class
	      "\code{\linkS4class{sampSurf}}" from which the population
	      (sampling surface grid) values will be extracted. }
		
	} %itemize
 } %item

}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
