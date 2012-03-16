\name{monte-methods}
\docType{methods}
\alias{monte-methods}
\alias{monte,sampSurf-method}

\title{ Methods for "\code{\linkS4class{monte}}" Object Construction
  in Package \pkg{sampSurf} }

\description{ The following methods for generic function
  \code{\link{monte}} will construct valid objects of class
  "\code{\linkS4class{monte}}". Please see the \sQuote{\dQuote{monte}:
  When is \emph{n} Sufficiently Large?}  vignette for more details and
  examples.  }

\section{Methods}{

  \emph{Please note} that each constructor has the same argument list,
  so it is only stated once below for the first constructor. The
  signature argument in each case specifies the \emph{population} from
  which samples should be drawn. 
  
\describe{

\item{\strong{\code{signature(object = "montePop")}}}{

   This constructor method is only for use with objects of class
   "\code{\linkS4class{montePop}}". Ultimately, this constructor is
   called by all the others. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         zeroTruncate = TRUE,
         n = object@n,
         mcSamples = 100,
         type = c('both', 'normalTheory', 'bootstrap'),
         R = 100, 
         alpha = 0.05, 
         description = 'Monte Carlo Object',
         replace = TRUE,
         startSeed = NA,
         runQuiet = TRUE,
         ... )}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class
	    "\code{\linkS4class{montePop}}". }
		 
	  \item{\code{zeroTruncate}:}{ \code{TRUE}: truncate the zero values
	  from the population; \code{FALSE}: leave it as is.}
		 
	  \item{\code{n}:}{ A vector of sample sizes that will be used to
	    draw \code{mcSamples} Monte Carlo samples from the population.}
		 
	  \item{\code{mcSamples}:}{ A scalar specifying the number of Monte
	    Carlo samples to draw from the population of sizes \code{n}. }
		 
	  \item{\code{type}:}{ The type of confidence intervals to
	    calculate: normal theory, bootstrap, or both. }
				 
	  \item{\code{R}:}{ A scalar specifying the number of bootstrap
	    sample replicates to draw. }
		  
	  \item{\code{alpha}:}{ The two-tailed alpha level for confidence
        interval calculation. I.e., for the 95\% confidence level, specify
        \code{alpha=0.05}. }
		 
	  \item{\code{replace}:}{ \code{TRUE}: sample with replacement;
		\code{FALSE}: sample without replacement. Note that this only
		affects how each of the individual \code{mcSamples} samples are
		drawn from the population; specifically, note that bootstrapping
		is always with replacement \emph{from} these individual
		\code{mcSamples} samples by definition. }
		
	  \item{\code{description}:}{ A description of the object as a
	    character string.}
		 
	  \item{\code{startSeed}:}{ A scalar specifying a random number seed
	    for the Monte Carlo draws. See \code{\link{initRandomSeed}} for
	    details. }
		 
	  \item{\code{runQuiet}:}{ \code{TRUE}: no feedback; \code{FALSE}:
	    feedback if available. }
		
	  \item{... :}{ Other arguments to be passed along to
	    \code{\link{montePop}}, \code{\link{monteNTSample}} and
	    \code{\link{monteBSSample}}. }
	  
	} %itemize
 } %item 

\item{\strong{\code{signature(object = "numeric")}}}{

   This constructor method is only for use with objects of class
   "\code{\linkS4class{numeric}}"; i.e., a numeric vector. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         zeroTruncate = TRUE,
         n = c(10),
         mcSamples = 100,
         type = c('both', 'normalTheory', 'bootstrap'),
         R = 100, 
         alpha = 0.05, 
         description = 'Monte Carlo Object',
         replace = TRUE,
         startSeed = NA,
         runQuiet = TRUE,
         ... )}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class
	    "\code{\linkS4class{numeric}}". This should be a numeric vector
	    of appropriate length for the population. }
	  
	} %itemize
 } %item 

\item{\strong{\code{signature(object = "sampSurf")}}}{

   This constructor method is only for use with objects of class
   "\code{\linkS4class{sampSurf}}". The population will be extracted
   from the cell values in the object.}
  
  \item{}{\bold{usage\ldots}
    \preformatted{function(object,
         zeroTruncate = TRUE,
         n = c(10),
         mcSamples = 100,
         type = c('both', 'normalTheory', 'bootstrap'),
         R = 100, 
         alpha = 0.05, 
         description = 'Monte Carlo Object',
         replace = TRUE,
         startSeed = NA,
         runQuiet = TRUE,
         ... )}
	   
	\itemize{
		 
	  \item{\code{object}:}{ An object of class
	    "\code{\linkS4class{sampSurf}}". }
	  
	} %itemize
 } %item 

}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
