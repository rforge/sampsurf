\name{monteNTSample-methods}
\docType{methods}
\alias{monteNTSample-methods}
\alias{monteNTSample,montePop-method}

\title{ Methods for Function \code{\link{monteNTSample}} in Package
  \pkg{sampSurf} }

\description{
The following methods will construct valid objects of class
  "\code{\linkS4class{monteNTSample}}." Please see the
  \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?}
  vignette for more details and examples. }

\section{Methods}{
  
\describe{

\item{\strong{\code{signature(population = "montePop")}}}{

     This method works with an object of class
	 "\code{\linkS4class{montePop}}" as the signature argument. }

  
  \item{}{\bold{usage\ldots}
    \preformatted{monteNTSample(population,
              n = c(10),
              mcSamples = 100,
              alpha = 0.05, 
              replace = TRUE,
              startSeed = NA,
              runQuiet = TRUE,
              ... )}
	   
	\itemize{
		 
	  \item{\code{population}:}{ An object of class
	    "\code{\linkS4class{montePop}}" containing the population
	    values. }
		 
	  \item{\code{n}:}{ A vector of sample sizes that will be used to
	    draw \code{mcSamples} Monte Carlo samples from the population.}
		 
	  \item{\code{mcSamples}:}{ A scalar specifying the number of Monte
	    Carlo samples to draw from the population of sizes \code{n}. }
		 
	  \item{\code{alpha}:}{ The two-tailed alpha level for confidence
        interval calculation. I.e., for the 95\% confidence level, specify
        \code{alpha=0.05}. }
		 
	  \item{\code{replace}:}{ \code{TRUE}: sample with replacement;
		\code{FALSE}: sample without replacement. }
		 
	  \item{\code{startSeed}:}{ A scalar specifying a random number seed
	    for the Monte Carlo draws. See \code{\link{initRandomSeed}} for
	    details. }
		 
	  \item{\code{runQuiet}:}{ \code{TRUE}: no feedback; \code{FALSE}:
	    feedback if available. }
		
	  \item{... :}{ Other arguments to be passed along to
	    \code{\link{sample}}. }
	  
	} %itemize
 } %item
		
}} %\section

\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
