\name{sampSurf-methods}
\docType{methods}
\alias{sampSurf-methods}
\alias{sampSurf,downLogIZs,Tract-method}
\alias{sampSurf,numeric,Tract-method}

\title{ Methods for "sampSurf" object construction in Package `sampSurf' }

\description{The following methods will construct valid objects of
  class "\code{\linkS4class{sampSurf}}". Please see \emph{"The
  sampSurf Class"} vignette for more details and examples.
}

\section{Methods}{
\describe{

\item{\strong{\code{signature(object = "izContainer", tract =
	  "Tract")}}}{%\cr

      This constructor will allow the most flexibility. One can create
	  all of the individual objects from generating a population of
	  "Stem" objects up through the corresponding collection of
	  inclusion zones and give the latter, along with the \dQuote{Tract}
	  object to this constructor to generate the surface. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{sampSurf(object, 
         tract,
         estimate = unlist(c(.StemEnv$puaEstimates, .StemEnv$ppEstimates)),
         description = 'sampling surface object',
         runQuiet = FALSE,
         ... )}
	\itemize{
		 
	  \item{\code{object}:}{ An object of one of the subclasses of
	    "izContainer." }
		 
	  \item{\code{tract}:}{ An object of class "Tract" or subclass.}
		 
	  \item{\code{estimate}:}{ A character variable corresponding to the
	  desired estimate attribute; these include 'volume', 'Length'
	  (logs), 'Density', 'surfaceArea', 'coverageArea' (logs),
	  'basalArea' (trees), 'biomass' and 'carbon'. The sample 'depth'
	  surface is now also available.}
		
	  \item{\code{description}:}{ A description of the object as a
	    character string.}
		 
	  \item{\code{runQuiet}:}{ This routine will print a report of the
	    grid cells as it visits them; set this to \code{TRUE}: (default)
	    to see the report; \code{FALSE}: to run quietly, which is useful
	    for larger simulations.}
	  
	  \item{... :}{ Other arguments to be passed along--not used at present.}
	} %itemize
 } %item


	  
\item{\strong{\code{signature(object = "numeric", tract =
	  "Tract")}}}{%\cr

      This constructor masks much of the job of creating
	  a sampling surface. Just specify the number of stems and a tract
	  for signature arguments along with any options to generate a
	  surface. All arguments except those listed below are as in the
	  above constructor. Note that this constructor eventually calls the
	  first constructor detailed above to generate the sampling
	  surface. }
  
  \item{}{\bold{usage\ldots}
    \preformatted{sampSurf(object, 
         tract,
         iZone, 
         estimate = unlist(.StemEnv$puaEstimates),
         description = 'sampling surface object',
         runQuiet = FALSE,
         ...
        )}
	\itemize{
		 
	  \item{\code{object}:}{ The number of "Stem" subclass objects
	    (i.e., down logs) to generate in the population. }
		 
	  \item{\code{iZone}:}{ The type of inclusion zone or sampling
	    method/protocol desired. The routine will determine whether to
	    generate logs or trees based on this argument.}
	  
	  \item{... :}{ Other arguments to be passed along to routines that
		construct the downlog population, the inclusion zones, and
		eventually the first \dQuote{sampSurf} constructor given
		above. For example, one can pass arguments affecting how the
		sample log population will be drawn as shown in the vignette
		mentioned above (e.g., \code{logLens=c(4,12)}).}	
	} %itemize
 } %item

}}

\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
