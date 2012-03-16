\name{monte-class}
\Rdversion{1.1}
\docType{class}
\alias{monte-class}
\alias{hist,monte-method}
\alias{show,monte-method}
\alias{summary,monte-method}

\title{Class \code{"monte"}}

\description{
  This is the class that contains the information about a basic repeated
  sampling (Monte Carlo) run. The class makes use of several other
  classes in the package that should be consulted for details. Also, the
  vignette referenced below is a good source of extended information and
  examples of how this class would be used.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("monte",
  ...)}. However, there is a constructor generic by the same name with
methods for creating objects. This is to be preferred over the use of
\code{new} since the objects returned from the constructor are
guaranteeted to be valid. 
}

\section{Slots}{
  
  \describe{
	
    \item{\code{pop}:}{Object of class \code{"montePop"}: A Monte Carlo population
    object, please see \code{\linkS4class{montePop}} for details. }
	
    %\item{\code{zeroTruncated}:}{Object of class \code{"logical"}:
    %\code{TRUE} if the population has been zero-truncated; \code{FALSE}
    %otherwise. This is mostly relevant for \code{\linkS4class{sampSurf} objects. }
	
    \item{\code{estimate}:}{Object of class \code{"character"}: In the
      case of \code{sampSurf} objects, this is the attribute for which
      the surface has been estimated. }
	
    \item{\code{NTsamples}:}{Object of class
      \code{"monteNTSampleOrNULL"}: An object of class
      \code{\linkS4class{monteNTSample}}, or \code{NULL} if non-existent. See the
      constructor for details. }
	
    \item{\code{BSsamples}:}{Object of class
      \code{"monteBSSampleOrNULL"}: An object of class
      \code{\linkS4class{monteBSSample}}, or \code{NULL} if non-existent. See the
      constructor for details. }
	
    \item{\code{description}:}{Object of class \code{"character"}: Some
      descriptive text about the object. }
	
  }
}

\section{Methods}{
  \describe{
	
    \item{hist}{\code{signature(x = "monte")}: Generate a set of histograms. }
	
    \item{show}{\code{signature(object = "monte")}: Print the object
      summary succinctly. }
	
    \item{summary}{\code{signature(object = "monte")}: Print the object summary. }
	
	 }
}

\references{
The \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently Large?} vignette.\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
\code{\linkS4class{montePop}}, \code{\linkS4class{monteSample}}
}
   

\examples{
showClass("monte")
}
\keyword{classes}
