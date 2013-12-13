\name{fullChainSawIZ-class}
\Rdversion{1.1}
\docType{class}
\alias{fullChainSawIZ-class}
%\alias{izGrid,fullChainSawIZ,Tract-method}

\title{Class \code{"fullChainSawIZ"}}

\description{This class holds the inclusion zone definition for the full
\sQuote{chainsaw} method (Gove and Van Deusen, 2011) for sampling down
coarse woody debris.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fullChainSawIZ",
  ...)}.  However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{fullChainSawIZ}} to create new objects.  
}

\section{Slots}{
  This class is a direct subclass of \dQuote{sausageIZ} and adds no new
  slots. Please see \code{"\linkS4class{sausageIZ}"} class definition for details. 
}

\section{Extends}{
Class \code{"\linkS4class{sausageIZ}"}, directly.\cr
Class \code{"\linkS4class{downLogIZ}"}, by class "sausageIZ", distance 2.\cr
Class \code{"\linkS4class{InclusionZone}"}, by class "sausageIZ", distance 3.
}

\section{Methods}{
  \describe{
    \item{izGrid}{\code{signature(izObject = "fullChainSawIZ", tract =
  "Tract")}: Used in sampling surface construction. }
	 }
   }
   
\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry} \bold{84}:109--117.\cr
}

\author{
Jeffrey H. Gove 
}

\note{ The inclusion zone for the full chainsaw method is the sausage
  inclusion zone, but it is given its own class here so that we are
  later able to use it as a signature argument for actually constructing
  the sampling surface. Each grid cell within the inclusion zone will
  have its estimate determined based on the
  \code{"\linkS4class{chainSawIZ}"} method for fixed-area plots, which
  produces a variable surface. All estimates for this method are
  unbiased with the exception of Density, which can have severe
  bias. The sausage method must be used to get unbiased estimates of
  this attribute. See the reference below for more details.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\linkS4class{standUpIZ}},
  \code{\linkS4class{chainSawIZ}}, \code{\linkS4class{sausageIZ}} and
  the \code{\linkS4class{downLogIZs}} container class.
}
\examples{
showClass("fullChainSawIZ")
}
\keyword{classes}
