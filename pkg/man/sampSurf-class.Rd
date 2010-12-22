\name{sampSurf-class}
\Rdversion{1.1}
\docType{class}
\alias{sampSurf-class}
%\alias{bbox,sampSurf-method}
%\alias{perimeter,sampSurf-method}
%\alias{plot3D,sampSurf-method}
%\alias{plot,sampSurf,missing-method}
%\alias{show,sampSurf-method}
%\alias{summary,sampSurf-method}

\title{Class "sampSurf"}

\description{ This class allows one to collect a population of
  "\code{\linkS4class{Stem}}" (actually subclasses thereof) objects
  along with their inclusion zones for a given sampling method or
  protocol and generate the sampling surface resulting from the
  method. One can think of the sampling surface grid as generating all
  possible samples of size one over the population based on the grid
  cell size. That is, the number of sample point locations is equivalent
  to the number of grid cells. At each grid cell, an estimate based on
  one sample is recorded. Because inclusion zones will often overlap,
  the estimate at the individual grid cells will be a function of those
  logs whose inclusion zones overlapped the grid cell center. In this
  way, the surface is constructed representing the samples at each
  point. One can think of the surface as a discrete approximation to the
  actual continuous surface; the approximation getting better as the
  grid cell size goes to zero (grid resolution increases).

  Much of this is explained in far more detail in the references
  below. One can also generate multiple copies based on different log
  populations and average those surfaces to get a result that minimizes
  any possible irregularities in a particular sample draw of
  \dQuote{Stem} objects. This has not been built into the package yet,
  but is simple enough to do, and can be added later.

  Note that only one attribute can be represented for a given sampling
  surface. However, it is trivial to use the same population of logs and
  inclusion zones to estimate a surface for a different attribute on the
  same tract. It is also straightforward to use the same population of
  logs and base tract extents to compare different sampling methods or
  protocols. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sampSurf",
  ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{sampSurf}} to create new objects. There is more
than than one form of the function, so please refer to its methods for
alternatives.  More details are
found in \emph{"The sampSurf Class"} vignette.
}

\section{Slots}{
  \describe{
    \item{\code{description}:}{Object of class \code{"character"}: a
      character description to be associated with this surface. }
	
    \item{\code{izContainer}:}{Object of class \code{"downLogIZs"}: A
      collection or population of down log
      "\code{\linkS4class{InclusionZone}}" objects. Note that this can
      be extended to include standing tree inclusion zone objects when
      that portion of the package is completed. }
	
    \item{\code{tract}:}{Object of class \code{"Tract"}: This is the
      tract over which to generate the sampling surface, and on which
      the objects in the \code{izContainer} slot exist. }
	
    \item{\code{estimate}:}{Object of class \code{"character"}: The
      attribute estimate name for the sampling surface represented on
      this tract; i.e., "cubicVolume", or number of stems
      ("Density"), etc., per unit area.}
	
    \item{\code{surfStats}:}{Object of class \code{"list"}: A list
      object with the summary statistics for the sampling surface with
      estimate attribute in the \code{estimate} slot. } }
  
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "sampSurf")}: ... }
    \item{perimeter}{\code{signature(object = "sampSurf")}: ... }
    \item{plot3D}{\code{signature(x = "sampSurf")}: ... }
    \item{plot}{\code{signature(x = "sampSurf", y = "missing")}: ... }
    \item{show}{\code{signature(object = "sampSurf")}: ... }
    \item{summary}{\code{signature(object = "sampSurf")}: ... }
	 }
   }
   
\references{
Williams, M. S. 2001. New approach to areal sampling in ecological
surveys. \emph{Forest Ecology and Management} \bold{154}:11--22. \cr
%
Williams, M. S. 2001. Nonuniform random sampling: an alternative method
of variance reduction for forest surveys. \emph{Canadian Journal of Forest
  Research} \bold{31}: 2080--2088. \cr
%
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. (Accepted).\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\note{ Some of the routines in the \pkg{raster} package automatically
  subsample the grid for certain operations (I believe mostly plotting,
  but I have to look into this). One must therefore make sure, if
  combination of \dQuote{Tract} size and grid cell resolution is such
  that the number of cells is more than 100,000, that this does not
  affect the statistical results. Again, I need to check this further to
  make sure it is not an issue with \code{\link{raster}}.
}


\seealso{
\code{\link{sampSurf-package}}, \code{\link{sampSurf}}, \code{\link{sampSurf-methods}}
}

\examples{
showClass("sampSurf")
}
\keyword{classes}
