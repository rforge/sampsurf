\name{sampSurf-package}
\alias{sampSurf-package}
%\alias{sampSurf} %conflicts with the generic
\docType{package}
\title{
Sampling Surface Simulation
}

\description{

The \pkg{sampSurf} package can be used to construct sampling surface
simulations for different areal sampling methods common to forestry and
ecology. These would include fixed-area plot methods, line intersect
sampling, and various variable-radius plot sampling methods, the latter
more commonly used in forestry. Most of these fall under the general
probability proportional to size (\acronym{PPS}) umbrella and have some
form of inclusion zone associated with each individual in the
population. The inclusion zone has well-defined area (sometimes called
the inclusion area) and may be thought of simply as that zone within
which a sample point could fall and select the associated
individual. The sample point can be the center of a circular fixed-area
plot, an actual point as in horizontal point sampling, or say, the
midpoint of a line in line intersect sampling.

In general we are interested in determining the properties of various
sampling methods mentioned above when applied to fixed objects such as
standing trees or down logs. The \pkg{sampSurf} package will allow
generation of log or tree populations within a fixed tract area. The
surface generated from the intersection of inclusion zones applied to
the individuals in the population for a given attribute (e.g., cubic
volume, number of individuals, etc.) are represented by the
"\code{\linkS4class{sampSurf}}" class and can be displayed
graphically. Estimator variance is directly associated with the sampling
surface roughness, and so methods can also be compared visually.

Please note that \pkg{sampSurf} is quite functional, and has support for
down logs sampled on fixed-area plots (which include the
\sQuote{stand-up,} \sQuote{sausage,} and \sQuote{chainsaw} methods or
protocols), point relascope sampling, several variants of perpendicular
distance sampling, as well as a new method called distance limited Monte
Carlo. Support for standing trees has not yet been added. The class
structure is designed to support additions with relative ease, however,
so more methods will appear in future releases (hopefully with some
contributed by users).

}

\details{
  
\tabular{ll}{
Package: \tab sampSurf\cr
Type: \tab Package\cr
Version: \tab 0.51\cr
Date: \tab 2011-04-27\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
}

The package is written in S4 and has a number of classes and
constructors, as well as some helper functions. In general, the
capabilities include the following\ldots

\itemize{

  \item Generate synthetic populations of logs or standing trees (the
  latter is not implemented yet) as \dQuote{\code{\linkS4class{Stem}}}
  objects. Note that data from field measurements can also be used if
  available.
%  
  \item Construct inclusion zones around each individual in the
  population and assign attributes to the zone for volume, etc.
%  
  \item Generate \dQuote{\code{\linkS4class{Tract}}} objects of given dimension
  (x,y) and resolution (grid cell size) to hold the objects. The grid
  cell resolution related directly to sampling intensity: each grid cell center
  can be thought of as a sample point for the method under consideration.
%
  \item Combine the above into a sampling surface and calculate summary
  statistics, or display visually.
%
  \item Simulate multiple realizations of the same method with differing
  populations of objects to get more general results over many
  \dQuote{plots} (i.e., \dQuote{Tract} objects). (This is not
  implemented yet.)

} %itemize

Please note that there are several vignettes included within the package
that explain in detail the classes and methods listed below using many
examples. Please refer to the \dQuote{Overview} vignette to get
started. Then perhaps refer to the other vignettes in the order
presented in the overview for detailed examples and descriptions. To
find the vignettes on your help system, go to the index for the
\pkg{sampSurf} package (see the link at the bottom of this page). The help
files linked to below explain the various classes and methods in more
detail, including slot and argument definitions.

} %details



\section{Classes for use in \pkg{sampSurf}}{ The following S4 classes
  are defined within the \pkg{sampSurf} package to facilitate sampling
  surface generation. Note that it is not necessary to work with the
  class structures directly if all you want to do is generate a sampling
  surface for a population of synthetic logs or trees. However, some
  familiarization with the basic class structure is helpful in order to
  understand how the pieces fit together into a final sampling
  surface. Again, the vignettes are a source of much information.

  \subsection{The \dQuote{Stem} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{Stem}}: \tab The stem class is virtual, but
	  from it subclasses are defined for logs and trees\cr
	\code{\linkS4class{downLog}}: \tab A class for down logs\cr
	\code{\linkS4class{downLogs}}: \tab Container class for a population
	  of down log objects\cr
  } }

  \subsection{The \dQuote{Tract} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{Tract}}: \tab A class structure for the land area
	  which holds the population of individuals \cr
	\code{\linkS4class{bufferedTract}}: \tab A \dQuote{Tract} subclass
	with a buffer \cr
  } }

  \subsection{The \dQuote{ArealSampling} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{ArealSampling}}: \tab A class structure for describing
	  areal sampling methods \cr
    \code{\linkS4class{circularPlot}}: \tab A subclass of
	  \dQuote{ArealSampling} for circular plot construction\cr
	\code{\linkS4class{pointRelascope}}: \tab A subclass of
	  \dQuote{ArealSampling} for point relascope sampling\cr
	\code{\linkS4class{perpendicularDistance}}: \tab A subclass of
	  \dQuote{ArealSampling} for variants of perpendicular distance sampling\cr
	\code{\linkS4class{distanceLimited}}: \tab A subclass of
	  \dQuote{ArealSampling} for distance limited sampling\cr
  } }

  \subsection{The \dQuote{InclusionZone} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{InclusionZone}}: \tab Virtual base class for combining the
	  sampling method with the individual down log or tree objects\cr
	\code{\linkS4class{downLogIZ}}: \tab Virtual subclass for all down log
	  \code{InclusionZone} objects.\cr
	\code{\linkS4class{standUpIZ}}: \tab Class for the "standup" method
	  of sampling down logs.\cr
	\code{\linkS4class{sausageIZ}}: \tab Class for the "sausage" method
	  of sampling down logs.\cr
	\code{\linkS4class{chainSawIZ}}: \tab Class for the "chainsaw" method
	  of sampling down logs.\cr
	\code{\linkS4class{pointRelascopeIZ}}: \tab Class for the point relascope method
	  of sampling down logs.\cr
	\code{\linkS4class{perpendicularDistanceIZ}}: \tab Class for the
	  perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{omnibusPDSIZ}}: \tab Class for the omnibus
	  perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{distanceLimitedPDSIZ}}: \tab Class for the
	  distance limited perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{omnibusDLPDSIZ}}: \tab Class for the omnibus
	  distance limited perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{distanceLimitedMCIZ}}: \tab Class for the 
	  distance limited Monte Carlo method of sampling down logs.\cr
	\code{\linkS4class{downLogIZs}}: \tab A container class to hold a
	  population of inclusion zone objects associated with down logs \cr
	} }

  \subsection{The \dQuote{InclusionZoneGrid} class\ldots}{

  \tabular{ll}{  
	\code{\linkS4class{InclusionZoneGrid}}: \tab Not for general use, see
	  documentation for details \cr
	\code{\linkS4class{csFullInclusionZoneGrid}}: \tab Not for general
	  use, see documentation for details \cr
	} }

  \subsection{The \dQuote{sampSurf} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{sampSurf}}: \tab The class structure for the final
	  sampling surface simulation \cr
	} }

  Again, many of these classes will only be of interest to someone
  wanting to do special simulations or for someone who wants to add
  extensions--for example, new sampling methods. 
	
} %classes



\section{Object constructors}{
  For each of the classes defined in the table above, we must be able to
  create objects that can be used in \R. This is done using
  class-specific "constructor" methods that take the drudgery away from
  creating what can often be somewhat complicated (with all the
  graphical components) new object instances. There may be more than one
  constructor for a given class of object, and these are differentiated
  by the method signature; see the links provided below for more details\ldots


  \subsection{\dQuote{Stem} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{downLog}}: \tab Constructor for individual
	  \dQuote{Stem} objects\cr   
	\code{\link{downLogs}}: \tab Constructs a container object 	holding
	multiple \dQuote{downLog} objects\cr
  } }
	

  \subsection{\dQuote{Tract} class constructors\ldots}{

  \tabular{ll}{
    \code{\link{Tract}}: \tab Constructor for \dQuote{Tract} objects \cr 
	\code{\link{bufferedTract}}: \tab Constructs a
	\dQuote{bufferedTract} object\cr
  }}
	

  \subsection{\dQuote{ArealSampling} class constructors\ldots}{

  \tabular{ll}{
    \code{\link{circularPlot}}: \tab Constructs a circular plot object
	  \cr
    \code{\link{pointRelascope}}: \tab Constructs a point relascope object
	  \cr
    \code{\link{perpendicularDistance}}: \tab Constructs a perpendicular
      distance object \cr
    \code{\link{distanceLimited}}: \tab Constructs a distance limited object
	  \cr
  }}	

  \subsection{\dQuote{InclusionZone} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{standUpIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{stand-up} sampling method \cr
	\code{\link{sausageIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{sausage} sampling method \cr
	\code{\link{chainSawIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{chainsaw} sampling method \cr
	\code{\link{pointRelascopeIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{point relascope} sampling method \cr
	\code{\link{perpendicularDistanceIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{perpendicular distance} sampling method \cr
	\code{\link{omnibusPDSIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{omnibus perpendicular distance} sampling method \cr
	\code{\link{distanceLimitedPDSIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{distance limited perpendicular distance} sampling method \cr
	\code{\link{omnibusDLPDSIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{omnibus distance limited perpendicular distance} sampling method \cr
	\code{\link{distanceLimitedMCIZ}}: \tab Creates an \dQuote{InclusionZone}
	  object for the \sQuote{distance limited Monte Carlo} sampling method \cr
	\code{\link{downLogIZs}}: \tab Holds a population of 
      \dQuote{InclusionZone} objects for down logs\cr
   }}


  \subsection{\dQuote{InclusionZoneGrid} class constructors\ldots}{

  \tabular{ll}{  	
	\code{\link{izGrid}}: \tab Creates an \dQuote{InclusionZoneGrid}
	  object\cr
	\code{\link{izGridCSFull}}: \tab Creates an \dQuote{csFullInclusionZoneGrid}
	object\cr
   }}

   
  \subsection{\dQuote{sampSurf} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{sampSurf}}: \tab Constructor for \dQuote{sampSurf}
	  objects\cr 
  }}
	
} %constructors




\section{Summary and plotting methods}{
  Almost without exception, the objects created above have graphical
  content made possible by using classes from the \code{\link{sp}} or
  \code{\link{raster}} packages. Therefore, methods have been added to
  the \code{\link{plot}} generics to allow for graphical display of
  objects. In addition, \code{\link{summary}} and \code{\link{show}}
  methods are also available (sometimes producing the same result) for
  printing information within the object\ldots

  \tabular{ll}{
	\code{\link{plot}}: \tab  To plot one of the package objects\cr
	\code{\link{summary}}: \tab  To summarize one of the package objects\cr
	\code{\link{show}}: \tab  To succinctly print one of the package objects\cr
	\code{\link{plot3D}}: \tab  To plot \dQuote{sampSurf} objects using
	  \pkg{rgl}\cr 
  } %tabular
  
} %summary and plotting



\section{Helper methods}{
  Slots in S4 objects can be accessed directly using \code{\link{slot}}
  method or the \code{\link{@}} operator. However, some \dQuote{helper}
  functions have been provided for certain objects, along with other
  potentially useful routines\ldots

  \tabular{ll}{
	\code{\link{bbox}}: \tab Return the bounding box for a spatial
	  object\cr
	\code{\link{bboxCheck}}: \tab Check for a valid bounding box for a
	  spatial object\cr
	\code{\link{bboxToPoly}}: \tab Convert a \code{bbox} matrix to a
	"\code{\linkS4class{SpatialPolygons}}" object\cr
	\code{\link{bboxSum}}: \tab	Calculate an overall \code{bbox} from and
	array of \code{bbox} matrices\cr
	\code{\link{perimeter}}: \tab Returns the perimeter of a spatial
	  object, which can then be plotted\cr
  } %tabular
  
} %helper methods




\section{Coercion methods}{
  Object coercion is accomplished in S4 by a call to \code{\link{as}}
  with the appropriate object and class to which one wants to
  convert. Only one conversion method is supplied thus far\ldots

  \tabular{ll}{
	\code{\link{as}}: \tab Convert from a \dQuote{downLogs} object to a
	data frame\cr
  } %tabular
  
} %coercion methods



\section{Miscellaneous methods}{
  Some methods that might be useful as stand-alone routines\ldots

  \tabular{ll}{
	\code{\link{sampleLogs}}: \tab Draws a simulated sample of down logs and
	  returns a data frame. \cr
	\code{\link{boltDimensions}}: \tab Calculates segment volumes,
	  surface area, etc.\cr
    \code{\link{taperInterpolate}}: \tab Interpolates diameters or
	  lengths within down logs.\cr
	\code{\link{heapIZ}}: \tab Used to heap inclusion zone grids into a
	  sampling surface.\cr
	\code{\link{gridCellEnhance}}: \tab Draws grid lines and centers.\cr
    \code{\link{initRandomSeed}}: \tab Initialize \code{.Random.seed}.\cr
    \code{\link{transparentColorBase}}: \tab Transparancy in base graphics.\cr
	
  } %tabular
     
} %misc methods



\section{\sQuote{sampSurf} environments}{
  This package uses a \dQuote{hidden} environment to keep things that
  are not necessary for the user out of mind. There is little reason to peak
  into it, except that some function argument defaults are specified
  directly via this environment. Thus, to enable you to figure out better
  what is going on, the environment is exported and its members can be
  accessed if desired.
  
  \tabular{ll}{
	\code{\link{.StemEnv}}: \tab This environment holds parameters for
	  much of the package. \cr
	
  } %tabular
     
} %environments
 

\section{\sQuote{sampSurf} class unions}{
  There are a few class unions that are documented here only for the
  sake of completeness. It is unlikely that the casual user would ever
  need to be concerned about these.
  
  \tabular{ll}{
	\code{\linkS4class{dlsNumeric}}: \tab Accepts either class
	  \dQuote{distanceLimited} or \dQuote{numeric}. \cr
	\code{\linkS4class{dlmcIZNull}}: \tab Accepts either class
	  \dQuote{distanceLimitedMCIZ} or \dQuote{NULL}. \cr
	\code{\linkS4class{pdsIZNull}}: \tab Accepts either class
	  \dQuote{perpendicularDistanceIZ} (or subclass) or
	  \dQuote{NULL}. \cr
	%only used in solidType slot...  
	\code{\linkS4class{numericNULL}}: \tab Accepts either class
	  \dQuote{numeric} or \dQuote{NULL}. \cr
  } %tabular
  

} %class unions  
  
\author{
Jeffrey H. Gove
}

\references{ The first two references describe the sampling surface
method in detail. The rest provide some examples of its use and are not
exhaustive.

\describe{
%
\item{}{Williams, M. S. 2001. New approach to areal sampling in ecological
  surveys. \emph{Forest Ecology and Management} \bold{154}:11--22.} 
%
\item{}{Williams, M. S. 2001. Nonuniform random sampling: an alternative method
  of variance reduction for forest surveys. \emph{Canadian Journal of Forest
  Research} \bold{31}:2080--2088.}
%
\item{}{Williams, M. S. and Gove, J. H. 2003. Perpendicular distance sampling:
  an alternative method for sampling downed coarse woody
  debris. \emph{Canadian Journal of Forest Research}
  \bold{33}:1564--1579.}
%
\item{}{Gove, J. H., Williams, M. S., Stahl, G., and Ducey, M. J. 2005. Critical
  point relascope sampling for unbiased volume estimation of downed coarse
  woody debris. \emph{Forestry} \bold{78}:417--431.}
%
\item{}{Stahl, G., Gove, J. H., Williams, M. S., and Ducey, M. J. 2010. Critical
  length sampling: a method to estimate the volume of downed coarse woody
  debris. \emph{European Journal of Forest Research}
  \bold{129}:993--1000.}  
%
\item{}{Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
  downed coarse woody debris. \emph{Forestry} \bold{84}:109--117.}
}
}

\keyword{ package }
\seealso{
\code{\link{sp}}, \code{\link[raster:raster-package]{raster}}
}
%\examples{

%}
