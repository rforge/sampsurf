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

\pkg{sampSurf} has support for several methods/protocols for sampling
down logs; e.g.,\ldots

\itemize{

  \item Fixed area plots, which includes the so-called \sQuote{stand-up,}
    \sQuote{sausage,} and \sQuote{chainsaw} protocols.
%
  \item Point relascope sampling (PRS).
%
  \item Perpendicular distance sampling (PDS) in several forms:
    Canonical PDS, omnibus PDS, canonical distance limited PDS (DLPDS),
    omnibus DLPDS, and hybrid DLPDS. Each of these PDS variants can be used
    with any one of three selection attributes: volume, coverage or
    surface areas.
%
  \item Distance limited sampling in two forms: Simple (DLS), and Monte
    Carlo (DLMCS).	

} %itemize


Support for standing trees includes the following methods\ldots

\itemize{

  \item Circular fixed area plots.
%
  \item Horizontal point (angle gauge) sampling (and tradiational Monte Carlo variants).
%
  \item Critical height sampling (also a Monte Carlo HPS variant) and
  importance sampling variants of CHS.
%
  \item Horizontal line (angle gauge) sampling.

} %itemize


Monte Carlo subsampling on down logs and standing trees is also
available outside of the areal sampling paradigm. In addition, some
areal sampling methods have special Monte Carlo variants, but the
tradiational Monte Carlo subsampling framework now in place on stem
objects means that these methods can be applied in conjunction with
virtually any areal method. Horizontal point sampling is an example
where both special and traditional Monte Carlo methods are available in
the package.
  
The class structure is designed to support additions with relative ease,
so more methods will appear in future releases (hopefully with some
contributed by users).

}

\details{
  
\tabular{ll}{
Package: \tab sampSurf\cr
Type: \tab Package\cr
Version: \tab 0.7-0\cr
Date: \tab 1-Aug-2013\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
}

The package is written in S4 and has a number of classes and
constructors, as well as some helper functions. In general, the
capabilities include the following\ldots

\itemize{

  \item Generate synthetic populations of logs or standing trees as
  \dQuote{\code{\linkS4class{Stem}}} objects. Note that data from field
  measurements can also be used if available.
%  
  \item Construct inclusion zones around each individual in the
  population and assign attributes to the zone for volume, etc.
%  
  \item Generate \dQuote{\code{\linkS4class{Tract}}} objects of given dimension
  (x,y) and resolution (grid cell size) to hold the objects. The grid
  cell resolution is related directly to sampling intensity: each grid cell center
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

In addition, the project homepage at R-Forge has some simple examples
that might be helpful: \url{http://sampsurf.r-forge.r-project.org/}.

} %details


%
%  S4 classes section...
%
\section{Classes For Use In \pkg{sampSurf}\ldots}{ The following S4
  classes are defined within the \pkg{sampSurf} package to facilitate
  sampling surface generation. Note that it is not necessary to work
  with the class structures directly if all you want to do is generate a
  sampling surface for a population of synthetic logs or trees. However,
  some familiarization with the basic class structure is helpful in
  order to understand how the pieces fit together into a final sampling
  surface. Again, the vignettes are a source of much information.

%  
  \subsection{The \dQuote{Stem} class\ldots}{

  Objects of the non-virtual classes below can be created using
  constructor functions of the same name, see the \emph{Object
  Constructors} section for details.

  \tabular{ll}{
	\code{\linkS4class{Stem}} \tab Virtual base class for the following\cr
	\code{\linkS4class{downLog}} \tab A class for down logs\cr
	\code{\linkS4class{standingTree}} \tab A class for standing trees\cr
  }
  
  }%Stem


%
  \subsection{The \dQuote{StemContainer} class\ldots}{

  Container classes hold collections of individual \dQuote{Stem}
  subclass objects of the corresponding name\ldots

  \tabular{ll}{
	\code{\linkS4class{StemContainer}} \tab Virtual base class for the following\cr
	\code{\linkS4class{downLogs}} \tab Container class for a population  of down log objects\cr
	\code{\linkS4class{standingTrees}} \tab Container class for a population of standing tree objects\cr
  }
  
  }%StemContainer


%	
  \subsection{The \dQuote{Tract} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{Tract}} \tab Land area holding the population of individuals \cr
	\code{\linkS4class{bufferedTract}} \tab A "Tract" subclass with a buffer \cr
  }

  The above classes are derived from the \code{\linkS4class{Raster}}
  class in the \pkg{raster} package. They conform to a rectangular area
  of ground such as a plot or any tract of land, tessellated into grid cells.

  }%Tract



%	
\subsection{The \dQuote{MonteCarloSampling} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{MonteCarloSampling}} \tab Virtual base class for
	Monte Carlo sampling \cr
	\code{\linkS4class{crudeMonteCarlo}} \tab Crude Monte Carlo sampling \cr
	\code{\linkS4class{importanceSampling}} \tab Importance subsampling \cr
	\code{\linkS4class{controlVariate}} \tab Control variate sampling \cr
	\code{\linkS4class{antitheticSampling}} \tab Antithetic sampling
	  applied to any of the above \cr
  }

  The above classes allow one to use the Monte Carlo methods mentioned
  to sample from individual "\code{\linkS4class{Stem}}" subclass
  objects. With these stand-alone classes, there is no areal context,
  sampling is done within individual stems.

  \subsection{Associated Container classes\ldots}{

  \tabular{ll}{
	\code{\linkS4class{mcsContainer}} \tab A collection of
	  "\code{MonteCarloSampling}" objects \cr
	\code{\linkS4class{antitheticContainer}} \tab A collection of
	"\code{antitheticSampling}" objects\cr
  }
  }

  }%MonteCarloSampling

  

%
  \subsection{The \dQuote{ArealSampling} class\ldots}{

	These classes define the different sampling methods available and
	provide the method-based information for the subsequent construction of
	inclusion zones\ldots

  \tabular{ll}{
	\code{\linkS4class{ArealSampling}} \tab A class structure for describing areal sampling methods \cr
    \code{\linkS4class{circularPlot}} \tab A subclass of "ArealSampling" for circular plot construction\cr
	\code{\linkS4class{pointRelascope}} \tab A subclass of "ArealSampling" for point relascope sampling\cr
	\code{\linkS4class{perpendicularDistance}} \tab A subclass of
	      "ArealSampling" for variants of perpendicular distance
	      sampling\cr 
	\code{\linkS4class{distanceLimited}} \tab A subclass of "ArealSampling" for distance limited sampling\cr
	\code{\linkS4class{angleGauge}} \tab A subclass of
	      "ArealSampling" for angle gauge (e.g., prism) sampling\cr
	\code{\linkS4class{lineSegment}} \tab A subclass of "ArealSampling"
	     for line-oriented sampling methods\cr
	}

  }%ArealSampling


%
  \subsection{The \dQuote{InclusionZone} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{InclusionZone}} \tab Virtual base class for combining the
	   sampling method with the individual down log or tree objects.\cr
	%
	\emph{Classes for down logs\ldots} \tab \cr
	\code{\linkS4class{downLogIZ}} \tab Virtual subclass for all down log
	  \code{InclusionZone} objects.\cr
	\code{\linkS4class{standUpIZ}} \tab Class for the "standup" method
	  of sampling down logs.\cr
	\code{\linkS4class{sausageIZ}} \tab Class for the "sausage" method
	  of sampling down logs.\cr
	\code{\linkS4class{chainSawIZ}} \tab Class for the "chainsaw" method
	  of sampling down logs.\cr
	\code{\linkS4class{pointRelascopeIZ}} \tab Class for the point relascope method
	  of sampling down logs.\cr
	\code{\linkS4class{perpendicularDistanceIZ}} \tab Class for the
	  perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{omnibusPDSIZ}} \tab Class for the omnibus
	  perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{distanceLimitedPDSIZ}} \tab Class for the
	  distance limited perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{omnibusDLPDSIZ}} \tab Class for the omnibus
	  distance limited perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{hybridDLPDSIZ}} \tab Class for the "hybrid"
	  distance limited perpendicular distance method of sampling down logs.\cr
	\code{\linkS4class{distanceLimitedIZ}} \tab Class for the 
	  distance limited method of sampling down logs.\cr
	\code{\linkS4class{distanceLimitedMCIZ}} \tab Class for the 
	distance limited Monte Carlo method of sampling down logs.\cr
	%
	\emph{Classes for standing trees\ldots} \tab \cr
	\code{\linkS4class{standingTreeIZ}} \tab Virtual subclass for all
	  standing tree \code{InclusionZone} objects.\cr
	\code{\linkS4class{circularPlotIZ}} \tab Class for the
	  sampling standing trees using circulat plots.\cr
	\code{\linkS4class{horizontalPointIZ}} \tab Class for
	  horizontal point sampling of standing trees.\cr
	\code{\linkS4class{horizontalPointCMCIZ}} \tab Class for crude Monte
	  Carlo subsampling within horizontal point sampling of standing trees.\cr
	\code{\linkS4class{horizontalPointISIZ}} \tab Class for importance
	  subsampling within horizontal point sampling of standing trees.\cr
	\code{\linkS4class{horizontalPointCVIZ}} \tab Class for control variate
	  subsampling within horizontal point sampling of standing trees.\cr
	\code{\linkS4class{criticalHeightIZ}} \tab Class for
	  critical height sampling of standing trees.\cr
	\code{\linkS4class{importanceCHSIZ}} \tab Class for importance
	  critical height sampling of standing trees.\cr
	\code{\linkS4class{antitheticICHSIZ}} \tab Class for antithetic importance
	  critical height sampling of standing trees.\cr
	\code{\linkS4class{pairedAICHSIZ}} \tab Class for paired antithetic importance
	  critical height sampling of standing trees.\cr
	\code{\linkS4class{horizontalLineIZ}} \tab Class for
	  horizontal line sampling of standing trees.\cr
	\emph{other\ldots} \tab\cr
	\code{\linkS4class{MonteCarloSamplingIZ}} \tab This class allows combining Monte Carlo
	  with areal methods.\cr
	%}
	} %tabular

  }%InclusionZone

  
%
  \subsection{The \dQuote{izContainer} class\ldots}{

  The following are container classes that hold collections of objects
  of the corresponding type for inclusion zones generated based on down
  logs or standing trees and the desired sampling methods. Each holds a
  collection of objects that are a subclass of \dQuote{downLogIZ} or
  \dQuote{standingTreeIZ}, respectively.

  \tabular{ll}{  
	\code{\linkS4class{izContainer}} \tab Virtual base class for the following\cr
	\code{\linkS4class{downLogIZs}} \tab Holds inclusion zone objects associated with down logs\cr
	\code{\linkS4class{standingTreeIZs}} \tab Holds inclusion zone objects associated with standing trees\cr
  }
  
  }%izContainer
  

%  
  \subsection{The \dQuote{InclusionZoneGrid} class\ldots}{

  \tabular{ll}{  
	\code{\linkS4class{InclusionZoneGrid}} \tab Not for general use, see
	  documentation for details \cr
	\code{\linkS4class{csFullInclusionZoneGrid}} \tab Not for general
	  use, see documentation for details \cr
	}
  }%InclusionZoneGrid
  

%  
  \subsection{The \dQuote{sampSurf} class\ldots}{

  \tabular{ll}{
	\code{\linkS4class{sampSurf}} \tab The class structure for the final
	  sampling surface simulation \cr
	}

  }%sampSurf

  Again, many of these classes will only be of interest to someone
  wanting to do special simulations or for someone who wants to add
  extensions--for example, new sampling methods. 
	
} %classes


%--------------------------------------------------------------------------
%  constructors section...
%
\section{Object Constructors}{
  
  For each of the classes defined in the table above, we must be able to
  create objects that can be used in \R. This is done using
  class-specific "constructor" methods that take the drudgery away from
  creating what can often be somewhat complicated (with all the
  graphical components) new object instances. There may be more than one
  constructor for a given class of object, and these are differentiated
  by the method signature; see the links provided below for more details\ldots

%
  \subsection{\dQuote{Stem} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{downLog}} \tab Constructor for individual
	  "downLog" objects\cr   
	\code{\link{standingTree}} \tab Constructor for individual
	  "standingTree" objects\cr   
  }

  }%Stem

%
  \subsection{The \dQuote{StemContainer} class\ldots}{

  \tabular{ll}{
	\code{\link{downLogs}} \tab Constructs a container object holding
	  multiple "downLog" objects\cr
	\code{\link{standingTrees}} \tab Constructs a container object holding
	  multiple "standingTree" objects \cr
  }
  
  }%StemContainer
	
%
  \subsection{\dQuote{Tract} class constructors\ldots}{

  \tabular{ll}{
    \code{\link{Tract}} \tab Constructor for "Tract" objects \cr 
	\code{\link{bufferedTract}} \tab Constructs a
	"bufferedTract" object\cr
  }
  }%Tract


%	
\subsection{The \dQuote{MonteCarloSampling} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{crudeMonteCarlo}} \tab Apply Crude Monte Carlo
	  sampling \cr
	\code{\link{importanceSampling}} \tab Apply importance subsampling \cr
	\code{\link{controlVariate}} \tab Apply control variate sampling \cr
	\code{\link{antitheticSampling}} \tab Apply antithetic sampling
	  to any of the above \cr
  }

  \subsection{Associated Container class constructors\ldots}{

  \tabular{ll}{
	\code{\link{mcsContainer}} \tab A collection of
	  "\code{MonteCarloSampling}" objects \cr
	\code{\link{antitheticContainer}} \tab A collection of
	"\code{antitheticSampling}" objects\cr
  }
  }

  }%MonteCarloSampling

  
%
  \subsection{\dQuote{ArealSampling} class constructors\ldots}{

  \tabular{ll}{
    \code{\link{circularPlot}} \tab Constructs a circular plot object
	  \cr
    \code{\link{pointRelascope}} \tab Constructs a point relascope object
	  \cr
    \code{\link{perpendicularDistance}} \tab Constructs a perpendicular
      distance object \cr
    \code{\link{distanceLimited}} \tab Constructs a distance limited object\cr
    \code{\link{angleGauge}} \tab Constructs an angle gauge object\cr
    \code{\link{lineSegment}} \tab Constructs a line segment object\cr
	}
  }%ArealSampling
  
%
  \subsection{\dQuote{InclusionZone} class constructors\ldots}{

  \tabular{ll}{	
	\emph{Classes for down logs\ldots} \tab \cr
	\code{\link{standUpIZ}} \tab Creates an "InclusionZone"
	  object for the 'stand-up' sampling method \cr
	\code{\link{sausageIZ}} \tab Creates an "InclusionZone"
	  object for the 'sausage' sampling method \cr
	\code{\link{chainSawIZ}} \tab Creates an "InclusionZone"
	  object for the 'chainsaw sampling method \cr
	\code{\link{pointRelascopeIZ}} \tab Creates an "InclusionZone"
	  object for the 'point relascope' sampling method \cr
	\code{\link{perpendicularDistanceIZ}} \tab Creates an "InclusionZone"
	  object for the 'perpendicular distance' sampling method \cr
	\code{\link{omnibusPDSIZ}} \tab Creates an "InclusionZone"
	  object for the 'omnibus perpendicular distance' sampling method \cr
	\code{\link{distanceLimitedPDSIZ}} \tab Creates an "InclusionZone"
	  object for the 'distance limited perpendicular distance' sampling method \cr
	\code{\link{omnibusDLPDSIZ}} \tab Creates an "InclusionZone"
	  object for the 'omnibus distance limited perpendicular distance' sampling method \cr
	\code{\link{hybridDLPDSIZ}} \tab Creates an "InclusionZone"
	  object for the 'hybrid distance limited perpendicular distance' sampling method \cr
	\code{\link{distanceLimitedIZ}} \tab Creates an "InclusionZone"
	  object for the 'distance limited' sampling method \cr
	\code{\link{distanceLimitedMCIZ}} \tab Creates an "InclusionZone"
	  object for the 'distance limited Monte Carlo' sampling method \cr
	%
	\emph{Classes for standing trees\ldots} \tab \cr
	\code{\link{circularPlotIZ}} \tab Creates an "InclusionZone"
	  object for the fixed-area circular plot sampling method \cr
	\code{\link{horizontalPointIZ}} \tab Creates an "InclusionZone"
	  object for the horizontal point (prism) sampling method \cr
	\code{\link{horizontalPointCMCIZ}} \tab Creates an "InclusionZone"
	  object for HPS with crude Monte Carlo subsampling \cr
	\code{\link{horizontalPointISIZ}} \tab Creates an "InclusionZone"
	  object for HPS with importance subsampling \cr
	\code{\link{horizontalPointCVIZ}} \tab Creates an "InclusionZone"
	  object for HPS with control variate subsampling \cr
	\code{\link{criticalHeightIZ}} \tab Creates an "InclusionZone"
	  object for the critical height (CH) sampling method \cr
	\code{\link{importanceCHSIZ}} \tab Creates an "InclusionZone"
	  object for the importance critical height sampling method \cr
	\code{\link{antitheticICHSIZ}} \tab Creates an "InclusionZone"
	  object for the antithetic importance CH sampling method \cr
	\code{\link{pairedAICHSIZ}} \tab Creates an "InclusionZone"
	  object for the paired antithetic importance CH sampling method \cr
	\code{\link{horizontalLineIZ}} \tab Creates an "InclusionZone"
	object for the horizontal line sampling method \cr
  }
  }%InclusionZone
  
%
  \subsection{The \dQuote{izContainer} class constructors\ldots}{

  \tabular{ll}{  
	\code{\link{downLogIZs}} \tab Creates a collection of
	  "downLogIZ" objects for down logs\cr
	\code{\link{standingTreeIZs}} \tab Creates a collection of
	  "standingTreeIZ" objects for standing trees\cr
  }
  
  }%izContainer

%
  \subsection{\dQuote{InclusionZoneGrid} class constructors\ldots}{

  \tabular{ll}{  	
	\code{\link{izGrid}} \tab Creates an "InclusionZoneGrid"
	  object\cr
	\code{\link{izGridCSFull}} \tab Creates an "csFullInclusionZoneGrid"
	object\cr
  }
  }%InclusionZoneGrid

%   
  \subsection{\dQuote{sampSurf} class constructors\ldots}{

  \tabular{ll}{
	\code{\link{sampSurf}} \tab Constructor for "sampSurf"
	  objects\cr 
	}
  }%sampSurf
	
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
	\code{\link{hist}} \tab Create a histogram of object attributes\cr	
	\code{\link{plot}} \tab  To plot one of the package objects\cr
	\code{\link{plot3D}} \tab  To plot "sampSurf" objects using
	  package 'rgl'\cr 
%when fixed, change this back to:	  \pkg{rgl}\cr 
	\code{\link{show}} \tab  To succinctly print one of the package objects\cr
	\code{\link{summary}} \tab  To summarize one of the package objects\cr
  } %tabular
  
} %summary and plotting



\section{Helper methods}{
  Slots in S4 objects can be accessed directly using \code{\link{slot}}
  method or the \code{\link{@}} operator. However, some "helper"
  functions have been provided for certain objects, along with other
  potentially useful routines\ldots

  \tabular{ll}{
	\code{\link{area}} \tab Returns the area of a spatial object\cr
	\code{\link{bbox}} \tab Return the bounding box for a spatial
	  object\cr
	\code{\link{bboxCheck}} \tab Check for a valid bounding box for a
	  spatial object\cr
	\code{\link{bboxToPoly}} \tab Convert a \code{bbox} matrix to a
	  "\code{\linkS4class{SpatialPolygons}}" object\cr
	\code{\link{bboxSum}} \tab	Calculate an overall \code{bbox} from and
	  array of \code{bbox} matrices\cr
	\code{\link{perimeter}} \tab Returns the perimeter of a spatial
	  object, which can then be plotted\cr
	\code{\link{spCircle}} \tab Returns a circular
	"\code{\linkS4class{SpatialPolygons}}" object\cr
	\code{\link{getID}} \tab Returns the IDs associated with objects\cr
	\code{\link{getProxy}} \tab Return the desired proxy function for MC
	  sampling\cr 
  } %tabular
  
} %helper methods




\section{Coercion methods}{
  Object coercion is accomplished in S4 by a call to \code{\link{as}}
  with the appropriate object and class to which one wants to
  convert\ldots

  \tabular{ll}{
	\code{\link{as}} \tab Convert from a "downLogs" or "standingTrees" object to a
	     data frame\cr
	\code{\link{as}} \tab Convert "downLogIZs" or "standingTreeIZs" object to respective "StemContainer"\cr
	} %tabular

	The resulting data frame in the first method will be compatible with those generated
	from calls to either \dQuote{sampleLogs} or
	\dQuote{sampleTrees}. The second takes the drudgery out of getting a
	log collection from, say, the \code{izContainer} slot of a \code{sampSurf} object.
  
} %coercion methods



\section{Miscellaneous methods}{
  Some methods that might be useful as stand-alone routines\ldots

  \tabular{ll}{
	\code{\link{boltDimensions}} \tab Calculates segment volumes,
	  surface area, etc.\cr
	\code{\link{gridCellEnhance}} \tab Draws grid lines and centers.\cr
	\code{\link{heapIZ}} \tab Used to heap inclusion zone grids into a
	  sampling surface.\cr
    \code{\link{initRandomSeed}} \tab Initialize \code{.Random.seed}.\cr
	\code{\link{sampleLogs}} \tab Draws a simulated sample of down logs and
	  returns a data frame. \cr
	\code{\link{segmentVolume}} \tab Calculates segment volumes for down
	  logs or standing trees. \cr
    \code{\link{taperInterpolate}} \tab Interpolates diameters or
	  lengths within down logs or standing trees.\cr
    \code{\link{transparentColorBase}} \tab Transparancy in base graphics.\cr
	\code{\link{sampleTrees}} \tab Draws a simulated sample of standing
	  trees and returns a data frame. \cr
	
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
	\code{\link{.StemEnv}} \tab This environment holds parameters for
	  much of the package. \cr
	
  } %tabular
     
} %environments
 

\section{\sQuote{sampSurf} class unions}{
  
  There are a few class unions that are documented here only for the
  sake of completeness. It is unlikely that the casual user would ever
  need to be concerned about these.
  
  \tabular{ll}{
	\code{\linkS4class{dlsIZNull}} \tab Accepts either class
	  "distanceLimitedIZ" (or subclass) or "NULL". \cr
	\code{\linkS4class{dlsNumeric}} \tab Accepts either class
	  "distanceLimited" or "numeric". \cr
	\code{\linkS4class{monteBSSampleOrNULL}} \tab Accepts either class
	  "monteBSSample" or "NULL". \cr
	\code{\linkS4class{monteNTSampleOrNULL}} \tab Accepts either class
	  "monteNTSample" or "NULL". \cr
	%next only used in solidType slot...  
	\code{\linkS4class{numericNULL}} \tab Accepts either class
	  "numeric" or "NULL". \cr
	\code{\linkS4class{pdsIZNull}} \tab Accepts either class
	  "perpendicularDistanceIZ" (or subclass) or
	  "NULL". \cr
    \code{\linkS4class{horizontalPointMonteCarloSamplingIZ}} \tab
	  Accepts any of the MC and horizontal point sampling combinations.\cr
  } %tabular
  
} %class unions  

\section{\dQuote{monte}: When is \emph{n} Sufficiently Large? }{
  
  This section has both classes and methods defined for doing Monte
  Carlo analysis of the convergence of confidence interval catch rates
  as sample size increases.
  
  \tabular{ll}{
	\code{\linkS4class{montePop}} \tab Population class with
	  constructor \code{\link{montePop}}. \cr
	\code{\linkS4class{monteSample}} \tab Virtual repeated sampling class with
	  the following two subclasses. \cr
	\code{\linkS4class{monteNTSample}} \tab Normal theory repeated sampling class with
	  constructor \code{\link{monteNTSample}}. \cr
	\code{\linkS4class{monteBSSample}} \tab Bootstrap repeated sampling class with
	  constructor \code{\link{monteBSSample}}. \cr
	\code{\linkS4class{monte}} \tab Main Monte Carlo class with
	  constructor \code{\link{monte}}. \cr
	} %tabular

  Please see the \sQuote{\dQuote{monte}: When is \emph{n} Sufficiently
  Large?} vignette for more details and examples.\cr

  
} %monte  



\author{
Jeffrey H. Gove
}


\references{ The first two references describe the sampling surface
method in detail. The rest provide some examples of its use, or document
some of the methods in the package, and are not exhaustive.

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
%
\item{}{Gove, J. H., Ducey, M. J. and Valentine, H. T. 2012. A distance
  limited method for sampling downed coarse woody debris. \emph{Forest Ecology
	and Management} \bold{282}:53--62. }
%
\item{}{Ducey, M. J., Williams, M. S., Gove, J. H., Roberge, S. and
  Kenning, R. S. 2013. Distance limited perpendicular distance sampling
  for coarse woody material: Theory and field results. \emph{Forestry}
  \bold{86}:119--128}
%
\item{}{Gove, J. H., Ducey, M. J., Valentine, H. T. and Williams,
  M. S. 2013. A comprehensive comparison of perpendicular distance
  sampling methods for sampling down coarse woody
  debris. \emph{Forestry} \bold{86}:129--143.}
%
\item{}{Lynch,T. B. and Gove, J. H. 2013. An antithetic variate to
  facilitate upper-stem height measurements for critical height sampling
  and fixed-radius plot sampling with importance
  sampling. \emph{Canadian Journal of Forest Research} (forthcoming). }

}%describe

}%references



\keyword{ package }
\seealso{
\code{\link{sp}}, \code{\link[raster:raster-package]{raster}}
}
%\examples{

%}
