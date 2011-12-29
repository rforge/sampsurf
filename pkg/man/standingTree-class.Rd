\name{standingTree-class}
\Rdversion{1.1}
\docType{class}
\alias{standingTree-class}
\alias{bbox,standingTree-method}
%\alias{perimeter,standingTree-method}
\alias{plot,standingTree,missing-method}
\alias{summary,standingTree-method}

\title{Class \code{"standingTree"}: Representation of Standing Trees}

\description{
  A subclass of virtual class \dQuote{\code{\linkS4class{Stem}}} that can be used to
  represent standing trees. The class provides for creation and graphical
  display of \dQuote{standingTree} objects.

  Detail examples and information concerning this class is found in
  \dQuote{The Stem Class} vignette, which should be consulted for more
  details on creating and using objects from this class.
}

\section{Objects from the Class}{

Objects can be created by calls of the form \code{new("standingTree",
...)}. However, this is \emph{not} recommended. The object has a number
of required slots that can be somewhat complex to calculate. Therefore,
a constructor function \code{\link{standingTree}} has been provided that
will make the desired object with the correct values for the slots. It
is strongly recommended that one use this generic's methods to create
the tree objects.  }

\section{Slots}{

  Please note that all diameters below are presumed to be in the
  \emph{same} units as height, i.e., meters for \dQuote{metric}, and
  feet for \dQuote{English} \code{units}.

  In addition to the slots provided by the virtual superclass
  \dQuote{\code{\linkS4class{Stem}}}, the following slots are
  represented\ldots 
  
  \describe{
	
    \item{\code{buttDiam}:}{Object of class \code{"numeric"}: The
      diameter at the butt (e.g., stump) of the tree in the same units a height. }
	
    \item{\code{topDiam}:}{Object of class \code{"numeric"}: The
      diameter at the tip of the tree in the same units a height. }
	
    \item{\code{height}:}{Object of class \code{"numeric"}: The total
      height of the tree in feet or meters.  }
	
    \item{\code{dbh}:}{Object of class \code{"numeric"}: The diameter at
      breast height of the tree.}
	
    \item{\code{ba}:}{Object of class \code{"numeric"}: The tree basal
      area. }
	
    \item{\code{solidType}:}{Object of class \code{"numericNULL"}: Object of class
	  \code{"\linkS4class{numericNULL}"}: The form parameter in the
	  simple taper and volume equation presented in Van Deusen (1990)
	  and Gove and Van Deusen (2011). }
	
    \item{\code{treeVol}:}{Object of class \code{"numeric"}: The tree's volume. }
	
    \item{\code{surfaceArea}:}{Object of class \code{"numeric"}: The
      tree's surface area. }
	
    \item{\code{biomass}:}{Object of class \code{"numeric"}: The tree's
      stem biomass.  This will be \code{NA} if no conversion was specified at
      object creation. }
	
    \item{\code{carbon}:}{Object of class \code{"numeric"}: The tree's
      stem carbon content. This will be \code{NA} if no conversion was
      specified at object creation.}
	
    \item{\code{conversions}:}{Object of class \code{"numeric"}: A
      vector with names \code{c('volumeToWeight','weightToCarbon')}
      specifying the conversion factors for woody biomass and carbon
      content. }
	
    \item{\code{taper}:}{Object of class \code{"data.frame"}: The tree's
	  taper, either as specified from measurements, or as generated via
	  \code{\link{standingTree}}. }
	
    \item{\code{profile}:}{Object of class \code{"data.frame"}: The tree
	  profile as generated from the taper. The tree is assumed to be
	  oriented standing North, and the pith of the base at the origin. }
	
    \item{\code{transTree}:}{Object of class \code{"matrix"}: Same as
      \code{profile}, but translated as desired. }
	
    \item{\code{spTree}:}{Object of class \code{"SpatialPolygons"}: A
	  \code{\linkS4class{SpatialPolygons}} representation of
	  \code{transTree} (i.e., the full standing tree) via the
	  \code{\link{sp}} package.  }
	
    \item{\code{spDBH}:}{Object of class \code{"SpatialPolygons"}: A
	  \code{\linkS4class{SpatialPolygons}} representation of the tree
	  \code{dbh}.  }

  }
}

\section{Extends}{
Class \code{"\linkS4class{Stem}"}, directly.
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "standingTree")}:  Returns the bounding
      box for the object. }
	
    \item{perimeter}{\code{signature(object = "standingTree")}: Extract
      the perimeter as a "\code{\link{SpatialPolygons}}" object for
      possible plotting. }
	
    \item{plot}{\code{signature(x = "standingTree", y = "missing")}:
      Plots the object. }
	
    \item{summary}{\code{signature(object = "standingTree")}: A summary
	  of the object. }
	 }
   }
   
\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry} \emph{Forestry} \bold{84}:109--117.\cr\cr
%
Van Deusen, P.C. 1990. Critical height versus importance sampling for
log volume: does critical height prevail? \emph{Forest Science}\cr
\bold{36}(4):930--938. \cr\cr

\dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
"\code{\linkS4class{Stem}}", "\code{\linkS4class{downLog}}",
 "\code{\linkS4class{StemContainer}}"
}

\examples{
showClass("standingTree")
}
\keyword{classes}
