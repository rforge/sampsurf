\name{InclusionZoneGrid-class}
\Rdversion{1.1}
\docType{class}
\alias{InclusionZoneGrid-class}
\alias{bbox,InclusionZoneGrid-method}
\alias{heapIZ,InclusionZoneGrid,Tract-method}
\alias{izGridCSFull,InclusionZoneGrid,Tract-method}
\alias{plot,InclusionZoneGrid,missing-method}
\alias{show,InclusionZoneGrid-method}
\alias{summary,InclusionZoneGrid-method}

\title{Class "InclusionZoneGrid"}

\description{ This class really provides the functional building block
  for piecing inclusion zones together into the final sampling surface
  object. These functions should not be required by the casual user of
  this package, but will be required for writing extensions for new
  methods as they are added. Basically, the class defines a subgrid that
  just encapsulates an "\code{\linkS4class{InclusionZone}}"
  object. Attribute values (e.g., volume) are assigned to the subgrid
  cells falling within the inclusion zone of the object, with zero
  values outside the zone. This subgrid is aligned to the overall tract
  grid in the constructor, and upon return, can be "heaped" or "piled"
  additively onto the "\code{\linkS4class{Tract}}" object to develop the
  sampling surface.  \cr

  In order to make a complete enumeration of the chainsaw method under
  Protocol 1 of Gove and Van Deusen (2011), we need an extension to this
  class. The subclass "\code{\linkS4class{csFullInclusionZoneGrid}}"
  should be consulted for such tasks.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("InclusionZoneGrid", ...)}. However, this is not recommended
because the objects are fairly complex. Instead, one can use the object
constructor \code{\link{izGrid}} to create new objects. More details are
found in \emph{"The InclusionZoneGrid Class"} vignette.
}

\section{Slots}{
  \describe{
    \item{\code{description}:}{Object of class \code{"character"}: A
      description of the object if desired. }
	
    \item{\code{iz}:}{Object of class \code{"InclusionZone"}: An object
    of one of the "InclusionZone" subclasses such as
    "\code{\linkS4class{standUpIZ}}." } 
	
    \item{\code{grid}:}{Object of class \code{"RasterLayer"}: This is
      the minimal encompassing grid that must be aligned to a "Tract"
      object's extents in order to build a sampling surface. }
	
    \item{\code{data}:}{Object of class \code{"data.frame"}: A data
	  frame holding the values for each of the per unit area estimates
	  available in the “InclusionZone” object in the columns, with rows
	  matching grid cells. }
	
    \item{\code{bbox}:}{Object of class \code{"matrix"}: The overall
	  bounding box for the object, which includes the inclusion zone and
	  the “Stem” subclass object plus the grid. Sometimes the inclusion
	  zone itself includes the stem (e.g., the sausage method), but
	  other times it does not (e.g., chainsaw method). This is used
	  primarily in graphing the object. }
	
  }
}

\section{Methods}{
  \describe{
    \item{bbox}{\code{signature(obj = "InclusionZoneGrid")}: ... }
    \item{heapIZ}{\code{signature(izgObject = "InclusionZoneGrid", tract = "Tract")}: ... }
    \item{izGridCSFull}{\code{signature(izGridSausage = "InclusionZoneGrid", tract = "Tract")}: ... }
    \item{plot}{\code{signature(x = "InclusionZoneGrid", y = "missing")}: ... }
    \item{show}{\code{signature(object = "InclusionZoneGrid")}: ... }
    \item{summary}{\code{signature(object = "InclusionZoneGrid")}: ... }
	 }
   }
   
\references{
Gove, J. H. and Van Deusen, P. C. 2011. On fixed-area plot sampling for
downed coarse woody debris. \emph{Forestry}. (Accepted).\cr
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
\code{\linkS4class{InclusionZone}}, \code{\linkS4class{Stem}},
\code{\linkS4class{Tract}}, \code{\linkS4class{sampSurf}}, \code{\linkS4class{csFullInclusionZoneGrid}}
}

\examples{
showClass("InclusionZoneGrid")
}
\keyword{classes}
