\name{mirageInclusionZoneGrid-class}
\Rdversion{1.1}
\docType{class}
\alias{mirageInclusionZoneGrid-class}
%\alias{plot,mirageInclusionZoneGrid,missing-method}

\title{Class \code{"mirageInclusionZoneGrid"}}

\description{
  
This class is a subclass of \code{"\linkS4class{InclusionZoneGrid}"}
that is specifically for use in developing sampling surfaces where the
mirage method is to be implemented on a
\code{"\linkS4class{mirageTract}"}. It has a separate constructor
method, which should be used to construct the object.

}

\section{Objects from the Class}{
  
Objects can be created by calls of the form
\code{new("mirageInclusionZoneGrid", ...)}.  However, this is not
recommended because the objects are quite complex. Instead, please use
the object constructor \code{\link{izGridMirage}} to create new
objects. More details are found in the vignettes listed below.

}

\section{Slots}{

  As noted, this class is a subclass of
  "\code{\linkS4class{InclusionZoneGrid}}"; it adds several slots to the
  class, all other slots not defined below are as described in the
  superclass\ldots
  
  \describe{
	
    \item{\code{slopOver}:}{Object of class \code{"logical"}: A vector of
     of length four which flags where there was any boundary overlap in the
     cardinal ('north','south','east','west') directions: \code{TRUE} if
     so, \code{FALSE} otherwise. Note that one can easily check this
     vector to determine whether the associated \code{*.polygon} and
     \code{*.grid} slots are non-\code{NULL}.}
	
   \item{\code{north.polygon}:}{Object of class \code{"SPNULL"}: If
     \code{slopover['north']}, then this slot holds the external portion
     of the inclusion zone polygon due to boundary slopover on the north
     side. Otherwise, it is \code{NULL}. }
   
   \item{\code{north.grid}:}{Object of class \code{"RLNULL"}: If
     \code{slopover['north']}, then this slot holds the external portion
     of the extended grid on the north side. Otherwise, it is
     \code{NULL}. }
   
   \item{\code{south.polygon}:}{Object of class \code{"SPNULL"}: If
     \code{slopover['south']}, then this slot holds the external portion
     of the inclusion zone polygon due to boundary slopover on the south
     side. Otherwise, it is \code{NULL}. }
   
   \item{\code{south.grid}:}{Object of class \code{"RLNULL"}: If
     \code{slopover['south']}, then this slot holds the external portion
     of the extended grid on the south side. Otherwise, it is
     \code{NULL}. }
   
   \item{\code{east.polygon}:}{Object of class \code{"SPNULL"}: If
     \code{slopover['east']}, then this slot holds the external portion
     of the inclusion zone polygon due to boundary slopover on the east
     side. Otherwise, it is \code{NULL}.  }
   
   \item{\code{east.grid}:}{Object of class \code{"RLNULL"}: If
     \code{slopover['east']}, then this slot holds the external portion
     of the extended grid on the east side. Otherwise, it is
     \code{NULL}. }
   
   \item{\code{west.polygon}:}{Object of class \code{"SPNULL"}: If
     \code{slopover['west']}, then this slot holds the external portion
     of the inclusion zone polygon due to boundary slopover on the west
     side. Otherwise, it is \code{NULL}. }
   
   \item{\code{west.grid}:}{Object of class \code{"RLNULL"}: If
     \code{slopover['west']}, then this slot holds the external portion
     of the extended grid on the west side. Otherwise, it is
     \code{NULL}. }
   
   \item{\code{izGrid.extended}:}{Object of class \code{"izgNULL"}: This
     is the object showing the extended grid that encompases any slopover
     regions external to the boundary, where applicable. Otherwise, it
     is \code{NULL} if there is no slopover on any side. }
   
} } %describe, section

\section{Extends}{
Class \code{"\linkS4class{InclusionZoneGrid}"}, directly. \cr
Class \code{"\linkS4class{izgNULL}"}, by class "InclusionZoneGrid", distance 2.
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "mirageInclusionZoneGrid", y =
      "missing")}: This will plot the object. }
	 }
   }
   
\references{ See the \dQuote{Mirage Method} and
\dQuote{InclusionZoneGrid} vignettes for more explanation.

}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}


\seealso{
  \code{\link{izGridMirage}},
  \code{\linkS4class{InclusionZoneGrid}}, \code{\linkS4class{sampSurf}}
}

\examples{
showClass("mirageInclusionZoneGrid")
}
\keyword{classes}
