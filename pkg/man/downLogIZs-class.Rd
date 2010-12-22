\name{downLogIZs-class}
\Rdversion{1.1}
\docType{class}
\alias{downLogIZs-class}
\alias{plot,downLogIZs,missing-method}
\alias{sampSurf,downLogIZs,Tract-method}
\alias{show,downLogIZs-method}
\alias{summary,downLogIZs-method}

\title{Class "downLogIZs"}

\description{ Like the "\code{\linkS4class{downLogs}}" class, this a
 'container' class that allows multiple objects of a specific subclass
 of \code{\linkS4class{downLogIZ}} to be grouped into a population or
 collection. Its specific intent it to hold a collection of
 "\code{\linkS4class{InclusionZone}}" objects for a given log
 population, that will ultimately be used to generate a sampling surface
 for a given sampling method.  It is a simplified view of traditional
 container classes in that is does not currently allow for deletion or
 addition of objects.  Please see \dQuote{\emph{The InclusionZone
 Class}} vignette for more information.}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("downLogIZs", ...)}; however, as in the other classes
within this package, constructors have been written to simplify the
process. The \code{\link{downLogIZs}} constructor should therefore be used
in preference to \code{new}.  The constructor, \code{\link{downLogIZs}}, has only one
 form. }

\section{Slots}{
  \describe{
	
    \item{\code{iZones}:}{Object of class \code{"list"}: This slot holds
	  a list of objects that all correspond to the same subclass of the
	  "\code{\linkS4class{downLogIZ}}" class. In other words, each item
	  must have been generated from the same sampling method; i.e., all
	  of class "\code{\linkS4class{sausageIZ}}," for example.  }
	
    \item{\code{units}:}{Object of class \code{"character"}: The units
      for all of the logs in the collection; note that they must all be
      measured in the same units. }
	
    \item{\code{bbox}:}{Object of class \code{"matrix"}: The overall
      bounding box for the collection used in plotting. }
	
    \item{\code{description}:}{Object of class \code{"character"}: A
      description of the collection. }
	
  }
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "downLogIZs", y = "missing")}:
      Graphical plot of the collection. }
	
    \item{sampSurf}{\code{signature(object = "downLogIZs", tract =
		"Tract")}: Create a sampling surface for the collecition on a
		given "Tract". }
	
      \item{show}{\code{signature(object = "downLogIZs")}: Print some
      information about the collection.  }
	  
    \item{summary}{\code{signature(object = "downLogIZs")}: A summary of
	 the collection. }
	 }
}

\author{
Jeffrey H. Gove %, \email{jhgove@unh.edu}
}

\seealso{
For subclasses of \code{\linkS4class{downLogIZ}}, see: \code{\linkS4class{standUpIZ}},
  \code{\linkS4class{chainSawIZ}} and \code{\linkS4class{sausageIZ}}.
}

\examples{
showClass("downLogIZs")
}
\keyword{classes}
