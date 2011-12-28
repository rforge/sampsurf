\name{standingTrees-class}
\Rdversion{1.1}
\docType{class}
\alias{standingTrees-class}
\alias{hist,standingTrees-method}
\alias{plot,standingTrees,missing-method}
\alias{summary,standingTrees-method}

\title{Class \code{"standingTrees"}}

\description{ The \dQuote{standingTrees} class is a simplified container
class that can hold multiple objects of class
\dQuote{\code{\linkS4class{standingTree}}}. Its specific purpose is to hold a
population of standing trees that are either generated synthetically as part
of a simulation, or a collection from field measurements. The
constructor of the same name has several different forms corresponding
to possible argument signatures.
}

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("standingTrees", ...)}; however, as in the other classes
within this package, constructors have been written to simplify the
process. The \code{\link{standingTrees}} constructor should therefore be
used in preference to \code{new}.  }

\section{Slots}{

  Please see the virtual base class,
  \dQuote{\code{\linkS4class{StemContainer}}}, for additional slots
  definitions. 
  
  \describe{
    \item{\code{trees}:}{Object of class \code{"list"}: This holds the
      collection of \dQuote{\code{\link{standingTree}}} objects. }
  }

Please note that at the present time this class only partially meets the
requirements of a true \dQuote{container class} in object oriented
programming. This is because it does not as yet have methods for object
deletion, editing, or addition to the list of standing trees. Because
the statistics and bounding box are tied to the collection, a caution is
in order regarding changing in any way the objects within your \R
code. The best way to handle this is to simply extract the list from the
object, do whatever editing has to be done to it, then use the
constructor below to make a new object. Then everything will be
correctly represented within the object. 

}

\section{Extends}{
Class \code{"\linkS4class{StemContainer}"}, directly.
}

\section{Methods}{
  \describe{
    \item{hist}{\code{signature(x = "standingTrees")}: Displays a histogram
      of different variables in the collection. }
	
    \item{plot}{\code{signature(x = "standingTrees", y = "missing")}: Plot
      the collection. }
	
    \item{summary}{\code{signature(object = "standingTrees")}: Same as
      \code{show} currently. }
	 }
   }
   

\references{
  \dQuote{The Stem Class} vignette in this package.
}

\author{
Jeffrey H. Gove
}


\seealso{
  
Classes: "\code{\linkS4class{standingTree}}",
"\code{\linkS4class{Stem}}", "\code{\linkS4class{StemContainer}}"; and
to construct objects, the \code{\link{standingTrees}} constructor,
perhaps using \code{\link{sampleTrees}}.

}

\examples{
showClass("standingTrees")
buff = matrix(c(0,100,0,100), nrow=2, byrow=TRUE,
              dimnames=list(c('x','y'),c('min','max')))
st = sampleTrees(10, dbh = c(10,25), sampleRect = buff)
sts = standingTrees(st)
summary(sts)
}
\keyword{classes}
