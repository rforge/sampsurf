\name{smithPlot}
\alias{smithPlot}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
A function to make H. F. Smith plots
}

\description{ This function will create so-called \dQuote{Smith plots} from the
  simulation results of several objects of class \code{\linkS4class{sampSurf}}.
  The simulation results should be stored in a named list structure as described
  below for input to this routine. Please see \cite{Smith (1938)} for
  the original study and \cite{Gove (2017)} for a recent application.
}

\usage{
smithPlot(hfs,
          showPlot = TRUE,
          ylab = "Total surface variance",
          xlab = "Average Inclusion Zone Area",
          type = "b",
          pch = 19,
          theme = c("ggplot", "custom", "plain", "economist"),
          cols = c("#00526D", "#00A3DB", "#7A2713", "#939598"),
          ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{hfs}{ A named list of named lists with the following structure\ldots\cr\cr
           top = sampling methods; these should be named appropriately, e.g.,
                 \sQuote{hps}, \sQuote{chs}, etc.\cr
           sub = within each sampling method, a list of sampSurf objects,
                 one for each different plot size/baf, etc.; e.g., \sQuote{HPSbaf5},
                 \sQuote{CHSbaf7}, etc.
			   }
			   
  \item{showPlot}{ \code{TRUE}: display the plot; \code{FALSE}: create
	and return the plot only.}
  
  \item{ylab}{ Label for y-axis -- see \code{\link{par}}.}
  
  \item{xlab}{ Label for x-axis -- see \code{\link{par}}. }
  
  \item{type}{ See \code{\link{par}}. }
  
  \item{pch}{ See \code{\link{par}}. }
  
  \item{theme}{ A theme from the provided list -- see
    \href{http://latticeextra.r-forge.r-project.org/}{\pkg{latticeExtra}}
    \sQuote{styles} for more information.}

  \item{cols}{ A vector of alternative colors for the line graphs; \code{NULL} will 
            use the default theme colors; those listed above are essentially
            those from the economist theme.}

  \item{\dots}{ Passed on to \code{\link[lattice]{xyplot}}. }
	  
}
		
\details{

  A conceptual example setup for the \code{hfs} argument as used in the
   manuscript cited below would be\ldots\cr\cr

     $hps\cr
     [1] \dQuote{HPSbaf3} \dQuote{HPSbaf5} \dQuote{HPSbaf7} \dQuote{HPSbaf9}\cr
     $chs\cr
     [1] \dQuote{CHSbaf3} \dQuote{CHSbaf5} \dQuote{CHSbaf7} \dQuote{CHSbaf9}\cr
     $cmc\cr
     [1] \dQuote{CMCbaf3} \dQuote{CMCbaf5} \dQuote{CMCbaf7} \dQuote{CMCbaf9}\cr
     $cps\cr
     [1] \dQuote{CPSrad5} \dQuote{CPSrad6} \dQuote{CPSrad7} \dQuote{CPSrad9}\cr\cr
	 
   where the \dQuote{$} names are the \sQuote{top} list entries, and
   then each with its own corresponding named \sQuote{sub} list for that
   sampling method with the different \dQuote{sampSurf} objects
   corresponding to the row names in the respective sublist.  Thus there
   are four sampling methods (top names =
   \sQuote{hps},\sQuote{chs},\sQuote{cmc},\sQuote{cps}), each with four
   sets of \dQuote{sampSurf} simulations named as shown, for a total of
   16 \dQuote{sampSurf} objects in the entire list. Please see the
   example below for more information.\cr

   In general, \code{hfs} can be ragged; that is, the sublists do not need to
   be all the same length. Therefore some sampling methods may have
   fewer simulation results than others.
	 
}


\value{
  A list is returned invisibly with\ldots\cr

  \item{ df }{ The data frame that was used to create the figure. }
  \item{ plt }{ The lattice (class \dQuote{trellis}) plot object for the figure. }

}


\references{
%
\describe{
%
\item{}{Gove, J. H. 2017. Some refinements on the comparison of areal
  sampling methods via simulation. \emph{Forests}, (Submitted). }
%
\item{}{Smith, H. F. 1938. An empirical law describing heterogeneity in the
  yields of agricultural crops. \emph{Journal of Agricultural Science},
   \bold{28}:1-23.}

} %describe
} %references


\author{
Jeffrey H. Gove
}

\note{ \cite{Gove (2017)} provides a graphical example created from this
function corresponding to the example given below. That reference also
shows how these plots can be extended for use with the results of a
\acronym{MODWT} wavelet analysis. The main idea behind the variance
relationship dates back to the seminal study of \cite{Smith (1938)}.  }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\dQuote{\code{\linkS4class{sampSurf}}}
}

\examples{
#
#  Using smithPlot entails generating several simulations for each sampling
#  method and thus some rather lengthy code--too much so for
#  reproduction here. However, assume that we have several sampSurf
#  simulation objects as follows (see example in Details above)...
#
#  horizontal point sampling:         sshps.3, sshps.5, sshps.7, sshps.9
#  critical height sampling:          sschs.3, sschs.5, sschs.7, sschs.9
#  HPS /w crude Monte Carlo sampling: sscmc.3, sscmc.5, sscmc.7, sscmc.9
#  all with metric bafs 3, 5, 7 and 9.
#
#  Also we have four sampSurf circular plot sampling objects with plot
#  radii 5, 6, 7, and 9m...
#
#  sscps.5, sscps.6, sscps.7, sscps.9 
#
#  Then we can form the input lists required by smithPlot as...
#
\dontrun{
hps = list(HPSbaf3 = sshps.3, HPSbaf5 = sshps.5, HPSbaf7 = sshps.7, HPSbaf9 = sshps.9)
chs = list(CHSbaf3 = sschs.3, CHSbaf5 = sschs.5, CHSbaf7 = sschs.7, CHSbaf9 = sschs.9)
cmc = list(CMCbaf3 = sscmc.3, CMCbaf5 = sscmc.5, CMCbaf7 = sscmc.7, CMCbaf9 = sscmc.9)
cps = list(CPSrad5 = sscps.5, CPSrad6 = sscps.6, CPSrad7 = sscps.7, CPSrad9 = sscps.9)
hfs = list(hps = hps, chs = chs, cmc = cmc, cps = cps)
#
#  Finally, we create the Smith plot using...
#
res = smithPlot(hfs)
names(res)  #show the result object names
} %end \dontrun
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
