smithPlot = function(hfs,
                     showPlot = TRUE,
                     ylab = 'Total surface variance',
                     xlab = 'Average Inclusion Zone Area',
                     type = 'b',
                     pch = 19,
                     theme = c('ggplot','custom','plain','economist'),
                     cols = c("#00526D", "#00A3DB", "#7A2713", "#939598"),
                     ...
                    )
{
#---------------------------------------------------------------------------
#
#   This method will produce an H. F. Smith-type plot for objects of class 
#   sampSurf...
#
#   H. F. Smith. An empirical law describing heterogeneity in the yields 
#                of agricultural crops. Journal of Agricultural Science, 
#                28:1-23, 1938.
#
#   This code grew out of the manuscript entitled: "Some Refinements on the 
#   Comparison of Areal Sampling Methods via Simulation" currently submitted
#   to Forests. Though this routine was not used to make the figure for that
#   manuscript, the default is set up to reproduce it as long as hfs is 
#   a list object with the proper format and those sampSurf objects from the
#   paper.
#
#   Arguments...
#     hfs = a named list of named lists with the following design...
#           top = sampling methods; these should be named appropriately, e.g.,
#                 'hps', 'chs', etc.
#           sub = within each sampling method, a list of sampSurf objects,
#                 one for each different plot size/baf, etc.; e.g., "HPSbaf5",
#                 "CHSbaf7", etc.
#     showPlot = TRUE: display the plot; FALSE: just return a copy
#     ylab, xlab, type, pch, are all either graphics parameters
#       or lattice options
#     theme = see the themes in latticeExtra
#     cols = a vector of alternative colors for the line graphs; NULL will 
#            use the default theme colors; those listed above are essentially
#            the economist colors
#     ... = passed on to xyplot
#
#   Returns...
#     A list invisibly with...
#       -- the data frame used to make the plot
#       -- the lattice plot object
#
#   Note that the average inclusion zone areas for all trees or logs in each
#   sampSurf run is used for the x-axis. This, of course, is constant for most
#   fixed-area plot methods (though see the so-called "sausage" method for
#   down CWD as one exception), but varies for variable radius plot methods,
#   hence the use of the average area.
#
#   A conceptual example setup for the hfs argument as used in the Comparing
#   Methods manuscript would be (btw, what follows is from sm.ids below when
#   all the simulations for the manuscript run are included)...
#
#     $hps
#     [1] "HPSbaf3" "HPSbaf5" "HPSbaf7" "HPSbaf9"
#     $chs
#     [1] "CHSbaf3" "CHSbaf5" "CHSbaf7" "CHSbaf9"
#     $cmc
#     [1] "CMCbaf3" "CMCbaf5" "CMCbaf7" "CMCbaf9"
#     $cps
#     [1] "CPSrad5" "CPSrad6" "CPSrad7" "CPSrad9"
#
#   where the "$" names are the "top" list entries, and then each with its own
#   corresponding named "sub" list for that sampling method with the different
#   sampSurf objects corresponding to the row names in the respective sublist.
#   Thus there are four sampling methods (top names = 'hps','chs','cmc','cps'),
#   each with four sets of sampSurf simulations named as shown, for a total
#   of 16 sampSurf objects in the entire list.
#
#   In general, hfs can be ragged; that is, the sublists do not need to be
#   all the same length. Therefore some sampling methods may have fewer
#   simulation results than others.
#
#Author...									Date: 23-Aug-2017
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#
#   make sure all objects in the list are sampSurf objects...
#
    for(sm in hfs) {
      if(any(!sapply(sm, is, 'sampSurf')))
        stop('All objects passed in "hfs" must be of class "sampSurf" or subclass!')
    }
 

#
#   just get a few things that might be handy...
#
    sm.names = names(hfs)                     #vector of sampling method names
    sm.ids = lapply(hfs, names)               #a list of names corresponding to hfs
    n.sm = length(hfs)                        #number of sampling methods; e.g., hps, cps
    n.sims = sapply(hfs, length)              #number of sims by sampling method
    nn = sum(n.sims)
    if(length(unique(sm.names)) != n.sm)
      stop('The sampling method names must be unique!')
    if(sum(unlist(lapply(lapply(sm.ids, unique), length))) != nn)
      stop('The ids by sampling method must be unique!') #note this only checks w/in a sm
    df.colnames = c('sampMeth', 'id', 'mean', 'var', 'sd', 'izArea')
    
#
#   this will build the individual rows of the data frame for a given sampling method;
#   i.e., it corresponds to extracting the required information from a single sampSurf
#   object...
#
    ssGetStats = function(ss) {
               stats = ss@surfStats
               mean = stats$mean
               var = stats$var
               stDev = stats$stDev
               izArea = mean(sapply(ss@izContainer@iZones, area))     #average IZ area
               z = c(NA_real_, NA_real_, mean, var, stDev, izArea)
               names(z) = df.colnames
               return(z)
              } #ssGetStats
#
#   go through each sampling method and build the data frame...
#
    for(i in seq_len(n.sm)) {
      sm = hfs[[i]]
      z = as.data.frame(t(sapply(sm, ssGetStats)))
      z[, 'sampMeth'] = rep(sm.names[i], n.sims[i])
      z[, 'id'] = sm.ids[[i]]
      if(i == 1)
        df = z
      else
        df = rbind(df, z)
    }
    colnames(df) = df.colnames

#
#   set the latticeExtras theme as desired...
#
    #require(latticeExtra, quietly=TRUE)
    theme = match.arg(theme)
    lattice.options = lattice.options()
    switch(theme,
           plain = {par.settings = trellis.par.get()},
           custom = {par.settings = custom.theme.2()},
           ggplot = {par.settings = ggplot2like()
                     lattice.options = ggplot2like.opts()
                    },
           economist = {par.settings = theEconomist.theme()
                        lattice.options = theEconomist.opts()
                       }
          )

#
#   this is silly, but in order to stop R CMD check from reporting this as a Note... 
#   "smithPlot: no visible binding for global variable 'sampMeth'"
#   we must add the following line of code (evidently, the check does not look
#   for variable bindings in the data=df argument in xyplot below)...
#
    sampMeth = NULL  
 
#
#   create the lattice/trellis plot object...
#
    plt = xyplot(var ~ izArea, data = df, groups = sampMeth, 
                 type = type, pch = pch,
                 ylab = ylab,
                 xlab = xlab,
                 auto.key = list(x = .8, y = .9, corner = c(0, 0)),
                 par.settings = par.settings,
                 lattice.options = lattice.options,
                 ...
               )
    #substitute other colors if desired...
    if(!is.null(cols)) {
      plt$par.settings$superpose.symbol$col = cols
      plt$par.settings$superpose.symbol$fill = cols
      plt$par.settings$superpose.line$col = cols
    }    
                
    if(showPlot)                
      plot(plt)     
    
    return(invisible(list(df = df,
                          plt = plt
                         )
                    )
          )   
}   #smithPlot
    

