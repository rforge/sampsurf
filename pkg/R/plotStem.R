#---------------------------------------------------------------------------
#
#   Methods for generic plot() for class...
#     (1) Stem and subclasses; this includes the downLogs (plural)
#         container class
#
#   Note in the plotting of Spatial objects, the xlab and ylab arguments
#   do not seem to be passed through to plot, so no labels will be shown
#   when these arguments are specified. To get labels, use title(xlab=,
#   ylab=).
#
#Author...									Date: 9-Aug-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#



#================================================================================
#  method for data frames and class Stem...
#
setMethod('plot',
          signature(x = 'Stem', y='missing'),
function(x,
         pchStemLocation = 20,      #3 is also good
         stemLocationColor = .StemEnv$logAttributeColor,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple plot of the x,y location from the virtual class...
#
#   since there will never be an object of class 'Stem', we can safely assume
#   that some components have already been plotted, and we can just
#   add to it with points()
#
#------------------------------------------------------------------------------
#
    object = x

    suppressWarnings(                                       #**note, for 'add' parameter passed in ...
      points(object@location, col=stemLocationColor,
             pch=pchStemLocation, ...)  #don't use plot() for this!
                    )
        
    return(invisible())

}    #plot for 'downLog'
) #setMethod






#================================================================================
#  method for downLog subclass...
#
setMethod('plot',
          signature(x = 'downLog', y='missing'),
function(x,
         axes = FALSE,  #not a par() so can't be passed to callNextMethod, so separate it
         logColor = .StemEnv$logColor,
         showLogCenter = FALSE,
         pchLogCenter = 3,       #20 is also good
         logCenterColor = .StemEnv$logAttributeColor,
         showNeedle = FALSE,
         logNeedleColor = .StemEnv$logAttributeColor,
         logBorderColor = .StemEnv$logBorderColor,   #log perimeter color
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#  plots the downed log...
#------------------------------------------------------------------------------
#
    object = x
    suppressWarnings({                              #for non-plot arguments in ...    
    plot(object@spLog, col=logColor, axes=axes, border=logBorderColor, asp=asp, ...)

    if(showNeedle)
      plot(object@slNeedleAxis, col=logNeedleColor,  #also a Spatial object
           add=TRUE)                 
    })   

# 
#   call next method subsequent, adding to the existing plot()...
#
    if(showLogCenter)
      callNextMethod(x=object, pchStemLocation = pchLogCenter,
                     stemLocationColor = logCenterColor, ...)  

        
    return(invisible())
}   #plot for 'downLog'
)   #setMethod














#================================================================================
#================================================================================
#  method for a "downLogs" collection/population...
#
setMethod('plot',
          signature(x = 'downLogs', y='missing'),
function(x, 
         axes = FALSE,        #not a par() so can't be passed to callNextMethod, so separate it
         add = FALSE,          #no existing plot assumed
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#  plots all the downed logs in the collection...
#------------------------------------------------------------------------------
#
    object = x
    numLogs = length(object@logs)
    validObject(object)  #just make sure!

#
#   set up the plot extents via the bounding box, then plot each log...
#
    if(!add) {
      bbox = object@bbox
      plot(SpatialPoints(t(bbox)), col=NA, axes=axes, asp=asp)  #set up limits
    }

    suppressWarnings({                              #for non-plot arguments in ...    
    for(i in seq_len(numLogs)) 
      plot(object@logs[[i]], axes=axes, add = TRUE, ...)
    })   
        
    return(invisible())
}   #plot for 'downLogs'
)   #setMethod
    






    


#*******************
#showMethods('plot')
#*******************
