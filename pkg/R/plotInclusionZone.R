#---------------------------------------------------------------------------
#
#   Methods for generic plot() for class for InclusionZone and subclasses;
#
#   All y="missing" signature, x is...
#   1. "InclusionZone"
#   2. "StandUpIZ"
#   3. "chainSawIZ"
#   4. "sausageIZ"
#   5. "pointRelascopeIZ"
#   6. "perpendicularDistanceIZ"
#   7. "distanceLimitedPDSIZ'
#   8. "distanceLimitedMCIZ'
#
#   9. "downLogIZs"
#
#   Note that the plot routines for methods like standup, sausage, PRS, etc.
#   are all very similar, they could probably be collapsed sometime with a
#   "base" function taking away much of the duplication. 18-Jan-2011
#
#   Note in the plotting of Spatial objects, the xlab and ylab arguments
#   do not seem to be passed through to plot, so no labels will be shown
#   when these arguments are specified. To get labels, use title(xlab=,
#   ylab=).
#
#Author...									Date: 23-Aug-2010
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
#  1. method for data frames and class InclusionZone...
#
setMethod('plot',
          signature(x = 'InclusionZone', y='missing'),
function(x, 
         axes = FALSE,        
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#   this just sets up the limits of the plot w/r to the bbox if required
#------------------------------------------------------------------------------
#
    object = x

    bbox = object@bbox
    suppressWarnings(                                     #for object-specific parameters not in par() ...
    plot(SpatialPoints(t(bbox)), col=NA, axes=axes, asp=asp, ...)  #set up limits
                    )

    return(invisible())

}    #plot for 'InclusionZone'
) #setMethod
     






#================================================================================
#  2. method for standUpIZ subclass...
#
setMethod('plot',
          signature(x = 'standUpIZ', y='missing'),
function(x, 
         axes = FALSE,           #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#  plots the standUpIZ object...
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@circularPlot, izColor=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog)
        plot(object@downLog, add=TRUE, ...)
    })
                     
    return(invisible())
}   #plot for 'standUpIZ'
)   #setMethod







#================================================================================
#  3. method for chainSawIZ subclass...
#
setMethod('plot',
          signature(x = 'chainSawIZ', y='missing'),
function(x, 
         axes = FALSE,           #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         showSliver = TRUE,
         ltySliver = 'dashed',
         sliverBorder = 'black',
         sliverColor = transparentColorBase('coral', .StemEnv$alphaTrans),
         showBolt = TRUE,
         ltyBolt = 'dotted',
         boltBorder =  transparentColorBase('grey20', .StemEnv$alphaTrans),
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#  plots the chainSawIZ object...
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@circularPlot, izColor=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog) 
        plot(object@downLog, add=TRUE, ...)
      if(showSliver)
        plot(object@sliver, lty=ltySliver, border=sliverBorder, col=sliverColor, add=TRUE)
      if(showBolt)
        plot(object@bolt$spBolt, lty=ltyBolt, border=boltBorder, add=TRUE)
      
    })
                     
    return(invisible())
}   #plot for 'chainSawIZ'
)   #setMethod





#================================================================================
#  4. method for sausageIZ subclass...
#
setMethod('plot',
          signature(x = 'sausageIZ', y='missing'),
function(x, 
         axes = FALSE,                     #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#   plots the sausageIZ object...
#
#   note: the plot center and log center coincide with sausage, if you want to
#         show one of them, show the log center
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@perimeter, col=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog)
        plot(object@downLog, add=TRUE, ...)
    })
                     
    return(invisible())
}   #plot for 'sausageIZ'
)   #setMethod






#================================================================================
#  5. method for pointRelascopeIZ subclass (18-Jan-2011)...
#
setMethod('plot',
          signature(x = 'pointRelascopeIZ', y='missing'),
function(x, 
         axes = FALSE,                     #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         showDualCenters = FALSE,               #show centers of dual circles
         dcColor = .StemEnv$izBorderColor,      #color for dual circle centers
         ...
        )
{
#------------------------------------------------------------------------------
#   plots the pointRelascopeIZ object...
#
#   note: the IZ center and log center coincide with PRS, if you want to
#         show one of them, show the log center
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@perimeter, col=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog)
        plot(object@downLog, add=TRUE, ...)
    })

    if(showDualCenters)
      plot(SpatialPoints(object@dualCenters), add=TRUE, col=dcColor, ...)
                     
    return(invisible())
}   #plot for 'pointRelascopeIZ'
)   #setMethod





#================================================================================
#  6. method for perpendicularDistnaceIZ subclass (18-Jan-2011)...
#
setMethod('plot',
          signature(x = 'perpendicularDistanceIZ', y='missing'),
function(x, 
         axes = FALSE,                     #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#   plots the perpendicularDistanceIZ object...
#
#   note: the IZ center and log center coincide with PDS, if you want to
#         show one of them, show the log center
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@perimeter, col=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog)
        plot(object@downLog, add=TRUE, ...)
    })
                     
    return(invisible())
}   #plot for 'perpendicularDistanceIZ'
)   #setMethod




#================================================================================
#  7. method for distanceLimitedPDSIZ subclass (10-Mar-2011)...
#
setMethod('plot',
          signature(x = 'distanceLimitedPDSIZ', y='missing'),
function(x, 
         axes = FALSE,                     #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         showFullPDSIZ = FALSE,
         showDLSPart = FALSE,
         showPDSPart = FALSE,
         ...
        )
{
#------------------------------------------------------------------------------
#   plots the distanceLimitedPDSIZ object...
#
#   note: the IZ center and log center coincide with PDS, if you want to
#         show one of them, show the log center
#------------------------------------------------------------------------------
#
    object = x

#
#   set up extents and plot the everything PDS...
#
    if(!add) 
      if(showFullPDSIZ && !is.null(object@pdsFull))    #should never be NULL, but just in case
        object@bbox = bbox(object@pdsFull)             #need more room for full inclusion zone to fit
    
    callNextMethod(object, axes=axes, asp=asp, showLog=showLog,  #will show the log if desired
                   izColor=izColor, izBorder=izBorder,
                   add=add, ...)                                #if add=T: don't re-adjust extents

#
#   the following will just plot the perimeters for clarity...
#
    suppressWarnings({                                 #for object-specific parameters not in par() ...
      if(showPDSPart && !is.null(object@pdsPart))
        plot(perimeter(object@pdsPart), col=NULL, border=izBorder, axes=axes, add=TRUE, ...)
      if(showDLSPart && !is.null(object@dlsPart))
        plot(perimeter(object@dlsPart), col=NULL, border=izBorder, axes=axes, add=TRUE, ...)
      if(showFullPDSIZ && !is.null(object@pdsFull))
        plot(perimeter(object@pdsFull), col=NULL, border=izBorder, axes=axes, add=TRUE, ...)
     # if(showLog && !add)                              #only if not already shown above
      #  plot(object@downLog, add=TRUE, ...)
    })
                     
    return(invisible())
}   #plot for 'distanceLimitedPDSIZ'
)   #setMethod





#================================================================================
#  8. method for distanceLimitedMCIZ subclass (22-Mar-2011)...
#
setMethod('plot',
          signature(x = 'distanceLimitedMCIZ', y='missing'),
function(x, 
         axes = FALSE,                     #not a par() so can't be passed to callNextMethod, so separate it
         showLog = TRUE,
         izColor = .StemEnv$izColor,
         izBorder = .StemEnv$izBorderColor,
         add = FALSE,                           #add each IZ to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#   plots the distanceLimitedMCIZ object...
#
#   note: the IZ center and log center coincide with VPMC, if you want to
#         show one of them, show the log center
#------------------------------------------------------------------------------
#
    object = x

    if(!add)
      callNextMethod(object, axes=axes, asp=asp, ...)            #setup extents

    suppressWarnings({                                #for object-specific parameters not in par() ...
      plot(object@perimeter, col=izColor, border=izBorder, axes=axes, add=TRUE, ...)
      if(showLog)
        plot(object@downLog, add=TRUE, ...)
    })
                     
    return(invisible())
}   #plot for 'distanceLimitedMCIZ'
)   #setMethod















#================================================================================
#================================================================================
#  9. method for a "downLogIZs" collection/population...
#
setMethod('plot',
          signature(x = 'downLogIZs', y='missing'),
function(x, 
         axes = FALSE,        #not a par() so can't be passed to callNextMethod, so separate it
         add = FALSE,          #add each log to overall plot if TRUE
         asp = 1,
         ...
        )
{
#------------------------------------------------------------------------------
#  plots the downed logs...
#------------------------------------------------------------------------------
#
    object = x
    numIZs = length(object@iZones)
    #validObject(object)  #just make sure!

#
#   set up the plot extents via the bounding box, then plot each inclusion zone...
#
    suppressWarnings({                                #for object-specific parameters not in par() ...
    bbox = object@bbox
    if(!add)
      plot(SpatialPoints(t(bbox)), col=NA, axes=axes, asp=asp, ...)  #set up limits

    for(i in seq_len(numIZs))  #use one of the individual iz methods for plotting...
      plot(object@iZones[[i]], axes=axes, add=TRUE, ...)
    
    })
        
    return(invisible())
}   #plot for 'downLogIZs'
)   #setMethod
