#---------------------------------------------------------------------------
#
#   Methods for generic plot3D() rgl driver for class for "sampSurf" &
#   "Tract" classes.
#
#   "InclusionZoneGrid" class added 18-Apr-2011
#
#   Note: the only reason to have a method below for "Tract" is so that
#         the blue.colors() are used by default rather than terrain.colors(),
#         which it would default to using the "RasterLayer" method, if an
#         object of class "Tract" of subclass is passed directly to plot3D.
#
#Author...									Date: 18-Oct-2010
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
#  method for class 'sampSurf'...
#
setMethod('plot3D',
          signature(x = 'sampSurf'),
function(x,
         col = .StemEnv$blue.colors, #pass the actual function here, not a call!
         ...
        )
{
#------------------------------------------------------------------------------
#
    suppressWarnings({                              #for non-plot arguments in ...    
      plot3D(x@tract, col=col, ...)
    })   

    return(invisible())

}    #plot3D for 'sampSurf'
) #setMethod



#================================================================================
#  method for class 'Tract'...
#
setMethod('plot3D',
          signature(x = 'Tract'),
function(x,
         col = .StemEnv$blue.colors, #pass the actual function here, not a call!
         ...
        )
{
#------------------------------------------------------------------------------
#
    suppressWarnings({                              #for non-plot arguments in ...    
      callNextMethod(x, col=col, ...)
    })   

    return(invisible())

}    #plot3D for 'Tract'
) #setMethod



#================================================================================
#  method for class 'InclusionZoneGrid'...
#
setMethod('plot3D',
          signature(x = 'InclusionZoneGrid'),
function(x,
         estimate = names(.StemEnv$puaEstimates),
         col = .StemEnv$blue.colors, #pass the actual function here, not a call!
         ...
        )
{
#------------------------------------------------------------------------------
#
    estimate = match.arg(estimate)
    x@grid = setValues(x@grid, x@data[,estimate]) 
    suppressWarnings({                              #for non-plot arguments in ...    
      plot3D(x@grid, col=col, ...)                  #no next method
    })   

    return(invisible())

}    #plot3D for 'InclusionZoneGrid'
) #setMethod

