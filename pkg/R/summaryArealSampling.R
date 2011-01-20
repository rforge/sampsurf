#---------------------------------------------------------------------------
#
#   Methods for generic summary() for ArealSampling class...
#     (1) circularPlot class
#
#Author...									Date: 20-Aug-2010
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
#  method for virtual class ArealSampling...
#
setMethod('summary',
          signature(object = 'ArealSampling'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple summary of common items from virtual class...
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    if(!is.na(object@description))
      cat(object@description, fill=60)
    .StemEnv$underLine(60, prologue='')

    cat('\nArealSampling...')
    cat('\n  units of measurement: ', object@units)

    cat('\n')
    
    return(invisible())
}   #summary for 'ArealSampling'
) #setMethod




#================================================================================
# method for class "circularPlot"...
#
setMethod('summary',
          signature(object = 'circularPlot'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'circularPlot'...
#------------------------------------------------------------------------------
    callNextMethod()
    cat('    (Above coordinates are for plot center)\n')

    cat('\ncircularPlot...')
    if(object@units == .StemEnv$msrUnits$metric) {
      cat('\n  radius = ', object@radius, ' meters',sep='')
      cat('\n  area = ', object@area, ' square meters', sep='')
      cat(' (', format(object@area/.StemEnv$smpHectare, digits=4), ' hectares)')
    }
    else {
      cat('\n  radius = ', object@radius, ' feet',sep='')
      cat('\n  area = ', object@area, ' square feet', sep='')
      cat(' (', format(object@area/.StemEnv$sfpAcre, digits=4), ' acres)', sep='')
    }
    cat('\n  spatial units: ', object@spUnits@projargs)
    cat('\n  spatial ID:', object@spID)
    cat('\n  location...')
    cat('\n    x coord: ', coordinates(object@location)[,'x'])
    cat('\n    y coord: ', coordinates(object@location)[,'y'])

#
#   important check to see if any valid SpatialPolygon exists for the object...
#
    if(length(object@perimeter@polygons) != 0)  #check for object made with new()
      cat('\n  Number of perimeter points:',
          dim(object@perimeter@polygons$pgsCircPlot@Polygons$circPlot@coords)[1], #yeck
          '(closed polygon)')
    else
      cat('\n\n***No perimeter "SpatialPolygons" -- please use circularPlot constructor!\n')

    cat('\n')
        
    return(invisible())
}   #summary for 'circularPlot'
) #setMethod








#================================================================================
# method for class "pointRelascope"...
#
setMethod('summary',
          signature(object = 'pointRelascope'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'ArealSampling' method for 'pointRelascope'...
#------------------------------------------------------------------------------
    callNextMethod()

    if(object@units == .StemEnv$msrUnits$metric) 
      slUnits = 'square meters per hectare'
    else
      slUnits = 'square feet per acre'
    
    cat('\npointRelascope...')
    cat('\n  Angle (nu) in degrees =', object@angleDegrees)
    cat('\n  Angle (nu) in radians =', object@angleRadians)
    cat('\n  PRS area factor (phi) =', object@phi)
    cat('\n  PRS squared-length factor (L) =', object@slFactor, slUnits)
    cat('\n  This angle has a ',format(object@rwFactor,digits=3),':1 reach:width factor',sep='')

    cat('\n')
        
    return(invisible())
}   #summary for 'pointRelascope'
) #setMethod

    
