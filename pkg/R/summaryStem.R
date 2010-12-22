#---------------------------------------------------------------------------
#
#   Methods for generic summary() for class...
#     (1) Stem and subclasses; this includes the downLogs (plural)
#         container class
#
#Author...									Date: 6-Aug-2010
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
setMethod('summary',
          signature(object = 'Stem'),
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

    cat('\nStem...')
    cat('\n  Species: ', object@species)
    cat('\n  units of measurement: ', object@units)
    cat('\n  spatial units: ', object@spUnits@projargs)
    cat('\n  location...')
    cat('\n    x coord: ', coordinates(object@location)[,'x'])
    cat('\n    y coord: ', coordinates(object@location)[,'y'])

    cat('\n')
    
    return(invisible())
}   #summary for 'Stem'
) #setMethod




#================================================================================
#  method for data frames and class "downLog"...
#
setMethod('summary',
          signature(object = 'downLog'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   add a little to 'Stem' method for 'downLog'...
#------------------------------------------------------------------------------
    callNextMethod()
    cat('    (Above coordinates are for log center)')
    cat('\n  Spatial ID:', object@spLog@polygons$pgsLog@ID)

    cat('\n\ndownLog...')
    if(object@units == .StemEnv$msrUnits$metric) {
      cat('\n  Butt diameter = ', object@buttDiam, ' meters (', object@buttDiam*.StemEnv$m2cm, ' cm)',sep='')
      cat('\n  Top diameter = ', object@topDiam, ' meters (', object@topDiam*.StemEnv$m2cm, ' cm)', sep='')
      cat('\n  Log length = ', object@logLen, 'meters')
      cat('\n  Log volume = ', object@logVol, 'cubic meters')
    }
    else {
      cat('\n  Butt diameter = ', object@buttDiam, ' feet (', object@buttDiam*.StemEnv$ft2in, ' in)',sep='')
      cat('\n  Top diameter = ', object@topDiam, ' feet (', object@topDiam*.StemEnv$ft2in, ' in)', sep='')
      cat('\n  Log length = ', object@logLen, 'feet')
      cat('\n  Log volume = ', object@logVol, 'cubic feet')
    }
    cat('\n  Log angle of lie = ', object@logAngle, 'radians')
    cat(' (', .StemEnv$rad2Deg(object@logAngle), ' degrees)', sep='')
    #cat('\n  Log angle of lie = ', .StemEnv$rad2Deg(object@logAngle), 'degrees')
    cat('\n  Taper parameter = ', ifelse(is.null(object@solidType), 'NULL', object@solidType) )
    
    cat('\n\nTaper (in part)...\n')
    print(head(object@taper))

    if(!is.null(object@userExtra))
      cat('\n  "Note: userExtra" slot is non-NULL')
    
#
#   important check to see if any valid SpatialPolygon exists for the object...
#
    if(length(object@spLog@polygons) == 0)  #check for object made with new()
      cat('\n\n***No spLog "SpatialPolygons" -- please use downLog constructor!\n')

    cat('\n')
        
    return(invisible())
}   #summary for 'downLog'
) #setMethod








#================================================================================
#  method for data frames and class "downLogs" (plural!)...
#
setMethod('summary',
          signature(object = 'downLogs'),
function(object,
         ...
        )
{
#------------------------------------------------------------------------------
#   just a simple summary of items in the "downLogs" object...
#------------------------------------------------------------------------------
    cat('\nObject of class:', class(object))
    .StemEnv$underLine(60)
    cat('Container class object...')
    numLogs = length(object@logs)
    cat('\n  There are',numLogs,'logs in the population')
    cat('\n  Units of measurement: ', object@units)

    totVol = object@stats['total','volume']
    avgVol = object@stats['mean','volume']
    avgLen = object@stats['mean','length']
    if(object@logs[[1]]@units == .StemEnv$msrUnits$metric) {
      cat('\n  Population log volume = ', totVol, 'cubic meters')
      cat('\n  Average volume/log= ', avgVol, 'cubic meters')
      cat('\n  Average length/log= ', avgLen, 'meters')
    }
    else {
      cat('\n  Population log volume = ', totVol, 'cubic feet')
      cat('\n  Average volume/log= ', avgVol, 'cubic feet')
      cat('\n  Average length/log= ', avgLen, 'feet')
    }
    

    cat('\n\n  Encapulating bounding box...\n')
    print(object@bbox)

    cat('\n')
    return(invisible())
}   #summary for 'downLogs'
) #setMethod
    



#showMethods('summary')
