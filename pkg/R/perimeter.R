#---------------------------------------------------------------------------
#
#   These methods are used to get the perimeter spatial object from a given
#   InclusionZone object. It could also be used to get a perimeter from a
#   downLog object, etc. but there's really not much need for the latter
#   as that is simple to retrieve...
#
#   The methods include (in no particular order)...
#     1. standUpIZ
#     2. chainSawIZ: either the circular plot location IZ or the sausage IZ
#                    are possible returns
#     3. sausageIZ
#     4. downLog
#     5. downLogs
#     6. circularPlot of class ArealSampling
#     7. Tract
#     8. sampSurf
#     9. downLogIZs
#
#Author...									Date: 21-Sept-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#   generic definition...
#
if(!isGeneric('perimeter')) {
  setGeneric('perimeter',  
             function(object, ...) standardGeneric('perimeter'),
             signature = c('object')
            )
}





#================================================================================
#  method for standUpIZ object...
#
setMethod('perimeter',
          signature(object = 'standUpIZ'),
function(object, ...)
{
    return(object@circularPlot@perimeter)
}   #standUpIZ
) #setMethod






#================================================================================
#  method for chainSawIZ object...
#
setMethod('perimeter',
          signature(object = 'chainSawIZ'),
function(object,
         whatSense = c('point', 'plot', 'sausage'),
         ...)
{
    whatSense = match.arg(whatSense)
    if(whatSense == 'point')                    #return the plot centerpoint
      return(object@circularPlot@location)
    else if(whatSense == 'plot')                #return the plot
      return(perimeter(object@circularPlot))
    else {                                      #full sausage inclusion zone
      sa = sausageIZ(object@downLog, object@circularPlot@radius,
                     spUnits = object@circularPlot@spUnits, ...)
      return(sa@perimeter)
    }
      
}   #chainSawIZ
) #setMethod






#================================================================================
#  method for sausageIZ object...
#
setMethod('perimeter',
          signature(object = 'sausageIZ'),
function(object, ...)
{
    return(object@perimeter)
}   #sausage
) #setMethod



#================================================================================
#  method for pointRelascopeIZ object...
#
setMethod('perimeter',
          signature(object = 'pointRelascopeIZ'),
function(object, ...)
{
    return(object@perimeter)
}   #pointRelascope
) #setMethod



#================================================================================
#  method for downLog object...
#
setMethod('perimeter',
          signature(object = 'downLog'),
function(object, ...)
{
    return(object@spLog)
}   #downLog
) #setMethod




#================================================================================
#  method for downLogs object... (same code as Tract--should make this a function: bboxToPoly) ***
#
setMethod('perimeter',
          signature(object = 'downLogs'),
function(object, ...)
{
    return(bboxToPoly(object))
}   #downLogs
) #setMethod




#================================================================================
#  method for circularPlot object...
#
setMethod('perimeter',
          signature(object = 'circularPlot'),
function(object, ...)
{
    return(object@perimeter)
}   #circularPlot
) #setMethod



#================================================================================
#  method for Tract object...
#
setMethod('perimeter',
          signature(object = 'Tract'),
function(object, ...)
{
    return(bboxToPoly(object))
}   #Tract
) #setMethod



#================================================================================
#  method for sampSurf object...
#
setMethod('perimeter',
          signature(object = 'sampSurf'),
function(object, ...)
{
    perimeter = perimeter(object@tract)  #a 'Tract' object
  
    return(perimeter)
}   #sampSurf
) #setMethod



#================================================================================
#  method for downLogIZs object...
#
setMethod('perimeter',
          signature(object = 'downLogIZs'),
function(object, ...)
{
    return(bboxToPoly(object))
}   #downLogIZs
) #setMethod
