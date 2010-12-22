#---------------------------------------------------------------------------
#
#   This extends the bbox methods found in both the sp and raster packages
#   to the sampSurf package class objects...
#
#   The methods for classes include...
#     1. IncluzionZone & downLogIZs container
#     2. downLog
#     3. circularPlot
#     4. Tract
#     5. sampSurf
#
#Author...									Date: 17-Sept-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------

          
#================================================================================
#  method for 'InclusionZone' classes...
#
setMethod('bbox',
          signature(obj = 'InclusionZone'),
function(obj)
{
    return(obj@bbox)
}   #bbox for 'InclusionZone' only
)   #setMethod


#================================================================================
#  method for 'downLogIZs' classes...
#
setMethod('bbox',
          signature(obj = 'downLogIZs'),
function(obj)
{
    return(obj@bbox)
}   #bbox for 'InclusionZone' only
)   #setMethod

          
#================================================================================
#  method for 'InclusionZoneGrid' classes...
#
setMethod('bbox',
          signature(obj = 'InclusionZoneGrid'),
function(obj)
{
    return(obj@bbox)
}   #bbox for 'InclusionZoneGrid' only
)   #setMethod



#================================================================================
#  method for 'downLog' class...
#
setMethod('bbox',
          signature(obj = 'downLog'),
function(obj)
{
    return(bbox(obj@spLog))
}   #bbox for 'downLog' only
)   #setMethod


#================================================================================
#  method for 'downLogs' class...
#
setMethod('bbox',
          signature(obj = 'downLogs'),
function(obj)
{
    return(obj@bbox)
}   #bbox for 'downLogs' only
)   #setMethod
   

#================================================================================
#  method for 'circularPlot' class...
#
setMethod('bbox',
          signature(obj = 'circularPlot'),
function(obj)
{
    return(bbox(obj@perimeter))
}   #bbox for 'circularPlot' only
)   #setMethod
   

#================================================================================
#  method for 'Tract' class and subclasses...
#
setMethod('bbox',
          signature(obj = 'Tract'),
function(obj)
{
    bb = callNextMethod(obj)
    rownames(bb) = c('x','y')

    return(bb)
}   #bbox for 'Tract' and subclasses
)   #setMethod
    

#================================================================================
#  method for 'sampSurf' class and subclasses...
#
setMethod('bbox',
          signature(obj = 'sampSurf'),
function(obj)
{
    bb = bbox(obj@tract)   #should call 'Tract' method

    return(bb)
}   #bbox for 'Tract' and subclasses
)   #setMethod
   


