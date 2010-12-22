#---------------------------------------------------------------------------
#
#   This file holds the S4 class definitions for the Areal Sampling method
#   related classes.
#
#   Classes...
#     1. ArealSampling: virtual class for all methods
#     2. circularPlot: class for fixed-radius circular plot sampling
#
#Author...									Date: 19-Aug-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#




#=================================================================================================
#
#  define the virtual ArealSampling class...
#
setClass('ArealSampling',
         
#
#  slots for the class and its subclasses...
#
    representation(description = 'character',      #more descriptive name
                   units = 'character',            #English or metric units
                   location = 'SpatialPoints',     #object "central" location
                   spID = 'character',             #short id name for polygon label
                   spUnits = 'CRS'                 #sp units, character will change
                  ),
    prototype = list(description = '',                    #some defaults for validity checking
                     units = 'metric',
                     location = SpatialPoints(matrix(c(0,0), nrow=1, dimnames=list('1',c('x','y'))) ),
                     spID = paste('cp', format(runif(1, 0,10000),digits=8), sep=':'),
                     spUnits = CRS(projargs=as.character(NA)) 
                    ),
    contains = 'VIRTUAL',
    validity = function(object) {
                 if(!(object@units %in% c('English','metric')))
                   return('units of measure must be "English" or "metric"')
                 locNames = match(colnames(object@location), c('x','y'))
                 if(any(is.na(locNames)))
                   return('location names must be "x" and "y"!')

                 return(TRUE)
               } #validity check
) #class ArealSampling
                   



#=================================================================================================
#
#  the circular plot class is just a direct descendant of 'ArealSampling'...
#
#    location = the center of the circle
#
setClass('circularPlot',
    representation(radius = 'numeric',              #plot radius
                   area = 'numeric',                #plot area
                   perimeter = 'SpatialPolygons'    #sp polygon of the plot perimeter
                  ),
    prototype = list(radius = -1,                    #
                     area = 0
                    ),
    contains = 'ArealSampling',                     #a subclass of the virtual 'ArealSampling' class
    validity = function(object) {
                 if(object@radius <= 0)
                   return('plot radius must be positive!')
                 if(object@area <= 0)
                   return('plot area must be positive!')
      
                 if(!is.na(object@spUnits@projargs) && object@spUnits@projargs == '+proj=longlat')
                   return(paste('spUnits must be commensurate with units,',
                                'please convert to non-geographic coordinate system!')
                         )


                 
                 return(TRUE)
               } #validity check
) #class circularPlot
         

#
# initialize is called after the prototype values are set, so we can use them to
# set flags for default initialization here, before validity checking...
#
setMethod('initialize', 'circularPlot',
  function(.Object, ...) {

    if(.Object@radius < 0)
      .Object@radius = runif(1, 1, 20)  #meters
    .Object@area = pi * .Object@radius * .Object@radius

               
    callNextMethod(.Object, ...)
 } #function
) #setMethod initialize
