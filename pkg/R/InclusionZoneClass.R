#---------------------------------------------------------------------------
#
#   This file holds the S4 class definitions for the Inclusion Zone method
#   related classes. Inclusion zones are properties of the particular
#   'Stem' subclass, and 'ArealSampling' subclass. Therefore, the VIRTUAL
#   base class is composed of these two slots. Then we subclass this depending
#   on 'Stem' subclass-type, and finally 'ArealSampling' subclass-type.
#
#   Classes...
#     1. InclusionZone: virtual for all 'ArealSampling' methods and Stem
#                       classes
#     2. downLogIZ: virtual for all 'ArealSampling' mehtods and 'downLog'
#                   subclasses
#     3. standUpIZ: the standup method of sampling down cwd
#     4. chainSawIZ; the chain saw method for sampling down cwd
#     5. sausageIZ: the sausage method of sampling down cwd
#     6. downLogIZs: a container class for multiple objects of any subclass
#                  of 'downLogIZ'
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




#=================================================================================================
#
#  1. define the virtual InclusionZone class...
#
setClass('InclusionZone',
         
#
#  slots for the class and its subclasses...
#
    representation(description = 'character',
                   units = 'character',              #English or metric units
                   bbox = 'matrix',                  #overall bounding box
                   spUnits = 'CRS',                  #spatial units
                   puaBlowup = 'numeric',            #per unit area expansion factor
                   puaEstimates = 'list',            #per unit area estimates
                   userExtra = 'ANY'                 #anything else the user wants to include--no checks
                  ),
    prototype = list(description = 'Areal sampling inclusion zone',  #some defaults for validity checking
                     units = .StemEnv$msrUnits$metric,
                     bbox = matrix(rep(0,4), nrow=2, dimnames=list(c('x','y'), c('min','max'))),
                     spUnits = CRS(projargs=as.character(NA)),
                     puaBlowup = 1,                  #does no harm
                     puaEstimates = list(),          #empty list
                     userExtra = NULL
                    ),
    contains = 'VIRTUAL',
    validity = function(object) {
                 if(!(object@units %in% .StemEnv$msrUnits))
                   return('units of measure must be "English" or "metric"')

                 if(length(object@puaBlowup) > 1)
                   return('puaBlowup must be a scalar!')
                 if(object@puaBlowup < 0)
                   return('puaBlowup factor must be non-negative!')

                 if(length(object@puaEstimates > 0)) {
                   estNames = match(names(object@puaEstimates), unlist(.StemEnv$puaEstimates))
                   if(any(is.na(estNames)))
                     return('invalid slots in puaEstimates list object!')
                 }

                 #essentially the same checks as in bboxCheck()...
                 if(!class(object@bbox) == 'matrix')
                   return('bbox slot must be a 2x2 matrix')
                 bboxNames = match(rownames(object@bbox), c('x','y'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox rownames must be "x", "y"!')
                 bboxNames = match(colnames(object@bbox), c('min','max'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox colnames must be "min", "max"!')
                 if(any( apply(object@bbox,1,function(x) if(x['min'] >= x['max']) TRUE else FALSE) ))
                   return('in slot bbox, "min" must be less than "max" for x and y!')

                 if(!is.na(object@spUnits@projargs) && object@spUnits@projargs == '+proj=longlat')
                   return(paste('spUnits must be commensurate with units,',
                                'please convert to non-geographic coordinate system!')
                         )
                   
                 return(TRUE)
               } #validity check
) #class InclusionZone 
                     
                   



#=================================================================================================
#
#  2. the down log inclusion zone class is just a direct descendant of 'InclusionZone'...
#
#
setClass('downLogIZ',
    representation(downLog = 'downLog'              #downLog object
                  ),
    #prototype = list(#downLog = new('downLog'),         #some defaults for validity checking
    #                 units = .StemEnv$msrUnits$metric
    #                ),
    contains = c('InclusionZone', 'VIRTUAL'),             #a subclass of the virtual 'InclusionZone' class
    validity = function(object) {

                 if(!validObject(object@downLog))
                   return('object in downLog slot is an invalid object!')
                 
                 return(TRUE)
               } #validity check
) #class downLogIZ 


 





#=================================================================================================
#
#  3. the standup class is just a direct descendant of 'InclusionZone'...
#
#
setClass('standUpIZ',
    representation(circularPlot = 'circularPlot'              #circularPlot object
                  ),
    prototype = list(#downLog = new('downLog'),         #some defaults for validity checking
                    ),
    contains = 'downLogIZ',                             #a subclass of the virtual 'downLogIZ' class
    validity = function(object) {

                 if(!validObject(object@circularPlot))
                   return('object in circularPlot slot is an invalid object!')

                 if(!identical(object@downLog@units, object@circularPlot@units))
                   return('measurement units do not match between downLog and circularPlot objects!')

                 if(!identical(object@downLog@spUnits, object@circularPlot@spUnits))
                   return('Spatial units do not match between downLog and circularPlot objects!')
                 
                 return(TRUE)
               } #validity check
) #class standUpIZ 




#=================================================================================================
#
#  4. the 'chainSawIZ' class is just a direct descendant of 'InclusionZone'...
#
#
setClass('chainSawIZ',
    representation(circularPlot = 'circularPlot',              #circularPlot object
                   sliver = 'SpatialPolygons',                 #the intersection sliver w/in the log
                   bolt = 'list'                               #the bolt/sliver information
                  ),
    #prototype = list(
    #                ),
    contains = 'downLogIZ',                        #a subclass of virtual 'downLogIZ' class
    validity = function(object) {

                 if(!validObject(object@circularPlot))
                   return('object in circularPlot slot is an invalid object!')

                 if(!identical(object@downLog@units, object@circularPlot@units))
                   return('measurement units do not match between downLog and circularPlot objects!')

                 if(!identical(object@downLog@spUnits, object@circularPlot@spUnits))
                   return('Spatial units do not match between downLog and circularPlot objects!')
                 
                 return(TRUE)
               } #validity check
) #class chainSawIZ 


 





#=================================================================================================
#
#  5. the sausage class is a direct descendant of 'InclusionZone'...
#
#
setClass('sausageIZ',
    representation(sausage = 'matrix',              #holds the sausage in matrix form
                   radius = 'numeric',              #plot radius for sausage
                   area = 'numeric',                #exact area of the inclusion zone
                   perimeter = 'SpatialPolygons',   #sausage perimeter in 'SpatialPolygons' form
                   pgSausageArea = 'numeric'        #polygon sausage area approximation
                  ),
    #prototype = list(#downLog = new('downLog'),         #some defaults for validity checking
    #                ),
    contains = 'downLogIZ',                                  #a subclass of the 'downLogIZ' class
    validity = function(object) {
                 if(object@radius <= 0 || is.na(object@radius))
                   return('plot radius must be positive non-missing!')

                 if(object@area < 0 || is.na(object@area))
                   return('object has negative or missing inlusion zone area!')

                 if(object@pgSausageArea < 0 || is.na(object@pgSausageArea))
                   return('object has negative or missing polygon area!')
                 
                 return(TRUE)
               } #validity check
) #class sausageIZ 









#=================================================================================================
#=================================================================================================
#
#  6. the downLogIZs class (plural) is a container class for an number of "downLogIZ" objects...
#
setClass('downLogIZs',
    representation(iZones = 'list',                    #list of some subclass of "downLogIZ"
                   units = 'character',                #English or metric units
                   bbox = 'matrix',                    #overall bounding box
                   description = 'character'           #hmmm?
                  ),
    prototype = list(logs = list(),                  #empty, zero-length list
                     units = .StemEnv$msrUnits$metric,
                     bbox = matrix(rep(0,4), nrow=2, dimnames=list(c('x','y'), c('min','max'))),
                     description = ''
                    ),
    validity = function(object) {
                 if(!(object@units %in% .StemEnv$msrUnits))
                   return('units of measure must be "English" or "metric"')

                 numIZs = length(object@iZones)
                 if(numIZs < 1)
                   return('no "downLogIZ" objects found in iZones slot!')

                 for(i in seq_len(numIZs))
                   validObject(object@iZones[[i]])

                 for(i in seq_len(numIZs))
                   if(object@units != object@iZones[[i]]@units)
                     return('At least one downLogIZ has the wrong units!')

#                check on bbox matrix format...                 
                 if(!class(object@bbox) == 'matrix')
                   return('bbox slot must be a 2x2 matrix')
                 bboxNames = match(rownames(object@bbox), c('x','y'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox rownames must be "x", "y"!')
                 bboxNames = match(colnames(object@bbox), c('min','max'))
                 if(any(is.na(bboxNames)))
                   return('slot bbox colnames must be "min", "max"!')
                                      
#                consistent units check...
                 units = object@iZones[[1]]@units
                 for(i in seq_len(numIZs))
                   if(object@iZones[[i]]@units != units)
                     return('You can not mix measurement units within a population of inclusion zones!')

#                consistent class check...
                 class = class(object@iZones[[1]])
                 for(i in seq_len(numIZs))
                   if(class(object@iZones[[i]]) != class)
                     return('You can not mix inclusion classes in the population!')
#                more...
                 #if(!extends(class, 'InclusionZone'))  #last test passed for all, so just check first
                 if(!is(object@iZones[[1]], 'downLogIZ'))  #last test passed for all, so just check first
                   return('Classes of objects in iZones must be a subclass of "downLogIZ"!')
                 
                 
                 return(TRUE)
               } #validity check
) #class downLogIZs 
