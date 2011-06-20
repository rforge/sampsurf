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
#     2. downLogIZ: virtual for all 'ArealSampling' methods and 'downLog'
#                   subclasses
#     3. standUpIZ: the standup method of sampling down cwd
#     4. chainSawIZ; the chain saw method for sampling down cwd
#     5. sausageIZ: the sausage method of sampling down cwd
#     6. pointRelascopeIZ: the point relascope method for sampling down cwd
#     7. perpendicularDistanceIZ: normal PDS
#     8. omnibusPDSIZ: for omnibus estimation under normal PDS
#     9. distanceLimitedIZ: for canonical distance limited sampling (dls)
#    10. distanceLimitedMCIZ: DLMC
#    11. distanceLimitedPDSIZ: for canonical DLPDS
#    12. omnibusDLPDSIZ: combines omnibus PDS component with DLMC component
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
                 if(!is.na(object@puaBlowup) && object@puaBlowup < 0)  #it can be NA for DLPDS total
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

                 if(!identical(object@downLog@units, object@units))
                   return('measurement units do not match between downLog and IZ objects!')

                 if(!identical(object@downLog@spUnits, object@spUnits))
                   return('Spatial units do not match between downLog and IZ objects!')
                 
                 return(TRUE)
               } #validity check
) #class downLogIZ 


 





#=================================================================================================
#
#  3. the standup class is just a direct descendant of 'downLogIZ'...
#
#
setClass('standUpIZ',
    representation(circularPlot = 'circularPlot'              #circularPlot object
                  ),
    #prototype = list(#downLog = new('downLog'),         #some defaults for validity checking
    #                ),
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
#  4. the 'chainSawIZ' class is just a direct descendant of 'downLogIZ'...
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
#  5. the sausage class is a direct descendant of 'downLogIZ'...
#
#
setClass('sausageIZ',
    representation(sausage = 'matrix',              #holds the sausage incl zone in matrix form
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
#
#  6. the pointRelascope class is a direct descendant of 'downLogIZ'...
#
#     I'll call the "mastercard" double/dual circle (blob) just "dual"...
#
#     below: hc = homogeneous coordinates
#
#     added: 13-Jan-2011
#
setClass('pointRelascopeIZ',
    representation(prs = 'pointRelascope',          #point relascope sampling object
                   dualCircle = 'matrix',           #holds the blob perimeter in matrix form w/ hc
                   radius = 'numeric',              #radius for each of the half blobs
                   area = 'numeric',                #exact area of the inclusion zone
                   perimeter = 'SpatialPolygons',   #blob perimeter in 'SpatialPolygons' form
                   pgDualArea = 'numeric',          #polygon blob area approximation
                   dualCenters = 'matrix'           #centers of each dual circle, w/o hc
                  ),
    contains = 'downLogIZ',                         #a subclass of the 'downLogIZ' class
    validity = function(object) {
                 if(object@radius <= 0 || is.na(object@radius))
                   return('plot radius must be positive non-missing!')

                 if(object@area < 0 || is.na(object@area))
                   return('object has negative or missing inclusion zone area!')

                 if(object@pgDualArea < 0 || is.na(object@pgDualArea))
                   return('object has negative or missing polygon area!')

                 if(!identical(object@prs@units, object@units))
                   return('object units do not match the pointRelascope object units')
                 
                 return(TRUE)
               } #validity check
) #class pointRelascopeIZ 









#=================================================================================================
#
#  7. the perpendicularDistanceIZ class is a direct descendant of 'downLogIZ'...
#
#
#     below: hc = homogeneous coordinates
#
#     added: 26-Jan-2011
#
setClass('perpendicularDistanceIZ',
    representation(pds = 'perpendicularDistance',   #perpendicular distance sampling object
                   izPerim = 'matrix',              #holds the iz perimeter in matrix form w/ hc
                   area = 'numeric',                #exact area of the inclusion zone
                   perimeter = 'SpatialPolygons',   #izone perimeter in 'SpatialPolygons' form
                   pgArea = 'numeric',              #polygon izone area approximation
                   pdsType = 'character'            #protocol method: see validity below
                  ),
    contains = 'downLogIZ',                         #a subclass of the 'downLogIZ' class
    validity = function(object) {
                 if(object@area < 0 || is.na(object@area))
                   return('object has negative or missing inclusion zone area!')

                 if(object@pgArea < 0 || is.na(object@pgArea))
                   return('object has negative or missing polygon area!')

                 if(!identical(object@pds@units, object@units))
                   return('object units do not match the perpendicularDistance object units')

                 if(!object@pdsType %in% .StemEnv$pdsTypes)
                   return('illegal pdsType requested!')

                 if(object@pds@units != object@downLog@units)
                   return('units for pds and downLog components incompatible!')
                 
                 return(TRUE)
               } #validity check
) #class perpendicularDistanceIZ 









#=================================================================================================
#
#  8. the omnibusPDSIZ class is a direct descendant of 'perpendicularDistanceIZ'...
#
#
#     added: 4-Feb-2011
#
setClass('omnibusPDSIZ',
    contains = 'perpendicularDistanceIZ'
) #class omnibusPDSIZ 






#=================================================================================================
#
#  9. the distanceLimitedIZ class is a direct descendant of 'downLogIZ'...
#
#
#     below: hc = homogeneous coordinates
#
#     added: 25-May-2011
#
setClass('distanceLimitedIZ',
    representation(dls = 'distanceLimited',         #the distanceLimited sampling object
                   izPerim = 'matrix',              #holds the iz perimeter in matrix form w/ hc
                   area = 'numeric',                #exact area of the inclusion zone
                   perimeter = 'SpatialPolygons',   #izone perimeter in 'SpatialPolygons' form
                   pgArea = 'numeric'               #polygon izone area approximation
                  ),
    contains = 'downLogIZ',                         #a subclass of the 'downLogIZ' class
    validity = function(object) {
                 if(object@area < 0 || is.na(object@area))
                   return('object has negative or missing inclusion zone area!')

                 if(object@pgArea < 0 || is.na(object@pgArea))
                   return('object has negative or missing polygon area!')
                 
                 return(TRUE)
               } #validity check
) #class distanceLimitedIZ 




#=================================================================================================
#
#  10. the distanceLimitedMCIZ class is a direct descendant of 'distanceLimitedIZ'; the dates
#      below are off compared with the parent because I actually developed the MC version first,
#      then added the distanceLimtedIZ class, this necessitated switching the defintion so
#      that the parent was the base for this class of objects
#
#     added: 22&23-Mar-2011
#
setClass('distanceLimitedMCIZ',
    contains = 'distanceLimitedIZ'
) #class distanceLimitedMCIZ 






#=================================================================================================
#
#  11. the distanceLimitedPDSIZ class is a direct descendant of 'perpendicularDistanceIZ'...
#
#      Two class unions are defined below to allow the slots to be either filled with something
#      of the correct class, or be empty with NULL
#
#     added: 8&10-Mar-2011
#
setClassUnion('pdsIZNull', c('perpendicularDistanceIZ', 'omnibusPDSIZ', 'NULL')) #allow for omnibus
setClassUnion('dlsIZNull', c('distanceLimitedIZ', 'NULL')) 
setClass('distanceLimitedPDSIZ',
    representation(dls = 'distanceLimited',               #the distanceLimited ArealSampling object
                   dlsDiameter = 'numeric',               #limiting diameter
                   pdsPart = 'pdsIZNull',                 #pds object for pds section of the log
                   dlsPart = 'dlsIZNull',                 #distance limited component of the log
                   pdsFull = 'pdsIZNull'                  #as if this were a full PDS object  (never NULL?)
                  ),
    contains = 'perpendicularDistanceIZ',
         
    validity = function(object) {

                 if(object@dlsDiameter <= 0)
                   return('distance limit diameter must be positive')
                 
                 return(TRUE)
               } #validity check
                  
) #class distanceLimitedPDSIZ 








#=================================================================================================
#
#  12. the omnibusDLPDSIZ class is a direct descendant of 'distanceLimitedPDSIZ'...
#
#      combines DL with omnibus PDS
#
#     added: 15-Mar-2011
#
setClass('omnibusDLPDSIZ',
    contains = 'distanceLimitedPDSIZ'
) #class omnibusDLPDSIZ 


         



         






