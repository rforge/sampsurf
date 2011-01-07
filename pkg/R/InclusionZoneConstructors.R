#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructor methods of the
#   InclusionZone class & subclasses...
#
#   The methods include...
#     1. a constructor for 'standUpIZ'
#     2. for 'chainSawIZ'
#     3. for 'sausageIZ'
#
#   Note that the sp, raster, and gpclib packages must be loaded.
#
#   In each case, all estimated quatities are calculated for the object;
#   i.e., cubicVolume & Density; whatever else might be desired should
#   be added later.
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
#   generic definition...
#
#if (!isGeneric("standUpIZ")) 
  setGeneric('standUpIZ',  
             function(downLog, plotRadius, ...) standardGeneric('standUpIZ'),
             signature = c('downLog', 'plotRadius')
            )

#if (!isGeneric("chainSawIZ")) 
  setGeneric('chainSawIZ',  
             function(downLog, plotRadius, ...) standardGeneric('chainSawIZ'),
             signature = c('downLog', 'plotRadius')
            )

#if (!isGeneric("sausageIZ")) 
  setGeneric('sausageIZ',  
             function(downLog, plotRadius, ...) standardGeneric('sausageIZ'),
             signature = c('downLog', 'plotRadius')
            )

   



       
#================================================================================
#  method for functions and class standUpIZ...
#
setMethod('standUpIZ',
          signature(downLog = 'downLog', plotRadius = 'numeric'),
function(downLog,
         plotRadius,
         description = 'inclusion zone for "standup" method',
         spID = unlist(strsplit(tempfile('cp:',''),'\\/'))[2],
         #spID = paste('cp',format(runif(1,0,10000),digits=8),sep='.'),
         spUnits = CRS(projargs=as.character(NA)),
         ...
        )
{
#------------------------------------------------------------------------------
#
#   get bbox from the downLog object...
#
    downLog.bbox = bbox(downLog@spLog)
    units = downLog@units
   
#
#   now also from the circularPlot object...
#
    loc = coordinates(downLog@slNeedleAxis)$naLines$naLine[1,]    #transformed butt center point
    circularPlot = circularPlot(plotRadius, units=units, centerPoint=loc,
                                spID = spID, spUnits = spUnits,...)
    cp.bbox = bbox(circularPlot@perimeter)

#
#   combine them for the overall bbox...
#
    min = apply(cbind(downLog.bbox, cp.bbox), 1, min)
    max = apply(cbind(downLog.bbox, cp.bbox), 1, max)
    bbox = matrix(cbind(min,max),ncol=2, dimnames=list(c('x','y'), c('min','max')))

#
#   per unit area estimates...
#
    unitArea = ifelse(downLog@units==.StemEnv$msrUnits$English, .StemEnv$sfpAcre, .StemEnv$smpHectare) 
    puaBlowup = unitArea/circularPlot@area 
    puaEstimates = list(downLog@logVol*puaBlowup, puaBlowup)
    names(puaEstimates) = .StemEnv$puaEstimates[c('cubicVolume', 'Density')]

#
#   create the object...
#
    dlIZ = new('standUpIZ', downLog=downLog, circularPlot=circularPlot,
               bbox=bbox, spUnits=spUnits, description=description,
               units = units, puaBlowup = puaBlowup, puaEstimates = puaEstimates
              )

    return(dlIZ)
}   #standUpIZ constructor
)   #setMethod


   



       
#================================================================================
#  method for functions and class chainSawIZ...
#
setMethod('chainSawIZ',
          signature(downLog = 'downLog', plotRadius = 'numeric'),
function(downLog,
         plotRadius,
         plotCenter = c(x=0, y=0),
         description = 'inclusion zone for "chainsaw" method',
         spID = unlist(strsplit(tempfile('cs:',''),'\\/'))[2],
         #spID = paste('cp',format(runif(1,0,10000),digits=8),sep='.'),
         spUnits = CRS(projargs=as.character(NA)),
         ...
        )
{
#------------------------------------------------------------------------------
#
#   Currently, if there is no intersection between the plot and the log, the
#   routine will stop with an error message. This may not be the best thing
#   in general, but in sampling surface simulations, only plots whose centers
#   fall within the sausage-shaped inclusion zone should be passed here, so
#   unless someone tries to use this for some other purpose, it will not matter.
#------------------------------------------------------------------------------
#
#   the only way to do chainsaw is with the built-in taper equation, because we
#   must be able to calculate the sliver, bounding bolt, etc.; so if the log's
#   taper comes from some other source is.null(solidType), then we can not
#   proceed with this method...
#
    if(is.null(downLog@solidType))
      stop('Log taper must come from the built-in taper equation to use the chainsaw method')
  
#
#   get bbox from the downLog object...
#
    downLog.bbox = bbox(downLog@spLog)
    units = downLog@units
    logID = downLog@spLog@polygons$pgsLog@ID
   
#
#   now also from the circularPlot object...
#
    circularPlot = circularPlot(plotRadius, units=units, centerPoint=plotCenter,
                                spID = spID, spUnits = spUnits,...)
    cp.bbox = bbox(circularPlot@perimeter)
    cpCoords = coordinates(circularPlot@perimeter@polygons$pgsCircPlot@Polygons$circPlot)


#
#   combine them for the overall bbox...
#
    min = apply(cbind(downLog.bbox, cp.bbox), 1, min)
    max = apply(cbind(downLog.bbox, cp.bbox), 1, max)
    bbox = matrix(cbind(min,max),ncol=2, dimnames=list(c('x','y'), c('min','max')))

#
#   per unit area estimates...
#
    unitArea = ifelse(downLog@units==.StemEnv$msrUnits$English, .StemEnv$sfpAcre, .StemEnv$smpHectare) 
    puaBlowup = unitArea/circularPlot@area 

#
#   this uses the gpclib routines for intersection of plot & log polygons...
#
    gPlot = as(cpCoords, 'gpc.poly')                     #make a gpc polygon from the plot
    gLog = as(coordinates(downLog@spLog@polygons$pgsLog@Polygons$log), 'gpc.poly') #gpc polygon of the log 
    ov.log = intersect(gPlot, gLog)                      #overlay, result is of class gpc.poly
    if(length(ov.log@pts) == 0)                          #check for no intersection
      stop(paste('**>No plot-log intersection at plotCenter=',plotCenter[1],',',plotCenter[2]))
    mSect = as(ov.log, 'matrix')                         #convert sliver section to matrix for sp
    mSect = rbind(mSect, mSect[1,])                      #close the polygon
    pgSect = Polygon(mSect)                              #sliver matrix to sp classes...
    pgsSect = Polygons(list(sliver=pgSect), ID=paste(logID,'Sliver',sep=':')) 
    spSect = SpatialPolygons(list(pgsSliver=pgsSect))       #takes a list of Polygons objects

    css = chainsawSliver(downLog, sect = mSect, gLog = gLog, ...)

    puaEstimates = list(css$sectVol * puaBlowup, puaBlowup)
    names(puaEstimates) = .StemEnv$puaEstimates[c('cubicVolume', 'Density')]


#
#   and make a SpatialPolygons object out of the bolt...
#
    pgBolt = Polygon(css$rotBolt[,-3])                             #sans hc
    pgsBolt = Polygons(list(sausage=pgBolt), ID=paste(logID,'BBolt',sep=':'))
    spBolt = SpatialPolygons(list(pgsBolt=pgsBolt),      #takes a list of Polygons objects
                                proj4string = spUnits                       
                               )
    css$spBolt = spBolt
    
#
#   create the object...
#
    csIZ = new('chainSawIZ', downLog=downLog, circularPlot=circularPlot,
               bbox=bbox, spUnits=spUnits, description=description,
               units = units, puaBlowup = puaBlowup, puaEstimates = puaEstimates,
               sliver = spSect,
               bolt = css
              )

    return(csIZ)
        

}   #chainSawIZ constructor
)   #setMethod





    


       
#================================================================================
#  method for functions and class sausageIZ...
#
setMethod('sausageIZ',
          signature(downLog = 'downLog', plotRadius = 'numeric'),
function(downLog,
         plotRadius,
         nptsHalfCircle = 50,          #number of points in each end's half circle
         description = 'inclusion zone for dowed log "sausage" sampling method',
         spID = unlist(strsplit(tempfile('sausageIZ:',''),'\\/'))[2],
         #spID = paste('cp',format(runif(1,0,10000),digits=8),sep='.'),
         spUnits = CRS(projargs=as.character(NA)),
         ...
        )
{
#------------------------------------------------------------------------------
#
#   transformation matrix...
#
    centerOffset = coordinates(downLog@location)
    logAngle = downLog@logAngle
    trMat = transfMatrix(logAngle, centerOffset)

    
#
#   direct calculation of the inclusion zone area and blowup factor...
#
    logLen = downLog@logLen
    izArea = 2.0*logLen*plotRadius + pi*plotRadius*plotRadius 

#
#   per unit area estimates...
#
    unitArea = ifelse(downLog@units==.StemEnv$msrUnits$English, .StemEnv$sfpAcre, .StemEnv$smpHectare) 
    puaBlowup = unitArea/izArea 
    puaEstimates = list(downLog@logVol*puaBlowup, puaBlowup)
    names(puaEstimates) = .StemEnv$puaEstimates[c('cubicVolume', 'Density')]
  
#
#   make sure things are even...
#
    halfLen = logLen/2
    if(nptsHalfCircle > 10)
      npts = nptsHalfCircle
    else
      npts = 50
    
    if(npts%%2 > 0)             #augment if odd
      npts = npts+1
    nptsHalfCircle = npts/2

#
#   left half of the circle, then right...
#
    circ.left = rev( seq(pi/2, 3*pi/2, len=npts) )
    circ.right = rev( c(seq(3*pi/2, 2*pi, len=nptsHalfCircle), seq(0, pi/2, len=nptsHalfCircle)) )
    
#
#   make some sausage...
#
    sausage = matrix(c(-halfLen, plotRadius, 1), nrow=1)                      #left point on top "side"
    
#   right-half circle starts at top-right point, ends at bottom-right point...    
    sausage = rbind(sausage, cbind(halfLen + plotRadius*cos(circ.right),   
                                   plotRadius*sin(circ.right), rep(1,npts)) )
    
#   left half of circle starts at bottom-left, ends at top-left (beginning) point...
    sausage = rbind(sausage, cbind(-halfLen + plotRadius*cos(circ.left),     
                                   plotRadius*sin(circ.left), rep(1,npts)) )

#    
#   any little difference between start & end pts with identical() can mess up the
#   the sp package Polygon routine, so set the end point exactly to start, then transform...
#
    sausage[2*npts+1,] = sausage[1,]
    
    sausage = sausage %*% trMat
    dimnames(sausage) = list(NULL,c('x','y','hc'))

#
#   and make a SpatialPolygons object...
#
    pgSausage = Polygon(sausage[,-3])                             #sans hc
    pgsSausage = Polygons(list(sausage=pgSausage), ID=spID)
    spSausage = SpatialPolygons(list(pgsSausage=pgsSausage),      #takes a list of Polygons objects
                                proj4string = spUnits                       
                               )
    pgSausageArea = pgSausage@area

#
#   create the object...
#
    sausIZ = new('sausageIZ', downLog=downLog,
                 sausage = sausage,                  #matrix representation of perimeter
                 perimeter = spSausage,              #SpatialPolygons perimeter
                 pgSausageArea = pgSausageArea,      #area of SpatialPolygons sausage: approximate
                 spUnits = spUnits,                  #CRS units
                 description = description,          #a comment
                 units = downLog@units,              #units of measure
                 area = izArea,                      #exact inclusion zone area
                 puaBlowup = puaBlowup,              #sausage per unit area blowup factor
                 puaEstimates = puaEstimates,        #per unit area estimates
                 radius = plotRadius,                #plot radius for sausage
                 bbox = bbox(spSausage)              #overall bounding box--redundant here
                )

    return(sausIZ)
}   #sausageIZ constructor
)   #setMethod
    
