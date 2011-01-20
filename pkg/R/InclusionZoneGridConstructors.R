#---------------------------------------------------------------------------
#
#   This file holds the S4 definition for the constructor methods of the
#   InclusionZoneGrid class...
#
#   The methods include...
#     1. a constructor for 'standUpIZ'
#     2. for 'chainSawIZ'
#     3. for 'sausageIZ'
#
#     4. special, for the full chainSawIZ zone contained within a sausage
#        shaped inclusion zone. No other protocol will have this mess.
#
#     5. for 'pointRelascopeIZ'
#
#   Of course, if chainSaw was not so strange, we would only require one
#   method for every different technique!
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
#   generic definition...
#
setGeneric('izGrid',  
           function(izObject, tract, ...) standardGeneric('izGrid'),
             signature = c('izObject', 'tract')
            )

#for the encompassing chainSaw-sausage protcol only...
setGeneric('izGridCSFull',  
           function(izGridSausage, tract, ...) standardGeneric('izGridCSFull'),
             signature = c('izGridSausage', 'tract')
            )




          
#================================================================================
#  This is a helper routine, not normally called from outside the construction
#  of an InclusionZoneGrid object...
#
#  method for 'matrix' and 'Tract' classes; this is used within the following
#  constructors and should not normally be called by itself as it simply
#  established the minimal bounding grid that encompasses the inclusion zone
#
setMethod('izGrid',
          signature(izObject = 'matrix', tract='Tract'),
function(izObject,  #a bbox object
         tract,
         data = 0,
         useCrop = TRUE,  #T: use crop(); F: use extent method
         ...
        )
{
#---------------------------------------------------------------------------
#
#   need to get the inclusion zone bbox aligned with the tract grid...
#
    if(!useCrop) {
      bbex = extent(izObject)
      j = intersectExtent(bbex, extent(tract) )   #get extent of overlap
      jj = alignExtent(j, tract)                  #and align to tract grid
    }
    else
      jj = extent(izObject)
    
#
#   pad one extra cell on each side for good measure...
#
    cr = xres(tract)            #square cells always
    jj@xmin = jj@xmin - cr
    jj@xmax = jj@xmax + cr
    jj@ymin = jj@ymin - cr
    jj@ymax = jj@ymax + cr

#
#   now create a raster object large enough to surround the inclusion zone bbox
#   out of the above extent...
#
#   to use crop() instead of the above, the entire IZ must lie within the tract...
#
    if(!useCrop) {
      nx = (jj@xmax - jj@xmin)/cr                   #remember x=columns
      ny = (jj@ymax - jj@ymin)/cr                   #and y=rows
      sr = raster(jj, nrows=as.integer(ny), ncols=as.integer(nx), crs=NA)
    }
    else
      sr = crop(tract, jj)
    sr[] = rep(data, ncell(sr))

    return(sr)
}   #izGrid base method
)   #setMethod



  

  


          
#================================================================================
#  method for 'standUpIZ' and 'Tract' classes...
#
setMethod('izGrid',
          signature(izObject = 'standUpIZ', tract='Tract'),
function(izObject,
         tract,
         description = 'standUpIZ inclusion zone grid object',
         wholeIZ = TRUE,           #TRUE: grid the whole object; FALSE: just grid the IZ
         ...
        )
{
#---------------------------------------------------------------------------
#
#
    griz = izGridConstruct(izObject=izObject, tract=tract, description=description,
                           wholeIZ=wholeIZ, ...)
    return(griz)
}   #izGrid for'standUpIZ'
)   #setMethod


  

  


          
#================================================================================
#  method for 'sausageIZ' and 'Tract' classes...
#
setMethod('izGrid',
          signature(izObject = 'sausageIZ', tract='Tract'),
function(izObject,
         tract,
         description = 'sausageIZ inclusion zone grid object',
         wholeIZ = TRUE,           #TRUE: grid the whole object; FALSE: just grid the IZ
         ...
        )
{
#---------------------------------------------------------------------------
#
#
    griz = izGridConstruct(izObject=izObject, tract=tract, description=description,
                           wholeIZ=wholeIZ, ...)
    return(griz)
}   #izGrid for'sausageIZ'
)   #setMethod




  
 


          
#================================================================================
#  method for 'chainSawIZ' and 'Tract' classes--of course chainsaw is wierd
#  since it is really a point inclusion zone, so it must have a more involved
#  constructor method...
#
setMethod('izGrid',
          signature(izObject = 'chainSawIZ', tract='Tract'),
function(izObject,
         tract,
         description = 'chainSaw grid point inclusion zone grid object',
         wholeIZ = TRUE,           #TRUE: grid the whole object; FALSE: just grid the IZ
         ...
        )
{
#---------------------------------------------------------------------------
#
#   find the wholeIZ bounding grid to begin with...
#
    iz.bbox = bbox(izObject)                           #grid point plus log (always internal)    
    izg = izGrid(iz.bbox, tract)                       #get the minimal bounding grid
    cpt = perimeter(izObject, whatSense='point')       #circularplot centerpoint only
    grid = pointsToRaster(izg,                         #bounding grid
                          cpt,                         #plot center location
                          values=1,                    #strange
                          function(x) 0,               #value at x,y--anything other than NA
                          background=NA                #all other values are zero
                         )

#
#   now, if we are just interested in the single grid cell inclusion zone for the plot
#   centerpoint, we must do a little more work to narrow it down within the properties
#   of the raster grid...
#
    if(!wholeIZ) {
      j = cellFromXY(grid, cpt)        #cell number for center point of the circular plot
      xy = xyFromCell(grid, j)         #cell center point, may not be same as cpt
      res = xres(grid)/2               #half resolution of the grid cells
      ex = extent(c(xy[1,'x']-res, xy[1,'x']+res,  #extent for one grid cell which contains
                  xy[1,'y']-res, xy[1,'y']+res))   #the circular plot center point
      grid = crop(grid, ex)            #ensures the result is on the same grid as parent
      }

#
#   a data frame with each pua estimate...
#
    nr = ncell(grid)
    npua = length(izObject@puaEstimates)
    df = data.frame(matrix(NA, nrow = nr, ncol = npua))
    colnames(df) = names(izObject@puaEstimates)

    gridVals = getValues(grid)
    for(i in seq_len(npua))
        df[,i] = ifelse(is.na(gridVals), 0, izObject@puaEstimates[[i]])
        

#
#   combine them for the overall bbox...
#
    grid.bbox = bbox(grid)
    iz.bbox = bbox(izObject)                        #must always be whole object extents here
    min = apply(cbind(grid.bbox, iz.bbox), 1, min)
    max = apply(cbind(grid.bbox, iz.bbox), 1, max)
    bbox = matrix(cbind(min,max),ncol=2, dimnames=list(c('x','y'), c('min','max')))
    

    griz = new('InclusionZoneGrid', description = description,
               iz = izObject,
               grid = grid,
               data = df,
               bbox = bbox
              )

    return(griz)
}   #izGrid for'chainSawIZ'
)   #setMethod

    


 


          
#================================================================================
#  method for the full 'chainSawIZ' within the encompassing sausage inclusion
#  zone and 'Tract' classes; this is probably going to be the only exception to
#  the general InclusionZoneGrid class and constructors...
#
setMethod('izGridCSFull',
          signature(izGridSausage = 'InclusionZoneGrid', tract='Tract'),
function(izGridSausage,
         tract,
         description = 'Full chainSaw-sausage inclusion zone grid object',
         runQuiet = FALSE,
         ...
        )
{
#---------------------------------------------------------------------------
#
#   By default, the sausage InclusionZoneGrid object will have a grid that
#   encompasses the entire inclusion zone and log. So we just need to apply
#   the chain saw method to each grid cell within the sausage inclusion zone.
#
#   first, make sure we have a sausage-based InclusionZoneGrid object passed since
#   the signature alone for this argument is too general...
#
    if(!is(izGridSausage@iz, 'sausageIZ'))
      stop('Argument izGridSausage must be a sausage-based InclusionZoneGrid object!')

    plotRadius = izGridSausage@iz@radius
    downLog = izGridSausage@iz@downLog

    if(!runQuiet) 
      cat('\nThis can take some time...\ngrid cell: ')

#
#   now we need to assign all internal grid cells the correct value based
#   on a applying a chainSawIZ to each cell...
#
    grid = izGridSausage@grid
    numCells = ncell(grid)
    chiz = vector('list', numCells)
    names(chiz) = paste('izgCS',1:numCells,sep='.')
    mask = getValues(grid)              #vector valued
    npua = length(izGridSausage@iz@puaEstimates)    
    df = data.frame(matrix(0, nrow = numCells, ncol = npua)) #background grid defaults to zero
    colnames(df) = names(izGridSausage@iz@puaEstimates)
    for(i in seq_len(numCells)) {
      if(!runQuiet)
        cat(i,', ',sep='')
      if(!is.na(mask[i])) {
        xy = xyFromCell(grid, i)[1,]                          #make it a vector
        izCS = chainSawIZ(downLog, plotRadius=plotRadius, plotCenter = xy)
        izgCS = izGrid(izCS, tract, wholeIZ = FALSE)           #one grid cell/point only!!!
        df[i, ] = izgCS@data
        chiz[[i]] = izgCS
      }
      else
        chiz[[i]] = NA               #the rest of the background surface objects are missing 
    }
    if(!runQuiet)
      cat('\n')

#
#   create the object...
#
    griz = new('csFullInclusionZoneGrid',
               description = description,
               iz = izGridSausage@iz,
               chiz = chiz,
               grid = grid,
               data = df,
               bbox = izGridSausage@bbox
              )
    return(griz)
}   #izGridCSFull for the full chainSaw-sausage inclusion zone
)   #setMethod

               
  

  


          
#================================================================================
#  method for 'pointRelascopeIZ' and 'Tract' classes...
#
setMethod('izGrid',
          signature(izObject = 'pointRelascopeIZ', tract='Tract'),
function(izObject,
         tract,
         description = 'pointRelascopeIZ inclusion zone grid object',
         wholeIZ = TRUE,           #TRUE: grid the whole object; FALSE: just grid the IZ
         ...
        )
{
#---------------------------------------------------------------------------
#
#
    griz = izGridConstruct(izObject=izObject, tract=tract, description=description,
                           wholeIZ=wholeIZ, ...)
    return(griz)
}   #izGrid for'pointRelascopeIZ'
)   #setMethod
