taperInterpolate = function(dlog, whichSense = c('diameter', 'length'),
                            pts = NULL,
                            ...
                           )
{
#---------------------------------------------------------------------------
#
#   A simple little routine that will allow us to interpolate either
#   diameters or lengths from a downLog object at certain points.
#   This function uses either the built-in taper equation if the taper
#   slot was generated from this, or cubic spline interpolation otherwise.
#
#   Arguments...
#     dlog = a downLog object
#     whichSense = 'diameter' to interpolated diameters, pts must be lengths
#                  'length' to interpolate lengths, pts must be diameters
#     pts = see above, to interpolate diameters, these are lengths;
#           to interpolate lengths, these are diameters
#
#   Returns...
#     a vector of interpolated points.
#
#Author...									Date: 24-Feb-2011
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#
    require(sampSurf)
    
    if(!is(dlog, 'downLog'))
      stop('must pass a downLog argument!')
    whichSense = match.arg(whichSense)

    if(is.null(pts) || is.na(pts) || length(pts) < 1)
      stop('You must provide some points for interpolation!')

#
#   interpolate either diameter or length as desired...
#
    length = dlog@taper$length
    diameter = dlog@taper$diameter
    if(whichSense == 'diameter') {                    #interpolate diameters at given lengths
      rlen = range(length)
      rpts = range(pts)
      if(rpts[1]<rlen[1] || rpts[2]>rlen[2])
        stop('Some interpolated points are off the log!')

      if(!is.null(dlog@solidType)) 
        diams = .StemEnv$wbTaper(dlog@buttDiam, dlog@topDiam, dlog@logLen, nSegs=1,
                                 solidType=dlog@solidType, hgt=pts)$diameter 
      else
        diams = spline(length, diameter, xout=pts)$y
      return(diams)
    }
    else {                                           #interpolate lengths at given diameters
      rdiam = range(diameter)
      rpts = range(pts)
      if(rpts[1]<rdiam[1] || rpts[2]>rdiam[2])
        stop('Some interpolated points are off the log!')

      if(!is.null(dlog@solidType)) #use inverted wbTaper equation...
        lengths = dlog@logLen*(1 - ((pts - dlog@topDiam)/(dlog@buttDiam - dlog@topDiam))^(dlog@solidType/2)) 
      else
        lengths = spline(diameter, length, xout=pts)$y 
      return(lengths)
    }
      

    return(NULL)
}   #taperInterpolate


      
             
  
