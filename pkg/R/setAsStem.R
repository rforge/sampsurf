#---------------------------------------------------------------------------
#
#   Methods for coercion from some "Stem" class object to other forms.
#
#   1. from=downLogs, to=data.frame
#
#
#Author...									Date: 21-Oct-2010
#	Jeffrey H. Gove
#	USDA Forest Service
#	Northern Research Station
#	271 Mast Road
#	Durham, NH 03824
#	jhgove@unh.edu
#	phone: 603-868-7667	fax: 603-868-7604
#---------------------------------------------------------------------------
#
#   this method just converts from a 'downLogs' object to a data frame in
#   the form of the result returned from sampleLogs()...
#
setAs('downLogs', 'data.frame',
      function(from) {
        dlogs = from
        numLogs = length(dlogs@logs)
        df = data.frame(matrix(NA, nrow=numLogs, ncol=9))
        colnames(df) = c('species','logLen','buttDiam','topDiam','solidType','x','y',
                          'logAngle','logAngle.D')

        for(i in seq_len(numLogs)) {
          df[i,'species'] = dlogs@logs[[i]]@species
          df[i,'logLen'] = dlogs@logs[[i]]@logLen
          if(dlogs@units == .StemEnv$msrUnits$metric)
            cf = .StemEnv$m2cm
          else
            cf = .StemEnv$ft2in
          df[i,'buttDiam'] = dlogs@logs[[i]]@buttDiam * cf
          df[i,'topDiam'] = dlogs@logs[[i]]@topDiam * cf
          df[i,'solidType'] = dlogs@logs[[i]]@solidType
          df[i,'x'] = dlogs@logs[[i]]@location@coords[,'x']
          df[i,'y'] = dlogs@logs[[i]]@location@coords[,'y']
          df[i,'logAngle'] = dlogs@logs[[i]]@logAngle
          df[i,'logAngle.D'] = dlogs@logs[[i]]@logAngle * .StemEnv$rad2deg
        }

        return(df)
      } #function
)   #setAs

