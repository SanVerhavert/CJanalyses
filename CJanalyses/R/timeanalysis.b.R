
# This file is a generated template, your changes will not be overwritten

timeAnalysisClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeAnalysisClass",
    inherit = timeAnalysisBase,
    private = list(
        .run = function() {
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          if( is.null( self$options$duration ) )
            return( NULL )
          
          duration <- self$data[[self$options$duration]]
          
          tableGroup <- self$results$judge$table
          # plotGroup <- self$results$judge$plot
          
          resultGen <- summary( duration )
          resultGen <- c( resultGen, sd = sd( duration ) )
          
          tableGroup$general$addRow( rowKey = 1,
                                     values = list( 
                                       mean = resultGen[ "Mean"],
                                       sd = resultGen[ "sd" ],
                                       min = resultGen[ "Min."],
                                       max = resultGen[ "Max."] )
                                    )
          
          tableGroup$general$setState( resultGen )
            
          if( !is.null( self$options$Judge ) )
          {
            judge <- self$data[[self$options$Judge]]
            
            tableGroup$split$setVisible( TRUE )
            
            resultsSplit <- by( data = duration, INDICES = judge,
                                FUN = function( x ){ 
                                  res <- summary(x)
                                  res <- c( res, sd = sd(x) )
                                }  )
            
            for( i in 1:length( resultsSplit ) )
            {
              tableGroup$split$addRow( rowKey = i,
                                       values = list(
                                         judge = names( resultsSplit )[i],
                                         mean = resultsSplit[[i]][ "Mean" ],
                                         sd = resultsSplit[[i]]["sd" ],
                                         min = resultsSplit[[i]][ "Min." ],
                                         max = resultsSplit[[i]][ "Max." ] )
                                       )
            }
            rm(i)
            
            tableGroup$split$setState( resultsSplit )
            
          } else tableGroup$split$setVisible( FALSE )
        } )
)
