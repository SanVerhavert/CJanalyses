
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
          
          durationFull <- self$data[[self$options$duration]]
          
          duration <- na.omit( durationFull )
           
          missLength <- length( durationFull ) - length( duration )
          
          tableGroup <- self$results$judge$table
          plotGroup <- self$results$judge$plot
          
          resultGen <- summary( duration )
          resultGen <- c( resultGen, sd = sd( duration ) )
          #### why is miss NaN
          tableGroup$general$addRow( rowKey = 1,
                                     values = list(
                                       N = length( duration ),
                                       miss = missLength,
                                       mean = resultGen[ "Mean" ],
                                       sd = resultGen[ "sd" ],
                                       min = resultGen[ "Min."],
                                       max = resultGen[ "Max."] )
                                    )
          
          tableGroup$general$setState( resultGen )
          plotGroup$general$setState( duration )
            
          if( !is.null( self$options$Judge ) )
          {
            judge <- self$data[[self$options$Judge]]
            
            tableGroup$split$setVisible( TRUE )
            plotGroup$split$setVisible( TRUE )
            
            plotGroup$split$setState( by( data = duration, INDICES = judge,
                                          FUN = na.omit ) )
            
            resultsSplit <- by( data = duration, INDICES = judge,
                                FUN = function( x ){ 
                                  totLength <- length(x)
                                  x <- na.omit(x)
                                  res <- summary(x)
                                  res <- c( res, sd = sd(x) )
                                  res <- c( res, N = length(x) )
                                  res <- c( res, miss = totLength - res[ "N" ] )
                                } )
            
            for( i in 1:length( resultsSplit ) )
            {
              tableGroup$split$addRow( rowKey = i,
                                       values = list(
                                         judge = names( resultsSplit )[i],
                                         N = resultsSplit[[i]][ "N" ],
                                         miss = resultsSplit[[i]][ "miss.N" ],
                                         mean = resultsSplit[[i]][ "Mean" ],
                                         sd = resultsSplit[[i]]["sd" ],
                                         min = resultsSplit[[i]][ "Min." ],
                                         max = resultsSplit[[i]][ "Max." ] )
                                       )
            }
            rm(i)
            
            tableGroup$split$setState( resultsSplit )
            
          } else tableGroup$split$setVisible( FALSE )
          
        },
        .generalPlot = function( ... ) {
          if( is.null( self$results$judge$table$general$state ) )
            return( NULL )
          
          resultGen <- self$results$judge$table$general$state
          duration <- self$results$judge$plot$general$state
          
          layoutMat <- matrix( data = c( 1, 2 ), nrow = 1, ncol = 2 )
          layout( mat = layoutMat, widths = c( 2, 1 ) )
          
          hist( x = duration, main = "Histogram of judgement duration",
                xlab = "Durations", ylab = "Freq",
                col = "#2080be" )
          abline( v = resultGen[ "Mean" ], col = "#c24446", lty = 2 )
          
          TRUE
        },
        .splitPlot = function( ... ) {
          resultGen <- self$results$judge$table$general$state
          resultSplit <- self$results$judge$table$split$state
          duration <- self$results$judge$plot$split$state
          
          plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
          plotCol <- plotCol( length( duration ) )
          
          matDat <- 1:length( resultSplit )
          
          if( ( length( resultSplit ) %% 3 ) > 0 )
          {
            matDat <- c( matDat, rep( 0, times = 3 - length( resultSplit ) %% 3 ) )
          }
          
          layoutMat <- matrix( data = matDat, ncol = 3, byrow = T )
          
          layoutMat <- cbind( layoutMat, rep( length( resultSplit ) + 1,
                                              times = nrow( layoutMat ) )
                              )
          
          par( oma = c( 0, 0, 2, 0 ) )
          
          layout( mat = layoutMat, widths = c( rep( 2, times = ncol( layoutMat - 1 ) ),
                                               1 ) )
          
          for( i in 1:length( duration ) )
          {
            hist( x = duration[[i]], main = names( duration )[i],
                  xlab = "Durations", ylab = "Freq",
                  col = plotCol[i] )
            abline( v = resultGen[ "Mean" ], col = "#c24446", lty = 2 )
            abline( v = resultSplit[[i]][ "Mean" ], col = "#a0c178", lty = 2 )
          }
          rm(i)
          
          plot.new()
          legend( "topleft", legend = c( "General mean", "Group mean" ),
                  col = c( "#c24446", "#a0c178" ), lty = 2 )
          
          title( main = "Histogram of judgement duration", sub = "Per Judge",
                 outer = T, cex.main = 2 )
          
          TRUE
        } )
)
