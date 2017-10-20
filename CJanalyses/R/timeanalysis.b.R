
# This file is a generated template, your changes will not be overwritten

timeAnalysisClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeAnalysisClass",
    inherit = timeAnalysisBase,
    private = list(
        .run = function() {
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          # block error when no variables provided
          if( is.null( self$options$duration ) )
            return( NULL )
          
          # easy data handling
          Data <- self$data
          Duration <- self$options$duration
          Judge <- self$options$judge
          Filter <- self$options$filter
          
          duration <- Data[ , Duration ]
          
          ## general
          # filter ----
          if( Filter != 0 )
          {
            lengthAll <- length( duration )
            duration <- duration[ duration <= Filter ]
            
            outText <- paste0( "Filtered on '<=", Filter, "'!\n",
                               "General: Removed ", lengthAll - length( duration ),
                               " entrie(s)." )
            
            rm( lengthAll )
            
            self$results$talk$setContent( outText )
          }else
            self$results$talk$setContent( "" )
          
          # preparation ----
          durLengthF <- length( duration )
          
          duration <- na.omit( duration )
          
          tableGroup <- self$results$table
          plotGroup <- self$results$plot
          
          ## judge ----
          if( !is.null( Judge ) )
          {
            tableGroup$judge$setVisible( TRUE )
            plotGroup$judge$setVisible( TRUE )
            
            Data <- Data[ c( Duration, Judge ) ]
            names( Data ) <- c( "Duration", "Judge" )
            
            # filter ----
            if( Filter != 0 )
            {
              judgeFiltd <- Data$Judge[ Data$Duration >= Filter ]
              
              Data <- Data[ Data$Duration <= Filter, ]
              
              outText <- paste0( outText,
                                 "\nJudge: Removed ", length( judgeFiltd ),
                                 " entrie(s) for Judge(s) ",
                                 paste( unique( judgeFiltd ), sep = ", " ), "." )
              
              self$results$talk$setContent( outText )
              
            }else
              self$results$talk$setContent( "" )
          }
          
          
          
          ## summary ----
          resultGen <- summary( duration )
          resultGen <- c( resultGen, sd = sd( duration ) )
          
          # output ----
          tableGroup$general$addRow( rowKey = 1,
                                     values = list(
                                       N = length( duration ),
                                       miss = durLengthF - length( duration ),
                                       mean = resultGen[ "Mean" ],
                                       sd = resultGen[ "sd" ],
                                       min = resultGen[ "Min."],
                                       max = resultGen[ "Max."] )
                                    )
          
          rm( durLengthF )
          
          tableGroup$general$setState( resultGen )
          plotGroup$general$setState( duration )
            
          # and split
          if( !is.null( Judge ) )
          {
            plotGroup$judge$setState( by( data = Data$Duration,
                                          INDICES = Data$Judge,
                                          FUN = na.omit ) )

            resultsSplit <- by( data = Data$Duration,
                                INDICES = Data$Judge,
                                FUN = function( x ){
                                  totLength <- length(x)
                                  x <- na.omit(x)
                                  res <- summary(x)
                                  res <- c( res, sd = sd(x) )
                                  res <- c( res, N = length(x) )
                                  c( res, miss = totLength - res[ "N" ] )
                                } )

            for( i in 1:length( resultsSplit ) )
            {
              tableGroup$judge$addRow( rowKey = i,
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

            tableGroup$judge$setState( resultsSplit )

          } else tableGroup$judge$setVisible( FALSE )

        },
        .generalPlot = function( ... ) {
          if( is.null( self$results$table$general$state ) )
            return( NULL )
          
          resultGen <- self$results$table$general$state
          duration <- self$results$plot$general$state
          
          layoutMat <- matrix( data = c( 1, 2 ), nrow = 1, ncol = 2 )
          layout( mat = layoutMat, widths = c( 2, 1 ) )
          
          hist( x = duration, main = "Histogram of judgement duration",
                xlab = "Durations", ylab = "Freq",
                col = "#2080be" )
          abline( v = resultGen[ "Mean" ], col = "#c24446", lty = 2 )
          
          TRUE
        },
        .judgePlot = function( ... ) {
          resultGen <- self$results$table$general$state
          resultSplit <- self$results$table$judge$state
          duration <- self$results$plot$judge$state

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
        }
        )
)
