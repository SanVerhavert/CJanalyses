
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
          Repr1 <- self$options$repr1
          Repr2 <- self$options$repr2
          Filter <- self$options$filter
          
          duration <- Data[ , Duration ]
          
          ## general
          # filter ----
          if( Filter != 0 )
          {
            lengthAll <- length( duration )
            duration <- duration[ duration <= Filter ]
            
            outText <- paste0( "Filtered on '<=", Filter, "'!\n",
                               "\nGeneral: Removed ", lengthAll - length( duration ),
                               " entrie(s).\n" )
            
            rm( lengthAll )
            
          }
          
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
            
            DataJudge <- Data[ c( Duration, Judge ) ]
            names( DataJudge ) <- c( "Duration", "Judge" )
            
            # filter ----
            if( Filter != 0 )
            {
              judgeFiltd <- DataJudge$Judge[ DataJudge$Duration >= Filter ]

              DataJudge <- DataJudge[ DataJudge$Duration <= Filter, ]

              outText <- paste0( outText,
                                 "\nJudge: Removed ", length( judgeFiltd ),
                                 " entrie(s) for Judge(s) ",
                                 paste( unique( judgeFiltd ), collapse = ", " ), ".\n" )

              rm( judgeFiltd )

            }
          }
          
          ## repr
          if( !is.null( Repr1 ) & !is.null( Repr2 ) )
          {
            tableGroup$repr$setVisible( TRUE )
            plotGroup$repr$setVisible( TRUE )
            
            DataRepr1 <- data.frame( Duration = Data[ , Duration ],
                                     Repr = Data[ , Repr1 ])
            DataRepr2 <- data.frame( Duration = Data[ , Duration ],
                                    Repr = Data[ , Repr2 ] )
            DataLong <- rbind( DataRepr1, DataRepr2 )
            
            rm( DataRepr1, DataRepr2 )
            
            # filter ----
            if( Filter != 0 )
            {
              reprFiltd <- DataLong$Repr[ DataLong$Duration >= Filter ]

              DataLong <- DataLong[ DataLong$Duration <= Filter, ]
              
              outText <- paste0( outText,
                                 "\nRepresentation: Removed ", length( reprFiltd ),
                                 " entrie(s) for representations(s) ",
                                 paste( unique( reprFiltd ), collapse = ", " ), "." )
              
              rm( reprFiltd )

            }
          }
          
          ## Filter talk ---- 
          if( Filter != 0 )
          {
            self$results$talk$setVisible( TRUE )
            
            outText <- strwrap( x = outText, width = 105, exdent = 2 )
            self$results$talk$setContent( outText )
          }else
          {
            self$results$talk$setVisible( FALSE )
            self$results$talk$setContent( "" )
          }
          
          ## general ----
          # summary ----
          resultGen <- summary( duration )
          resultGen <- c( resultGen, sd = sd( duration ) )
          
          # output ----
          tableGroup$general$addRow( rowKey = 1,
                                     values = list(
                                       N = length( duration ),
                                       miss = durLengthF - length( duration ),
                                       median = resultGen[ "Median" ],
                                       sd = resultGen[ "sd" ],
                                       min = resultGen[ "Min."],
                                       max = resultGen[ "Max."] )
                                    )
          
          rm( durLengthF )
          
          tableGroup$general$setState( resultGen )
          plotGroup$general$setState( duration )
          
          ## Judge ----
          if( !is.null( Judge ) )
          {
            # split and summary ----
            judgeSplit <- by( data = DataJudge$Duration,
                                INDICES = DataJudge$Judge,
                                FUN = function( x ){
                                  totLength <- length(x)
                                  x <- na.omit(x)
                                  res <- summary(x)
                                  res <- c( res, sd = sd(x) )
                                  res <- c( res, N = length(x) )
                                  c( res, miss = totLength - res[ "N" ] )
                                } )

            # output ----
            for( i in 1:length( judgeSplit ) )
            {
              tableGroup$judge$addRow( rowKey = i,
                                       values = list(
                                         judge = names( judgeSplit )[i],
                                         N = judgeSplit[[i]][ "N" ],
                                         miss = judgeSplit[[i]][ "miss.N" ],
                                         median = judgeSplit[[i]][ "Median" ],
                                         sd = judgeSplit[[i]]["sd" ],
                                         min = judgeSplit[[i]][ "Min." ],
                                         max = judgeSplit[[i]][ "Max." ] )
                                       )
            }
            rm(i)
            
            plotGroup$judge$setState( by( data = DataJudge$Duration,
                                          INDICES = DataJudge$Judge,
                                          FUN = na.omit ) )
            
            tableGroup$judge$setState( judgeSplit )

          } else tableGroup$judge$setVisible( FALSE )
          
          ## repr ----
          if( exists( "DataLong" ) )
          {
            # split and summary ----
            reprSplit <- by( data = DataLong$Duration,
                                INDICES = DataLong$Repr,
                                FUN = function( x ){
                                  totLength <- length(x)
                                  x <- na.omit(x)
                                  res <- summary(x)
                                  res <- c( res, sd = sd(x) )
                                  res <- c( res, N = length(x) )
                                  res <- c( res, miss = totLength - res[ "N" ] )
                                  c( res, total = sum( x ) )
                                } )
            
            # output ----
            for( i in 1:length( reprSplit ) )
            {
              tableGroup$repr$addRow( rowKey = i,
                                       values = list(
                                         repr = names( reprSplit )[i],
                                         N = reprSplit[[i]][ "N" ],
                                         miss = reprSplit[[i]][ "miss.N" ],
                                         total = reprSplit[[i]][ "total" ],
                                         median = reprSplit[[i]][ "Median" ],
                                         sd = reprSplit[[i]]["sd" ],
                                         min = reprSplit[[i]][ "Min." ],
                                         max = reprSplit[[i]][ "Max." ] )
              )
            }
            rm(i)
            
            plotGroup$repr$setState( by( data = DataLong$Duration,
                                          INDICES = DataLong$Repr,
                                          FUN = na.omit ) )
            
            tableGroup$repr$setState( reprSplit )
            
          } else tableGroup$repr$setVisible( FALSE )

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
          abline( v = resultGen[ "Median" ], col = "#c24446", lty = 2 )
          
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
            abline( v = resultGen[ "Median" ], col = "#c24446", lty = 2 )
            abline( v = resultSplit[[i]][ "Median" ], col = "#a0c178", lty = 2 )
          }
          rm(i)

          plot.new()
          legend( "topleft", legend = c( "General median", "Group median" ),
                  col = c( "#c24446", "#a0c178" ), lty = 2 )

          title( main = "Histogram of judgement duration", sub = "Per Judge",
                 outer = T, cex.main = 2 )

          TRUE
        },
        .reprPlot = function( ... ) {
          resultGen <- self$results$table$general$state
          resultSplit <- self$results$table$repr$state
          duration <- self$results$plot$repr$state
          
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
            histObj <- hist( x = duration[[i]], main = names( duration )[i],
                             xlab = "Durations", ylab = "Freq",
                             col = plotCol[i] )
            abline( v = resultGen[ "Median" ], col = "#c24446", lty = 2 )
            abline( v = resultSplit[[i]][ "Median" ], col = "#a0c178", lty = 2 )
            text( x = max( histObj$mids ), y = max( histObj$counts ),
                  labels = paste0( "Total duration: ", resultSplit[[i]][ "total" ] ),
                  xpd = T )
          }
          rm(i)
          
          plot.new()
          legend( "topleft", legend = c( "General median", "Group median" ),
                  col = c( "#c24446", "#a0c178" ), lty = 2 )
          
          title( main = "Histogram of judgement duration", sub = "Per Judge",
                 outer = T, cex.main = 2 )
          
          TRUE
        } )
)
