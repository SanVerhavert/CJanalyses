relEvolClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "relEvolClass",
    inherit = relEvolBase,
    private = list(
      .run = function() {
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        
        # block error when no variables provided
        if( is.null( self$options$Judge ) |
            is.null( self$options$Repr1 ) |
            is.null( self$options$Repr2 ) |
            is.null( self$options$Selected ) |
            is.null( self$options$OrderOn ) )
          return( NULL )
        
        # preparation ----
        Data <- self$data[ , c( self$options$Judge,
                                self$options$Repr1,
                                self$options$Repr2,
                                self$options$Selected,
                                self$options$OrderOn ) ]
        
        names( Data ) <- c( "Judge", "Repr1", "Repr2", "Selected", "OrderOn" )
        
        Data$Judge <- as.character( Data$Judge )
        Data$Repr1 <- as.character( Data$Repr1 )
        Data$Repr2 <- as.character( Data$Repr2 )
        Data$Selected <- as.character( Data$Selected )
        
        Repr <- unique( c( Data$Repr1, Data$Repr2 ) )
        
        #---------------------------------------------------------------------
        # Calculate Score ----
        # will eventually be removed when jamovi functionality is expanded
        Data <- calcScore( Data = Data )
        #---------------------------------------------------------------------
        
        Data <- na.omit( Data )
        
        # Order data on OrderOn ----
        timeVec <- strptime( as.character( Data$OrderOn ),
                             format = "%Y/%m/%d %H:%M:%S" )
        suppressWarnings(
          numVec <- as.numeric( as.character( Data$OrderOn ) )
        )
        
        if( !any( is.na( timeVec ) ) )
        {
          Data <- Data[ order( timeVec ), ]
        } else if( !any( is.na( numVec ) ) )
        {
          Data <- Data[ order( numVec ) ]
        } else
        {
          self$results$Warning$setVisible( TRUE )
          warnText <- "!! Warning! Data not sorted before calculations! !!"
          self$results$Warning$setContent( warnText )
        }
        
        # Calculate Rounds ----
        Data <- data.frame( Data, rounds = rep( 0, times = length( Data$Judge ) ) )

        reprCount <- rep( 0, times = length( Repr ) )
        names( reprCount ) <- Repr

        for( i in 1:length( Data$Repr1 ) )
        {
          Scr1ID <- which( names( reprCount ) == Data$Repr1[i] )
          Scr2ID <- which( names( reprCount ) == Data$Repr2[i] )

          reprCount[ Scr1ID ] <- reprCount[ Scr1ID ] + 1
          reprCount[ Scr2ID ] <- reprCount[ Scr2ID ] + 1

          Data$rounds[i] <- reprCount[ Scr1ID ]

          rm( Scr1ID, Scr2ID )
        }
        rm(i, reprCount)

        # Calculate reliability evolution
        reliabEvol <- data.frame( rounds = unique( Data$rounds )[ order( unique( Data$rounds ) ) ],
                                  reliability = numeric( length( unique( Data$rounds ) ) ),
                                  returns = numeric( length( unique( Data$rounds ) ) ),
                                  nJudges = numeric( length( unique( Data$rounds ) ) ) )
        nJudgements <- data.frame( Judge = unique( Data$Judge ) )

        for( i in unique( Data$rounds )[ order( unique( Data$rounds ) ) ] )
        {
          DataSub <- subset( Data, Data$rounds <= i )
          DataSub$cte <- 1

          # estimate
          Ability <- BTLm( Data = DataSub[ , c( "Repr1", "Repr2", "Score" ) ] )

          ### reliability ###
          reliabEvol$reliability[i] <- round( reliability( Abildf = Ability ),
                                              digits = 2 )

          if( i > 1 )
          {
            reliabEvol$returns[i] <- abs( reliabEvol$reliability[i] -
                                            reliabEvol$reliability[ i - 1 ] )
          }
          
          ### judges per round ####
          reliabEvol$nJudges[i] <- length( unique( DataSub$Judge ) )
          
          ### Judgement in round by judge
          nJudgementsT <- aggregate( DataSub$cte,
                                    by = list( Judge = DataSub$Judge ),
                                    FUN = sum )
          nJudgements <- merge( nJudgements, nJudgementsT, by = "Judge",
                                all.x = T )
          names( nJudgements )[i+1] <- paste0( "R", i )
        }
        rm(i)
        
        # output ----
        for( i in reliabEvol$rounds )
        {
          self$results$table$addRow( rowKey = i,
                        values = list(
                          RoundNo = i,
                          Rel = reliabEvol$reliability[i],
                          Returns= reliabEvol$returns[i]
                        )
          )
        }
        rm(i)

        self$results$table$setState( reliabEvol )
        self$results$judgePlot$setState( nJudgements )
        
        if( self$options$nJudges |
            self$options$judgeRounds )
          self$results$judgePlot$setVisible( TRUE )
        
        if( !self$options$nJudges &
            !self$options$judgeRounds )
          self$results$judgePlot$setVisible( FALSE )
      },
      .evolPlot = function( ... ) {
        # block error when no variables provided
        if( is.null( self$options$Judge ) |
            is.null( self$options$Repr1 ) |
            is.null( self$options$Repr2 ) |
            is.null( self$options$Selected ) |
            is.null( self$options$OrderOn ) )
          return( NULL )
        
        reliabEvol <- self$results$table$state
        reliabEvol <- reliabEvol[ -1, ]
        
        plotCol <- c( "#2080be",  "#57b6af" )
        
        par( mar = c( 5, 4, 4, 4 ) )

        plot( reliabEvol$rounds, reliabEvol$reliability, type = "b",
              main = "Reliability evolution plot\nWith returns",
              xlab = "Rounds", ylab = "SSR", ylim = c( 0, 1), xaxt = "n",
              col = plotCol[1])
        lines( reliabEvol$rounds, reliabEvol$returns, type = "b", col = plotCol[2] )

        abline( h = 0.01, lty = 2, col = plotCol[2] )

        axis( side = 1, at = 1:max( reliabEvol$rounds ) )
        axis( side = 4, col = "grey50",
              col.axis = "grey50" )
        mtext( "Returns", side = 4, line = 3, cex = par("cex.lab"), col = plotCol[2])

        if( self$options$rel70 )
        {
          
          abline( h = .70, lty = 2, col = plotCol[1] )
          text( x = 1.5, y = .72, pos = 4, labels = "SSR=.70" )
        }
        
        if( self$options$rel80 )
        {
          abline( h = .80, lty = 2, col = plotCol[1] )
          text( x = 1.5, y = .82, pos = 4, labels = "SSR=.80", lty = 2 )
        }
        
        if( self$options$relMax )
        {
          abline( h = max( reliabEvol$reliability ), lty = 2 , col = plotCol[1])
          text( x = 1.5, y = max( reliabEvol$reliability ) + 0.02, pos = 4, 
                labels = paste0( "SSR=", max( reliabEvol$reliability ) ))
        }
        
        TRUE
      },
      .judgeRound = function( ... ) {
        # block error when no variables provided
        if( ( is.null( self$options$Judge ) |
              is.null( self$options$Repr1 ) |
              is.null( self$options$Repr2 ) |
              is.null( self$options$Selected ) |
              is.null( self$options$OrderOn ) ) &
            ( !self$options$nJudges | 
              !self$options$judgeRounds ) )
          return( NULL )
        
        reliabEvol <- self$results$table$state
        nJudgements <- self$results$judgePlot$state
        
        plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
        plotCol <- plotCol( length( nJudgements$Judge) )
        
        if( self$options$nJudges &
            self$options$judgeRounds )
        {
          par( mfrow = c( 1, 2 ) )
        } else
          par( mfrow = c( 1, 1 ) )
        
        if( self$options$nJudges )
        {
          plot( reliabEvol$rounds, reliabEvol$nJudges, type = "b",
                main = "Number of judges per round",
                xlab = "Rounds", ylab = "n judges", col = plotCol[1] )
          axis( side = 1, at = 1:max( reliabEvol$rounds ) )
        }
        
        
        if( self$options$judgeRounds )
        {
          theRounds <-names( nJudgements )[ -1 ]
          nRounds <- length( theRounds )
          
          plot( x = 0, y = 0, main = "Judgements per judge per round",
                xlab = "Rounds", ylab = "n judgements",
                xlim = c( 1, max( reliabEvol$rounds ) ),
                ylim = c( 0, max( nJudgements[ , -1 ], na.rm = T ) ), type = "n",
                col = plotCol[1] )
          
          for( i in 1:length( nJudgements$Judge ) )
          {
            lines( 1:nRounds , nJudgements[ i, -1 ],
                   col = plotCol[i], type = "b" )
          }
        }
        
        
        TRUE
      } )
)
