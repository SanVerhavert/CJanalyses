
# This file is a generated template, your changes will not be overwritten

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
        
        Data <- na.omit( Data )
        
        Repr <- unique( c( Data$Repr1, Data$Repr2 ) )
        
        #---------------------------------------------------------------------
        # Calculate Score ----
        # will eventually be removed when jamovi functionality is expanded
        Data <- calcScore( Data = Data )
        #---------------------------------------------------------------------
        
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
          self$results$Warning$setContent( "!! Warning! Data not sorted before calculations! !!")
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
                                  returns = numeric( length( unique( Data$rounds ) ) ) )

        for( i in unique( Data$rounds )[ order( unique( Data$rounds ) ) ] )
        {
          DataSub <- subset( Data, Data$rounds <= i )

          # estimate
          Ability <- BTLm( Data = DataSub[ , c( "Repr1", "Repr2", "Score" ) ] )

          ### reliability ###
          reliabEvol$reliability[i] <- reliability( Abildf = Ability )

          if( i > 1 )
          {
            reliabEvol$returns[i] <- abs( reliabEvol$reliability[i] - reliabEvol$reliability[ i - 1 ] )
          }
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
      } )
)
