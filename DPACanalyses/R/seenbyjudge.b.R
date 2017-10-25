
# This file is a generated template, your changes will not be overwritten

seenByJudgeClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "seenByJudgeClass",
    inherit = seenByJudgeBase,
    private = list(
        .run = function() {
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          # block error when no variables provided
          if( is.null( self$options$Judge ) |
              is.null( self$options$Repr1 ) |
              is.null( self$options$Repr2 ) )
            return( NULL )
          
          # preparation ----
          Data <- self$data[ , c( self$options$Judge,
                                  self$options$Repr1,
                                  self$options$Repr2 ) ]
          
          Data <- na.omit( Data )
          
          names( Data ) <- c( "Judge", "Repr1", "Repr2" )
          
          resultTbl <- seenBJudge( Data = Data )
          
          for( i in 1:length( resultTbl ) )
          {
            self$results$table$addColumn( name = names( resultTbl )[i],
                                          type = "number" )
          }
          rm(i)
          
          for( i in 1:nrow(resultTbl) )
          {
            fillList <- list( Judge = row.names( resultTbl )[i] )
            
            tempList <- plyr::alply( .data = resultTbl[i,], .margins = 2 )
            names( tempList ) <- names( resultTbl )
            
            fillList <- c( fillList, plyr::llply( .data = tempList,
                                                  .fun = function( x ) x[1,1] )
                          )
            
            self$results$table$addRow( rowKey = i, values = fillList )
          }
          rm(i)
        } )
)
