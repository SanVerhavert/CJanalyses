
# This file is a generated template, your changes will not be overwritten

numCompClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "numCompClass",
    inherit = numCompBase,
    private = list(
        .run = function() {
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          
          # block error when no variables provided
          if( is.null( self$options$Judge ) |
              is.null( self$options$Repr1 ) |
              is.null( self$options$Repr2 ) |
              is.null( self$options$Selected ) )
            return( NULL )
          
          # preparation ----
          Data <- self$data[ , c( self$options$Judge,
                                  self$options$Repr1,
                                  self$options$Repr2,
                                  self$options$Selected ) ]
          
          names( Data ) <- c( "Judge", "Repr1", "Repr2", "Selected" )
          
          Data$Judge <- as.character( Data$Judge )
          Data$Repr1 <- as.character( Data$Repr1 )
          Data$Repr2 <- as.character( Data$Repr2 )
          Data$Selected <- as.character( Data$Selected )
          
          Data <- na.omit( Data )
          
          #---------------------------------------------------------------------
          # Calculate Score ----
          # will eventually be removed when jamovi functionality is expanded
          Data <- calcScore( Data = Data )
          #---------------------------------------------------------------------
          
          if( self$options$byJudge )
          {
            theTable <- self$results$judgeTable
            
            theTable$setVisible( TRUE )
            
            compNum <- comparisonNumbers( Data = Data, By.judge = T )
            
            for (i in 1:nrow( compNum ) )
            {
              theTable$addRow( rowKey = i,
                               values = list(
                                 Judge = as.character( compNum$Judge[i] ),
                                 nComp= compNum$Ncomparisons[i]
                               )
                              )
            }
            rm(i)
            
          } else
          {
            self$results$judgeTable$setVisible( FALSE )
          }
          
          if( self$options$byRepr )
          {
            theTable <- self$results$reprTable
            
            theTable$setVisible( TRUE )
            
            compNum <- comparisonNumbers( Data = Data, By.judge = F )
            
            for (i in 1:nrow( compNum ) )
            {
              theTable$addRow( rowKey = i,
                               values = list(
                                 Repr = as.character( compNum$Repr[i] ),
                                 nComp= compNum$Ncomparisons[i]
                               )
                              )
            }
            rm(i)
          } else
          {
            self$results$reprTable$setVisible( FALSE )
          }
        } )
)
