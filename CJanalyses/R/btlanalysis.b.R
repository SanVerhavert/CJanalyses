
# This file is a generated template, your changes will not be overwritten

BTLanalysisClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "BTLanalysisClass",
  inherit = BTLanalysisBase,
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
      
      #---------------------------------------------------------------------
      # Calculate Score ----
      # will eventually be removed when jamovi functionality is expanded
      Data <- calcScore( Data = Data )
      #---------------------------------------------------------------------
      
      image <- self$results$networkPlot
      image$setState( Score2igraph( Data[ , 2:4] ) )
      
      # Estimate (core analysis) ----
      ### Preparations ###
      origDataLength <- length( Data$Repr1 )
      
      Data <- na.omit( Data )
      
      Nremoved <- origDataLength - length( Data$Repr1 )
      
      rm( origDataLength )
      
      repr <- unique( c( Data$Repr1, Data$Repr2 ) )
      
      ### some text output
      outText <- paste0( "Estimating the Bradley-Terry-Luce Model!\n\n",
                         "Estimating ability for:\n",
                         length( Data$Repr1 ), " comparisons" )
      
      if( Nremoved != 0 ) paste0( outText, " (", Nremoved, " NA judgements omited )" )
      outText <- paste0( outText, "\nby ", length( unique( Data$Judge ) ), " judges\n" )
      outText <- paste0( outText, "for ", length( repr ), " representation\n",
                         "using ", self$options$estIters, " iterations\n" )
      
      if( self$options$epsCor > 0 ) {
        outText <- paste0( outText, "With epsilon correction factor ", self$options$epsCor, "\n\n" )
      } else outText <- paste0( outText, "without epsilon correction\n\n" )
      
      
      rm( repr )
      
      self$results$text$setContent( outText )
      
      Abil <- BTLm( Data = Data[ , 2:4 ] )
      
      #---------------------------------------------------------------------
      
      # Results output ----
      Table <- self$results$table
      
      # sort Abil
      Abil <- Abil[ order( Abil$Ability, Abil$Ability ), ]
      
      for( i in 1:length( Abil$Repr ) )
      {
        Table$addRow( rowKey = i,
                      values = list(
                        Repr = as.character( Abil$Repr[i] ),
                        Ability= Abil$Ability[i],
                        se = Abil$se[i]
                      )
        )
      }
      rm(i)
      
      self$results$table$setState( Abil )
      
      if( self$options$plotGraph )
      {
        self$results$networkPlot$setVisible( visible = TRUE )
      } else
      {
        self$results$networkPlot$setVisible( visible = FALSE )
      }
      
    },
    .netPlot = function( image, ... ) {
      
        
        Data <- image$state
        
        graphDat <- graph_from_data_frame( d = Data )
        
        res <- self$results$table$state
        
        plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
        plotCol <- plotCol( length( res$Repr) )
        
        par( mar = c( 2,2,2,2 ) )
        plot( graphDat, edge.arrow.size = .2, vertex.color = plotCol )
        
        TRUE
      
    } )
)
