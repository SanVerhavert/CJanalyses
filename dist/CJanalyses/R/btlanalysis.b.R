
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
      
      image <- self$results$mainTitle$networkPlot
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
      Table <- self$results$mainTitle$tableTitle$table
      
      # sort Abil
      Abil <- Abil[ order( Abil$Ability, Abil$Ability ), ]
      
      for( i in 1:length( Abil$Repr ) )
      {
        Table$addRow( rowKey = i,
                      values = list(
                        RankNo = i,
                        Repr = as.character( Abil$Repr[i] ),
                        Ability= Abil$Ability[i],
                        se = Abil$se[i]
                      )
        )
      }
      rm(i)
      
      # reliability ----
      if( self$options$rel )
      {
        self$results$mainTitle$rel$setVisible( visible = TRUE )
        
        relText <- paste0( "SSR = ", round( reliability( Abildf = Abil ),
                                            digits = 3 ) )
        
        self$results$mainTitle$rel$setContent( relText )
        
      } else self$results$mainTitle$rel$setVisible( visible = FALSE )
      
      # Misfits ----
      if( self$options$misfit != "none" )
      {
        misfitGroup <- self$results$mainTitle$misfitTable
        misfitGroup$setVisible( visible = TRUE )
        
        misfitGroup$setTitle( self$options$misfit )
        
        judgeMisfitTable <- misfitGroup$judgeMisfit
        reprMisfitTable <- misfitGroup$reprMisfit
     
        judgeMisfitTable$setVisible( visible = TRUE )
        reprMisfitTable$setVisible( visible = TRUE )
        
      } else if( self$results$mainTitle$misfitTable$visible )
      {
        misfitGroup$judgeMisfit$setVisible( visible = FALSE )
        misfitGroup$reprMisfit$setVisible( visible = FALSE )
        misfitGroup$judgePlot$setVisible( FALSE )
        misfitGroup$reprPlot$setVisible( FALSE )
      }  
      
      if( self$options$misfit == "Infit" )
      {
        judgeMisfit <- Rasch.misfit( Data = Data, Abil = Abil,
                                     By.Judge = T,
                                     boundCalc = self$options$flagBound )
        reprMisfit <- Rasch.misfit( Data = Data, Abil = Abil,
                                     By.Judge = F,
                                     boundCalc = self$options$flagBound )
        
        judgeMisfit.stats <- judgeMisfit$misfit.stats
        reprMisfit.stats <- reprMisfit$misfit.stats
        
        judgeMisfit <- judgeMisfit$misfit
        reprMisfit <- reprMisfit$misfit
        
        judgeMisfit$Infit <- ( judgeMisfit$Infit - judgeMisfit.stats["mean"] ) /
                                  judgeMisfit.stats["sd"]
        reprMisfit$Infit <- ( reprMisfit$Infit - reprMisfit.stats["mean"] ) /
                                  reprMisfit.stats["sd"]
         
        judgeMisfitTable$addColumn( name = "Infit", index = 3, type = "number" )
        reprMisfitTable$addColumn( name = "Infit", index = 3, type = "number" )
        
        for( i in 1:nrow( judgeMisfit ) )
        {
          judgeMisfitTable$addRow( rowKey = i,
                                   values = list(
                                     id = i,
                                     Judge = as.character( judgeMisfit$Judge[i] ),
                                     Infit = judgeMisfit$Infit[i],
                                     Flag = judgeMisfit$Flag[i]
                                   )
          )
        }
        rm(i)
        
        for( i in 1:nrow( reprMisfit ) )
        {
          reprMisfitTable$addRow( rowKey = i,
                                  values = list(
                                    id = i,
                                    Repr = as.character( reprMisfit$Repr[i] ),
                                    Infit = reprMisfit$Infit[i],
                                    Flag = reprMisfit$Flag[i]
                                  )
          )
        }
        rm(i)
        
      } else if ( self$options$misfit == "Lz" )
      {
        judgeMisfit <- Lz.misfit( Data = Data, Abil = Abil,
                                   By.Judge = T,
                                  flagBound = (self$options$flagBound * -1) )
        reprMisfit <- Lz.misfit( Data = Data, Abil = Abil,
                                  By.Judge = F,
                                 flagBound = self$options$flagBound * -1 )
        
        judgeMisfitTable$addColumn( name = "Lz", index = 3, type = "number" )
        reprMisfitTable$addColumn( name = "Lz", index = 3, type = "number" )
        
        for( i in 1:nrow( judgeMisfit ) )
        {
          judgeMisfitTable$addRow( rowKey = i,
                                   values = list(
                                     id = i,
                                     Judge = as.character( judgeMisfit$Judge[i] ),
                                     Lz = judgeMisfit$Lz_Misfit[i],
                                     Flag = judgeMisfit$Flag[i]
                                   )
          )
        }
        rm(i)
        
        for( i in 1:nrow( reprMisfit ) )
        {
          reprMisfitTable$addRow( rowKey = i,
                                  values = list(
                                    id = i,
                                    Repr = as.character( reprMisfit$Repr[i] ),
                                    Lz = reprMisfit$Lz_Misfit[i],
                                    Flag = reprMisfit$Flag[i]
                                  )
          )
        }
        rm(i)
      }
      
      if( self$options$misfit != "none" )
      {
        judgeMisfitTable$setState( judgeMisfit )
        reprMisfitTable$setState( reprMisfit )
        
        if( self$options$misfitPlot )
        {
          misfitGroup$judgePlot$setVisible( TRUE )
          misfitGroup$reprPlot$setVisible( TRUE )
        } else
        {
          misfitGroup$judgePlot$setVisible( FALSE )
          misfitGroup$reprPlot$setVisible( FALSE )
        }
      }
      
      #-------------------------------------------------------------------------
      
      # preparations for plots ----
      Table$setState( Abil )
      
      if( self$options$plotGraph )
      {
        self$results$mainTitle$networkPlot$setVisible( visible = TRUE )
      } else
      {
        self$results$mainTitle$networkPlot$setVisible( visible = FALSE )
      }
      
      if( self$options$plotScale )
      {
        self$results$mainTitle$scalePlot$setVisible( visible = TRUE )
      } else
      {
        self$results$mainTitle$scalePlot$setVisible( visible = FALSE )
      }
      
    },
    .netPlot = function( image, ... ) {
      
        
        Data <- image$state
        
        graphDat <- graph_from_data_frame( d = Data )
        
        res <- self$results$mainTitle$tableTitle$table$state
        
        plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
        plotCol <- plotCol( length( res$Repr) )
        
        par( mar = c( 2,2,2,2 ) )
        plot( graphDat, edge.arrow.size = .2, vertex.color = plotCol )
        
        TRUE
    },
    .scalePlot = function( image, ...) {
      Ability <- self$results$mainTitle$tableTitle$table$state
      
      yMax <- max( Ability$Ability ) + 2 * max( Ability$se )
      yMin <- min( Ability$Ability ) - 2 * max( Ability$se )
      
      plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
      plotCol <- plotCol( length( Ability$Repr) )
      
      plot( 1:length( Ability$Repr ), Ability$Ability, col = plotCol,
            xaxt = "n", xlab = "Representation",
            ylab = "Logit Score", ylim = c( yMin, yMax ) )
      axis( side = 1, at = 1:length( Ability$Repr ) )
      errbar( 1:length( Ability$Repr ), Ability$Ability,
              height = 2 * Ability$se, width = .05, col = plotCol )
      
      TRUE
    },
    .JudgePlot = function( image, ... ) {
      misfit <- self$results$mainTitle$misfitTable$judgeMisfit$state
      
      plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
      plotCol <- plotCol( length( misfit$Judge) )
      
      if( self$options$misfit == "Infit" )
      {
        ymin <- min( misfit$Infit ) - 0.5
        
        ymax <- max( max( misfit$Infit ) + 0.5, self$options$flagBound + 0.5 )
        
        plot( 1:length( misfit$Judge ), misfit$Infit, xlab = "Judge",
              ylab = "Infit", ylim = c( ymin, ymax ),
              col = plotCol )
        abline( h = self$options$flagBound, col = "red" )
      } else if( self$options$misfit == "Lz" )
      {
        ymin <- min( min( misfit$Lz ) - 0.5,
                     ( self$options$flagBound * -1 ) - 0.5 )
        
        ymax <- max( misfit$Lz ) + 0.5
        
        plot( 1:length( misfit$Judge ), misfit$Lz, xlab = "Judge",
              ylab = "Lz", ylim = c( ymin, ymax ),
              col = plotCol )
        abline( h = self$options$flagBound * -1, col = "red" )
      }
      
      TRUE
    },
    .ReprPlot = function( image, ... ) {
      misfit <- self$results$mainTitle$misfitTable$reprMisfit$state
      
      plotCol <- colorRampPalette( c( "#2080be", "#c6ddf1", "#57b6af" ) )
      plotCol <- plotCol( length( misfit$Repr) )
      
      if( self$options$misfit == "Infit" )
      {
        ymin <- min( misfit$Infit ) - 0.5
        
        ymax <- max( max( misfit$Infit ) + 0.5, self$options$flagBound + 0.5 )
        
        plot( 1:length( misfit$Repr ), misfit$Infit, xlab = "Representation",
              ylab = "Infit", ylim = c( ymin, ymax ),
              col = plotCol )
        abline( h = self$options$flagBound, col = "red" )
      } else if( self$options$misfit == "Lz" )
      {
        ymin <- min( min( misfit$Lz ) - 0.5,
                     ( self$options$flagBound * -1 ) - 0.5 )
        
        ymax <- max( misfit$Lz ) + 0.5
        
        plot( 1:length( misfit$Repr ), misfit$Lz, xlab = "Representation",
              ylab = "Lz", ylim = c( ymin, ymax ),
              col = plotCol )
        abline( h = self$options$flagBound * -1, col = "red" )
      }
      
      TRUE
    } )
)
