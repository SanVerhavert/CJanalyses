Lz.misfit<-function( Input, data, subject=NA, player1, player2,
                        score = NA, by.Judge=T ) {
  
  ### Check if package is installed
  if( !is.data.frame( Input ) )
  {
    source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\Check_Package.R" )
    Check.Pac( "BradleyTerry2" )

    if( !is.factor( data[ , player1 ] ) && !is.factor( data[ , player2 ] ) )
    {
      stop( "player1 and player2 should be factors" )
    }else if( sum( levels( data[ , player1 ] ) %in% levels( data[ , player2 ] ) ) != length( levels( data[ , player1 ] ) ) )
    {
      stop( "player1 and player2 should be factors with the same levels" )
    }

  } else
  {
    if( !( "Script" %in% colnames( Input ) ) &&
        !( "trueScore" %in% colnames ) )
    {
      stop( "the data frame supplied to Input must contain columns named Script and trueScore, " )
    }

    if( is.na( score ) )
    {
      stop( "The input is a data frame. You should suply the name of the score column in data")
    }
    data <- data[ complete.cases( data ), ]
  }
  
  
  ### basic input checks
  if( length( player1 ) != 1 && length( player2 ) != 1 &&
      !is.character( player1 ) && !is.character( player2 ) )
  {
    stop( "player 1 and player2 should be strings indicating the names of the columns with the representations" )
  }

  if( by.Judge && is.na( subject ) )
  {
    stop( "If you want to aggregate by subject, you need to provide the name of the subject column" )
  }

  
  ### voorbereiding----
  if( !is.data.frame( Input ) )
  {
    ###predicted values nemen (pred) [Rash probabilities]
    pred <- round( predict( Input, type="response" ), digits = 10 )
  } else
  {
    
    ###predicted values nemen (pred) [Rash probabilities]
    # extract IDs of players to access ability estimates
    Scr1IDs <- match( data[ , player1 ], Input$Script )
    Scr2IDs <- match( data[ , player2 ], Input$Script )
    # calculate pred
    pred <- RaschProb( a = Input$trueScore[ Scr1IDs ] ,
                       b =  Input$trueScore[ Scr2IDs ] )
  }
  
  ## Multiply the ln of Rash prob with score 
  ##   and the ln of inverse Rash prob with inverse score
  Lo_player1 <- data[ , score ] * log( pred )
  Lo_player2 <- ( 1- data[ , score ] ) * log( 1 - pred )
  ## sum those per comparison
  Lo_misfit <- Lo_player1 + Lo_player2
  
  rm( Lo_player1, Lo_player2 )
  
  ## Calculate expected Lo
  ELo_player1 <- pred * log( pred )
  ELo_player2 <- ( 1 - pred ) * log( 1 - pred )
  ELo <- ELo_player1 + ELo_player2

  rm( ELo_player1, ELo_player2)

  ## Calculate variance Lo
  varLo_A <- pred * ( 1 - pred )
  varLo_B <- pred / ( 1 - pred )
  varLo_B <- log( varLo_B )^2
  varLo <- varLo_A * varLo_B

  rm( varLo_A, varLo_B )
  
  #GEMERGED DATABESTAND MAKEN
  DPAC <- data.frame( data, Lo_misfit, ELo, varLo )
  
  rm( data )
  
  #Lz MISFIT
  # voorbereiding
  if( !by.Judge ) #SCRIPT
  {
    ###lijst van Player1 en Player2 maken
    listPlayer1 <- list( DPAC[ , player1 ] )
    listPlayer2 <- list( DPAC[ , player2 ] )
    
    ###winProbWinner aggregreren voor elke lijst afzonderlijk
    ## list in 'by' stays connected to the original variable it was constructed from
    Player1.aggre_Lo <- aggregate( DPAC$Lo_misfit, FUN = sum,
                                   by = listPlayer1 )
    Player1.aggre_ELo <- aggregate( DPAC$ELo, FUN = sum,
                                  by = listPlayer1 )
    Player1.aggre_varLo <- aggregate( DPAC$varLo, FUN = sum,
                                  by = listPlayer1 )
    
    Player2.aggre_Lo <- aggregate( DPAC$Lo_misfit, FUN = sum,
                                   by = listPlayer2 )
    Player2.aggre_ELo <- aggregate( DPAC$ELo, FUN = sum,
                                    by = listPlayer2 )
    Player2.aggre_varLo <- aggregate( DPAC$varLo, FUN = sum,
                                      by = listPlayer2 )

    rm( DPAC )
    
    ###beide bestanden samenvoegen
    DPAC_Lo <- rbind( Player1.aggre_Lo, Player2.aggre_Lo )
    DPAC_ELo <- rbind( Player1.aggre_ELo, Player2.aggre_ELo )
    DPAC_varLo <- rbind( Player1.aggre_varLo, Player2.aggre_varLo )

    ### mergen
    DPAC <- merge( DPAC_Lo, DPAC_ELo, by = "Group.1" )
    DPAC <- merge( DPAC, DPAC_varLo, by = "Group.1" )

    rm( DPAC_Lo, DPAC_ELo, DPAC_varLo )
    
    names( DPAC ) <- c( "representations", "Lo_misfit", "ELo", "varLo")
    
    ###lijst met scripts maken
    aggrList <- list( DPAC[ , 1 ] )
    
  } else #JUDGES
  {
    ##lijst met judges maken
    aggrList <- list( DPAC[ , subject ] )
  }
  
  ###aggregeren
  Lo_Misfit <- aggregate( DPAC$Lo_misfit, FUN = sum,
                          by = aggrList )
  ELO <- aggregate( DPAC$ELo, FUN = sum,
                    by = aggrList )
  varLo <- aggregate( DPAC$varLo, FUN = sum,
                      by = aggrList )
  
  Lz_Misfit <-Lo_Misfit
  Lz_Misfit[ , 2 ] <- Lo_Misfit[ , 2 ] - ELO[ , 2 ]
  Lz_Misfit[ , 2 ] <- Lz_Misfit[ , 2 ] / sqrt( varLo[ , 2 ] )
  
  if( !by.Judge ) # script
  {
    names( Lz_Misfit ) <- c( "Representation", "Lz_Misfit" )
  } else
  {
    names( Lz_Misfit ) <- c( subject, "Lz_Misfit" ) 
  }
  
  rownames( Lz_Misfit ) <- NULL
  
  return( Lz_Misfit )
  
}