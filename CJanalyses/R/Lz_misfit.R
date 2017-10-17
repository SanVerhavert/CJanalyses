Lz.misfit<-function( Data, Abil, By.Judge = T, flagBound ) {
  
  ### voorbereiding----
  ###predicted values nemen (pred) [Rash probabilities]
  # extract IDs of players to access ability estimates
  Scr1IDs <- match( Data$Repr1, Abil$Repr )
  Scr2IDs <- match( Data$Repr2, Abil$Repr )
  # calculate pred
  pred <- RaschProb( a = Abil$Ability[ Scr1IDs ] ,
                     b =  Abil$Ability[ Scr2IDs ] )
  
  ## Multiply the ln of Rash prob with score 
  ##   and the ln of inverse Rash prob with inverse score
  Lo_player1 <- Data$Score * log( pred )
  Lo_player2 <- ( 1- Data$Score ) * log( 1 - pred )
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
  DPAC <- data.frame( Data, Lo_misfit, ELo, varLo )
  
  rm( Data )
  
  #Lz MISFIT
  # voorbereiding
  if( !By.Judge ) #SCRIPT
  {
    ###lijst van Player1 en Player2 maken
    listPlayer1 <- list( DPAC$Repr1 )
    listPlayer2 <- list( DPAC$Repr2 )
    
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
    
    names( DPAC ) <- c( "Repr", "Lo_misfit", "ELo", "varLo")
    
    ###lijst met scripts maken
    aggrList <- list( DPAC$Repr )
    
  } else #JUDGES
  {
    ##lijst met judges maken
    aggrList <- list( DPAC$Judge )
  }
  
  ###aggregeren
  Lo_Misfit <- aggregate( DPAC$Lo_misfit, FUN = sum,
                          by = aggrList )
  ELO <- aggregate( DPAC$ELo, FUN = sum,
                    by = aggrList )
  varLo <- aggregate( DPAC$varLo, FUN = sum,
                      by = aggrList )
  
  Lz_Misfit <- Lo_Misfit
  Lz_Misfit[ , 2 ] <- Lo_Misfit[ , 2 ] - ELO[ , 2 ]
  Lz_Misfit[ , 2 ] <- Lz_Misfit[ , 2 ] / sqrt( varLo[ , 2 ] )
  
  Lz_Misfit[ , "Flag" ] <- ifelse( Lz_Misfit[ , 2 ] <= flagBound, "*", "-")
  
  if( !By.Judge ) # script
  {
    names( Lz_Misfit ) <- c( "Repr", "Lz_Misfit", "Flag" )
  } else
  {
    names( Lz_Misfit ) <- c( "Judge", "Lz_Misfit", "Flag" ) 
  }
  
  rownames( Lz_Misfit ) <- NULL
  
  return( Lz_Misfit )
  
}