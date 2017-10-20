Rasch.misfit<-function( Data, Abil, By.Judge, boundCalc )
{
  ### Perparations
  ## find the corresponding ability values for each representation in pair
  Repr1ID <- match( Data$Repr1, table = Abil$Repr )
  Repr2ID <- match( Data$Repr2, table = Abil$Repr )
  
  Data$AbilR1 <- Abil$Ability[ Repr1ID ]
  Data$AbilR2 <- Abil$Ability[ Repr2ID ]
  
  rm( Repr1ID, Repr2ID )
  
  ## calcultate  residual (resid) from the predicted values (rashp)
  Data$raschp <- RaschProb( Data$AbilR1, Data$AbilR2 )
  Data$resid <-  Data$Score - Data$raschp
  
  ## calculate Fisher information (finfo)
  Data$finfo <- FisherInfo( p = Data$raschp )
  
  ## calculate Weighted Squared Residual ( WSR = info * pear.resid^2 = resid^2 )
  Data$WSR <- Data$resid^2
  
  ### Calculate infit
  if( !By.Judge ) #Repr
  {
    ## aggregate wsr over representation (inNom)
    wsrAggr1 <- aggregate( Data$WSR, by = list( Data$Repr1 ), FUN = sum )
    wsrAggr2 <- aggregate( Data$WSR, by = list( Data$Repr2 ), FUN = sum )
    
    wsrAggr <- rbind( wsrAggr1, wsrAggr2 )
    
    inNom <- aggregate( wsrAggr$x, by = list( wsrAggr$Group), FUN = sum )
    
    rm( wsrAggr1, wsrAggr2, wsrAggr )
    
    ## aggregate finfo over representation (inDenom)
    finfoAggr1 <- aggregate( Data$finfo, by = list( Data$Repr1 ), FUN = sum )
    finfoAggr2 <- aggregate( Data$finfo, by = list( Data$Repr2 ), FUN = sum )
    
    finfoAggr <- rbind( finfoAggr1, finfoAggr2 )
    
    inDenom <- aggregate( finfoAggr$x, by = list( finfoAggr$Group), FUN = sum )
    
    rm( finfoAggr1, finfoAggr2, finfoAggr )
    
  }else #Judge
  {
    ## aggregate wsr over judge (inNom)
    inNom <- aggregate( Data$WSR, by = list( Data$Judge ), FUN = sum )
    
    ## aggregate finfo over judge (inDenom)
    inDenom <- aggregate( Data$finfo, by = list( Data$Judge ), FUN = sum )
  }
  
  infit <- merge( inNom, inDenom, by = "Group.1",
                  suffixes = c( ".Nominator", ".Denominator" ) )
  
  ## Calculate infit ( inNom / inDenom )
  outFrame <- infit$x.Nominator / infit$x.Denominator
  
  outFrame <- data.frame( Group.1 = infit$Group.1, Infit = outFrame )
  
  rm( infit )
  
  ## calculate infit statistics ( mean +- 2 * sd )
  meanInfit <- mean( outFrame$Infit )
  sdInfit <- sd( outFrame$Infit )
  upperBound <- meanInfit + ( 2 * sdInfit )
  lowerBound <- meanInfit - ( 2 * sdInfit )
  
  outFrame$Flag <- as.character( ifelse( outFrame$Infit >= upperBound,
                                              "*", "-" ) )
  
  if( !By.Judge ) #Repr
  {
    names( outFrame )[1] <- "Repr"
  } else
  {
    names( outFrame )[1] <- "Judge"
  }
  
  ## Output
  theOutput <- list( misfit = outFrame, misfit.stats = c( mean = meanInfit,
                                                          sd = sdInfit,
                                                          lower = lowerBound,
                                                          upper = upperBound ) )
  return( theOutput )
}