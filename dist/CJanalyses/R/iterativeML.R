#### BTLm ####
### Wrapper function for estimateAbility ###
###  Restructures data and executes estimateAbility ###
BTLm <- function( Data, epsilonCorrect = .003, est.iters = 4 )
{
  ### Preparations ###
  repr <- unique( c( Data$Repr1, Data$Repr2 ) )
  
  Abil <- data.frame( Repr = repr, Ability = 0, se = 0 )
  
  rm( repr )
  
  ### Observed Score ###
  ## in Data
  Obs1 <- aggregate( Data$Score, by = list( Repr = Data$Repr1 ), FUN = "sum" )
  Obs2 <- aggregate( 1 - Data$Score, by = list( Repr = Data$Repr2 ), FUN = "sum" )
  Obs <- rbind( Obs1, Obs2) 
  Obs <- aggregate( Obs$x, by = list( Repr = Obs$Repr ), FUN = "sum")
  Abil <- merge( Abil, Obs, by = "Repr" )
  
  names( Abil )[4] <- c( "Observed" )
  
  rm( Obs1, Obs2, Obs )
  
  Comp1 <- aggregate( Data$Score, by = list( Repr = Data$Repr1 ), FUN = "length" )
  Comp2 <- aggregate( Data$Score, by = list( Repr = Data$Repr2 ), FUN = "length" )
  Comp <- rbind( Comp1, Comp2) 
  Comp <- aggregate( Comp$x, by = list( Repr = Comp$Repr ), FUN = "sum")
  Abil <- merge( Abil, Comp, by = "Repr" )
  
  names( Abil )[5] <- c( "totalComp" )
  
  rm( Comp1, Comp2, Comp )
  
  ## Correct Abil$Observed
  interm <-  Abil$totalComp - 2 * epsilonCorrect
  interm <- interm * Abil$Observed / Abil$totalComp
  Abil$Observed <- epsilonCorrect +  interm
  rm( interm )
  
  # clean up
  Abil <- Abil[ , -5 ]
  
  ### Estimate Abilities ###
  for( i in est.iters:0 )
  {
    ## find the corresponding ability values for each representation in pair
    Repr1ID <- match( Data$Repr1, table = Abil$Repr )
    Repr2ID <- match( Data$Repr2, table = Abil$Repr )
    
    Data$AbilR1 <- Abil$Ability[ Repr1ID ]
    Data$AbilR2 <- Abil$Ability[ Repr2ID ]
    
    rm( Repr1ID, Repr2ID )
    
    Abil <- BTLm.est( Data = Data, Abil = Abil, counter = i )
  }
  rm(i)
  
  ### Output ###
  return( Abil )
}

#### BTLm.est ####
### The actual BTL optimization function ###
BTLm.est <- function(Data, Abil, counter)
{
  ## calculate expected score
  Data$raschp <- RaschProb( Data$AbilR1, Data$AbilR2 )
  raschp1 <- aggregate( Data$raschp, by = list( Repr = Data$Repr1 ), FUN = "sum" )
  raschp2 <- aggregate( ( 1 - Data$raschp ), by = list( Repr = Data$Repr2 ), FUN = "sum" )
  raschp <- rbind( raschp1, raschp2 )
  raschp <- aggregate( raschp$x, by = list( Repr = raschp$Repr ), FUN = "sum" )
  # merge with Abil
  Abil <- merge( Abil, raschp, by = "Repr", all.y = F )
  names( Abil )[5] <- "Expected"
  
  rm( raschp1, raschp2, raschp )
  
  ## calculate expected info
  Data$finfo <- FisherInfo( p = Data$raschp )
  finfo1 <- aggregate( Data$finfo, by = list( Repr = Data$Repr1 ), FUN = "sum" )
  finfo2 <- aggregate( Data$finfo, by = list( Repr = Data$Repr2 ), FUN = "sum" )
  finfo <- rbind( finfo1, finfo2 )
  finfo <- aggregate( finfo$x, by = list( Repr = finfo$Repr), FUN = "sum" )
  Abil <- merge( Abil, finfo, by = "Repr", all.y = F )
  names( Abil )[6] <- "ExpectedInfo"
  
  rm( finfo1, finfo2, finfo )
  
  if( counter != 0 )
  {
    ## estimate new ability
    Abil$AbilityN <- Abil$Ability + ( Abil$Observed - Abil$Expected ) / Abil$ExpectedInfo
  } else
  {
    ## calculate se
    Abil$seN <- 1 / sqrt( Abil$ExpectedInfo )
    
    return( data.frame( Repr = Abil$Repr, Ability = Abil$Ability, se = Abil$seN ) )
  }
  
  return( data.frame( Repr = Abil$Repr, Ability = Abil$AbilityN, se = Abil$se,
                      Observed = Abil$Observed ) )
}