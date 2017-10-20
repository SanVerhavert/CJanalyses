comparisonNumbers <- function( Data, By.judge = T )
{
  #extra kolom aan data frame toevoegen
  Data[ "Cte" ] <- 1
  
  if( !By.judge )
  {
    Repr1List <- list( Data$Repr1 )
    Repr2List <- list( Data$Repr2 )
    
    Repr1Aggr <- aggregate( Data$Cte, FUN = sum, by = Repr1List )
    Repr2Aggr <- aggregate( Data$Cte, FUN = sum, by = Repr2List )
    
    rm( Repr1List, Repr2List )
    
    ReprAggr <- rbind( Repr1Aggr, Repr2Aggr )
    names( ReprAggr ) <- c( "Repr", "Ncomparisons" )
    
    rm( Repr1Aggr, Repr2Aggr )
    
    ReprList <- list( ReprAggr$Repr )
    
    ###aggregeren op script (totale som van (pearson residual?) per script nemen)
    Ncompar <- aggregate( ReprAggr$Ncomparisons, FUN = sum, by = ReprList )
    
    rm( ReprList, ReprAggr )
    
    names( Ncompar ) <- c( "Repr", "Ncomparisons" )
  } else
  {
    ##lijst met judges maken (judge.list) dependent on select or not
    judgeList <- list( Data$Judge )
    
    
    ###aantal gemaakte vergelijkingen per judge berekenen
    Ncompar <- aggregate( Data$Cte, FUN = sum, by = judgeList )
    
    names(Ncompar) <- c( "Judge", "Ncomparisons" )
  }
  
  return( Ncompar )
}