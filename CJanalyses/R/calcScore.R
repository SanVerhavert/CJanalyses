calcScore <- function( Data ){
  
  Score <- numeric(0)
  
  for( i in 1:length( Data$Repr1 ) )
  {
    if( !is.na( Data$Selected[i] ) )
    {
      if( Data$Selected[i] == Data$Repr1[i] )
      {
        Score <- append( Score, 1 )
      } else if( Data$Selected[i] == Data$Repr2[i] )
      {
        Score <- append( Score, 0 )
      }
    } else Score <- append( Score, NA )
    
  }
  rm( i )
  
  output <- data.frame( Data[ , ], Score )
  
  return( output )
}