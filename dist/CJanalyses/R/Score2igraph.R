Score2igraph <- function( data )
{
  for( i in 1:length( data$Repr1 ) )
  {
    if( data$Score[i] == 0 )
    {
      temp <- data$Repr2[i]
      data$Repr2[i] <- data$Repr1[i]
      data$Repr1[i] <- temp
      rm(temp)
    }
  }
  rm(i)
  data
}