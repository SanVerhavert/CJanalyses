seenByJudge <- function( Data ){
  DataRepr2 <- Data[ , c( "Judge", "Repr2" ) ]
  names( DataRepr2 )[2] <- "Repr1"
  DataLong <- rbind( Data[ , c( "Judge", "Repr1" ) ], DataRepr2 )
  OutTbl <- table( DataLong$Judge, DataLong$Repr1 )
  
  return( as.data.frame( OutTbl ) )
}