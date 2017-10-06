FisherInfo <- function( a = 0, b = 0, p = NULL, digits = 10 )
{
  if( is.null( p ) )
  {
    if( !exists( 'RaschProb') )
     source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\BTL_analyses\\RaschProb.R" )
    
    p <- RaschProb( a = a, b =  b )
  } 
  
  round( p * ( 1 - p ), digit = digits )
}