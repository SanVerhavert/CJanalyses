################################# Rasch Separation Reliability (RSR) #################################

######################################################################################################
#                                                                                                    #
#       Author: Tine van Daal  adapted by San Verhavert                                              #
#                                                                                                    #
#       Description:                                                                                 #
#         reliAbility calculates the Rasch Separation ReliAbility (RSR)                              #
#         of a Bradley-Terry-Luce model (BTLmodel).                                                  #
#       Arguments and options:                                                                       #
#         Abildf = fitted BTL model or dataframe with Abildf                                       #
#         check = Check if package is installed                                                      #
#                                                                                                    #
######################################################################################################

G.InputCheck <- function( Abildf = NULL, Repr = NULL, Ability = NULL,
                                   se = NULL, Fixed = NULL, remove.inf = F )
{
  if( is.null( Abildf ) )
  {
    if( is.null( Repr ) | is.null( Ability ) | is.null( se ) )
      stop( "If 'Abildf' is not supplied than 'Repr', 'Ability' and 'se' should be provided" )
    
    if( !is.vector( Repr ) | !is.character( Repr ) )
      stop( "'Repr' should be a character vector" )
    
    if( !is.vector( Ability ) | !is.numeric( Ability) )
      stop( "'Ability' should be a numeric vector" )
    
    if( !is.vector( se ) | !is.numeric( se ) )
      stop( "'se' should be a numeric vector" )
    
    if( is.null( Fixed ) )
    {
      Abildf <- data.frame( Repr, Ability, se )
      names( Abildf ) <- c( "Repr", "Ability", "se" )
    } else
    {
      if( !is.vector( Fixed ) | !is.logical( Fixed ) )
        stop( "'Fixed' shoud be a logical vector" )
      
      Abildf <- data.frame( Repr, Ability, se, Fixed )
      names( Abildf ) <- c( "Repr", "Ability", "se", "Fixed" )
    }
  } else if( !is.null( Repr ) && !is.null( Ability ) && !is.null( se ) )
  {
    errMsg <- "If 'Abildf' is provided, "
    
    if( ( length( Repr ) > 1 && length( Ability ) > 1 && length( se ) > 1 ) ||
        ( is.data.frame( Repr ) && is.data.frame( Ability ) && is.data.frame( se ) ) )
    {
      stop( paste0( errMsg, "'Repr', 'Ability' and 'se' should be a single value") )
    } else if( !is.character( Repr ) && !is.character( Ability ) && !is.character( se ) )
    {
      stop( paste0( errMsg, "'Repr', 'Ability' and 'se' should be a character value") )
    } else
    {
      AbildfOut <- Abildf[ , c( Repr, Ability, se ) ]
      names( AbildfOut ) <- c( "Repr", "Ability", "se" )
    }
    
    if( !is.null( Fixed ) && ( length( Fixed ) > 1 || is.data.frame( Fixed ) ) &&
        !is.character( Fixed ) )
    {
      stop( paste0( errMsg, "and 'Fixed' is specified, 'Fixed' shoulb be a character value") )
    } else
    {
      Abildf <- data.frame( AbildfOut, Abildf[ , Fixed ] )
      names( Abildf )[4] <- "Fixed"
    }
  } else
  {
    errMsg <- "If 'Repr', 'Ability' and 'se' are not provided, "
    if( ncol( Abildf ) < 3 )
      stop( paste0( errMgg, "Abildf should contain at least 3 columns" ) )
    
    if( !is.character( Abildf[ , 1 ] ) )
      stop( paste0( errMsg, "The first column of'Abildf' should be character" ) )
      
    if( !is.numeric( Abildf[ , 2 ]) )
      stop( paste0( errMgs, "The second column of 'Abildf' should be numeric" ) )
    
    if( !is.numeric( Abildf[ , 3 ] ) )
      stop( paste0( errMsg, "The third column of 'Abildf' should be numeric" ) )
    
    names( Abildf )[ 1:3 ] <- c( "Repr", "Ability", "se" )
    
    if( ncol( Abildf ) > 3 )
    {
      if( !is.logical( Abildf[ , 4 ] ) )
        stop( paste0( "The fourth column of 'Abildf' should be logical" ) )
      
      names( Abildf )[ 4 ] <- "Fixed"
    }
  }
  
  if( remove.inf )
  {
    Abildf <- Abildf[ !is.infinite( Abildf$Repr ), ]
  }
  
  return( Abildf )
  
}

Gseparation <- function( Abildf = NULL, Repr = NULL, Ability = NULL,
                         se = NULL, Fixed = NULL, perRepr = F, remove.inf = F,
                         reli = NULL )
{
  
  
  if( is.null( reli ) )
  {
    Abildf <- G.InputCheck( Abildf = Abildf, Repr = Repr, Ability = Ability,
                               se = se, Fixed = Fixed, remove.inf = remove.inf )
    
    if(!exists( "pop.sd" ) )
      source( "C:/Users/SVerhavert/Documents/R/functions/BTL_analyses/populationVarianceSD.R")
    
    #sd Abildf
    sd.mean <- pop.sd( Abildf$Ability )
    #rmse
    rmse <- sqrt( mean( Abildf$se^2 ) )
    
    ###ESTIMATE G (based on Pollitt, 2012)
    if( !perRepr )
    {
      #separation coefficient (G)
      G <- sd.mean/rmse
    } else
    {
      if( any( Abildf$Fixed ) )
      {
        Abildf <- subset( x = Abildf,
                          subset = !Abildf$Fixed )
      }
      
      
      G <- data.frame( Repr = Abildf$Repr,
                       G = rep( NA, times =length( Abildf$Repr ) ) )
      
      G$G <- sd.mean / Abildf$se
    }
  } else
  {
    if( !is.data.frame( reli ) ){
      G <- sqrt( 1 / ( 1 - reli ) )
    } else
    {
      G <- data.frame( Repr = reli$Repr,
                       G = sqrt( 1 / ( 1 - reli$alpha ) ) )
    }
  }
  
  return( G )
}

reliability<-function( Abildf = NULL, Repr = NULL, Ability = NULL,
                       se = NULL, Fixed = NULL, perRepr = F, remove.inf = F ) {
  
  #separation coefficient (G)
  G <- Gseparation( Abildf = Abildf, Repr = Repr, Ability = Ability, se = se,
                  Fixed = Fixed, perRepr = perRepr, remove.inf = remove.inf )

  ###ESTIMATE RELIAbility (based on Pollitt, 2012)
  
  
  if( !perRepr )
  {
    alpha <- ( G^2 - 1 ) / ( G^2 )  #correct formula
    
  } else
  {
    alpha <- ( G$G^2 - 1 ) / ( G$G^2 )  #correct formula
    
    alpha <- data.frame( Repr = G$Repr,
                         alpha = alpha )
  }
  
  return( alpha )
}