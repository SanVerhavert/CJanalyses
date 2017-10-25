################################# Scale Separation Reliability (SSR) #################################

reliability<-function( Abildf = NULL ) {
  obsSD <- pop.sd( Abildf$Ability )
  RMSE <- sqrt( mean( Abildf$se^2 ) )
  
  G <- obsSD / RMSE
  
  ( G^2 - 1 ) / ( G^2 )
}

