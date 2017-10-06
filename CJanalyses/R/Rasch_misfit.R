######################################################################################################
#                                                                                                    #
#       Author: Tine van Daal  adapted by San Verhavert                                              #
#                                                                                                    #
#       Description:                                                                                 #
#         Rasch.misfit calculate the misfit of the data compared to the model.                       #
#         It returns a list with the misfits and the ranges for acceptable outfit and infit          #
#         according the the model.                                                                   #
#       gebaseerd op formules in Pollitt (2012b-artikel)                                             #
#       Arguments and options:                                                                       #
#         Input = een BTm-model of een ability dataframe                                             #
#         data = extra dataset dat ruwe data bevat en extra gegevens over vergelijkingen;            #
#                 volgende volgorde van variabelen: ronde, judge, winner, loser                      #
#         subject =  als by.judge is TRUE, de naam van de judge kolom, anders NA                     #
#         player1, player 2 = de namen van de respectivelijke kolommen met player1 en 2              #
#         score = als input is data frame, de naam van de score kolom, anders NA                     #
#         by.judge = moet misfit van judge berekend worden? TRUE= ja; FALSE= nee van script          #
#                                                                                                    #
######################################################################################################

Rasch.misfit.info <- function(){
  message( paste0( "Description: Rasch.misfit calculates the Rasch infit and ",
                  "outfit and provides their\n\trespective means and ",
                  "+/- 2sd bounds.\n\n",
                  "Arguments:\n",
                  "\tInput: BTm-model or dataframe containing abilities\n",
                  "\tdata: Dataset containing the judgements (see details)\n",
                  "\tsubject: NA (default) if by.judge is FALSE or\n",
                  "\t\t(character string) the name of the judge column ",
                  "if by.judge is TRUE\n",
                  "\tplayer1: Character string. The name of the column containing\n",
                  "\t\tthe first representation / the winner\n",
                  "\tplayer2: Character string. The name of the column containing\n",
                  "\t\tthe second representation / the loser\n",
                  "\tscore: NA (default) if Input id a BTm-model or\n\t\t(character ",
                  "string) the name of the column containing the scores\n\t\tif ",
                  "Input is a dataframe\n",
                  "\tby.judge: TRUE (default) if misfit should be caclulated by judge ",
                  "or\n\t\tFALSE if misfit should be calculated by representation") )
}

Rasch.misfit<-function( Input, data, subject=NA, player1, player2,
                        score = NA, by.Judge=T )
{
  if( !exists( "comparisonNumbers" ) )
  {
    source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\BTL_analyses\\comparisonNumbers.R")
  }
  
  if( !exists( "RaschProb" ) )
  {
    source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\BTL_analyses\\RaschProb.R" )
  }
  
  if( !exists( "FisherInfo" ) )
  {
    source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\BTL_analyses\\FisherInfo.R" )
  }
  
  ### Check if package is installed
  if( !is.data.frame( Input ) )
  {
    source( "C:\\Users\\SVerhavert\\Documents\\R\\functions\\Check_Package.R" )
    Check.Pac( "BradleyTerry2" )
    
    if( !is.factor( data[ , player1 ] ) && !is.factor( data[ , player2 ] ) )
    {
      stop( "player1 and player2 should be factors" )
    }else if( sum( levels( data[ , player1 ] ) %in% levels( data[ , player2 ] ) ) != length( levels( data[ , player1 ] ) ) )
    {
      stop( "player1 and player2 should be factors with the same levels" )
    }
    
  } else 
  {
    if( !( "Script" %in% colnames( Input ) ) &&
          !( "trueScore" %in% colnames( Input ) ) )
    {
      stop( "the data frame supplied to Input must contain columns named Script and trueScore, " )
    }
    
    if( is.na( score ) )
    {
      stop( "The input is a data frame. You should suply the name of the score column in data")
    }
    data <- data[ complete.cases( data ), ]
  }
  
  
  ### basic input checks
  if( length( player1 ) != 1 && length( player2 ) != 1 && 
        !is.character( player1 ) && !is.character( player2 ) )
  {
    stop( "player 1 and player2 should be strings indicating the names of the columns with the representations" )
  }
  
  if( by.Judge && is.na( subject ) )
  {
    stop( "If you want to aggregate by subject, you need to provide the name of the subject column" )
  }
  
  #vector met nummering aantal vergelijkingen maken (vergelijking)
  data <- data.frame( vergelijking = 1:nrow( data ), data )
  
  ### Aantal vergelijkingen tellen
  Ncomparisons <- comparisonNumbers( Data = data, Repr1 = player1,
                                     Repr2 = player2, Score = score,
                                     Judge = subject, By.judge = by.Judge )
  
  names( Ncomparisons )[1] <- "grouping"
  
  if( by.Judge )
  {
    judge.list <- list(data[ , subject ])
  }
  
  
  
  if( !is.data.frame( Input ) )
  {
    ###predicted values nemen (pred) [Rash probabilities]
    pred <- round( predict( Input, type="response" ), digits = 10 )
    
    ###informatie berekenen (pred*(1-pred))
    info <- FisherInfo( pred )
    
    ##gestandaardiseerde residuals berekenen (pearson) = pear.res
    pear.res <- round( residuals( Input, type="pearson" ), digit = 10 )
    pear.res <- as.numeric( pear.res )
  } else
  {
    
    ###predicted values nemen (pred) [Rash probabilities]
    # extract IDs of players to access ability estimates
    Scr1IDs <- match( data[ , player1 ], Input$Script )
    Scr2IDs <- match( data[ , player2 ], Input$Script )
    # calculate pred
    pred <- RaschProb( a = Input$trueScore[ Scr1IDs ] ,
                       b =  Input$trueScore[ Scr2IDs ] )
    
    ###informatie berekenen (pred*(1-pred))
    info <- FisherInfo( p = pred )
    
    ##gestandaardiseerde residuals berekenen (pearson) = pear.res
    pear.res <- ( data[ , score ] - pred ) / sqrt( info )
  }
  
  ##gekwadrateerd pearson residual (pear.res.kwad)
  pear.res.kwad <- round( pear.res^2, digit = 10 )

  ##weighted squared residual (wsr) = info*(res^2)
  wsr <- round( info * ( pear.res^2 ), digit = 10 )
  
  #GEMERGED DATABESTAND MAKEN (misfit.judge)
  ##databestand (data.misfit) maken met vergelijking, pear.res,pear.res.kwad,info,wsr
  data.misfit <- data.frame( vergelijking = data$vergelijking, pear.res,
                             pear.res.kwad, info, wsr )
  
  ##colom namen opslaan van oorspronkelijke databestand
  datanames <- colnames( data )[ colnames( data ) != "vergelijking" ]

  ###data.misfit en DPAC mergen op "vergelijking" tot 1 databestand (misfit.judge)
  misfit.df <- merge( data, data.misfit, by = "vergelijking" )
  colnames( misfit.df ) <- c( "vergelijking", datanames, "pearson_res",
                              "gekwad_pearson", "info", "wsr" )
  
  #MISFIT
  # voorbereiding
  if( !by.Judge ) #SCRIPT
  {
    ##teller voor het berekenen van outfit (gemiddelde (pearson residual²) per script)
    ###lijst met winnaars en lijst met verliezers maken
    listwinner <- list( misfit.df[ , player1 ] )
    listloser <- list( misfit.df[ , player2 ] )
    
    ###(pearson res.²) aggregreren voor elke lijst afzonderlijk (som van gekwad. pearson residual nemen voor winnende/verliezende scripts afzonderlijk)
    ## list in 'by' stays connected to the original variable it was constructed from
    winner.aggre.out <- aggregate( misfit.df$gekwad_pearson, FUN = sum,
                                   by = listwinner )
    loser.aggre.out <- aggregate( misfit.df$gekwad_pearson, FUN = sum,
                                  by = listloser )

    ###beide bestanden samenvoegen (één bestand onder ander kleven) (win.los.aggre)
    win.los.aggre.out <- rbind( winner.aggre.out, loser.aggre.out )

    ###lijst met scripts maken
    listallout <- list( win.los.aggre.out[ , 1 ] )
    
    ###aggregeren op script (totale som van (pearson residual²) per script nemen)
    all.aggre.out <- aggregate( win.los.aggre.out[ , 2 ], FUN = sum,
                                by = listallout )
    
    outteller <- all.aggre.out
    names( outteller ) <- c( "grouping", "value")
    
    ##noemer voor het berekenen van outfit (=aantal keren dat script vergeleken is) 
    outnoemer <- Ncomparisons
    names( outnoemer )[2] <- "value"
    
    ##teller voor infit berekenen (gemiddelde wsr per script)
    ###aggrereren van som van alle weighted squared residuals voor elk script (inteller)
    winner.aggre.in <- aggregate( misfit.df$wsr, FUN = sum, by = listwinner )
    loser.aggre.in <- aggregate( misfit.df$wsr, FUN = sum, by = listloser )
    
    ###beide bestanden samenvoegen (één bestand onder ander kleven) (win.los.aggre)
    win.los.aggre.in <- rbind( winner.aggre.in, loser.aggre.in )
    
    ###lijst met scripts maken
    listallin<-list( win.los.aggre.in[ , 1 ] )
    
    ###aggregeren op script (totale som van wsr per script nemen)
    all.aggre.in <- aggregate( win.los.aggre.in[,2], FUN = sum,
                               by = listallin )
    
    ###som wsr per script berekenen (=inteller)
    inteller <- all.aggre.in
    names( inteller ) <- c( "grouping", "value" )

    ##noemer voor infit berekenen (geaggregeerde info per script)
    ###aggrereren van som van de informatie voor dezelfde vergelijkingen als de teller (noemer)
    winner.aggre.info <- aggregate( misfit.df$info, FUN = sum, by = listwinner)
    loser.aggre.info <- aggregate( misfit.df$info, FUN = sum, by = listloser )
    
    ###beide bestanden samenvoegen (één bestand onder ander kleven) (win.los.aggre)
    win.los.aggre.info <- rbind( winner.aggre.info, loser.aggre.info )
    
    ###lijst met scripts maken
    listallinfo <- list( win.los.aggre.info[ , 1 ] )

    ###aggregeren op script (totale som van wsr per script nemen)
    all.aggre.info <- aggregate( win.los.aggre.info[ , 2 ] , FUN = sum,
                                 by = listallinfo )
    
    ##noemer voor de infit berekenen (som van info per script)
    innoemer <- all.aggre.info
    
    names( innoemer ) <- c( "grouping", "value" )
    
  } else #JUDGES
  {
    
    ##teller voor outfit berekenen per judge
    ###aggrereren van som van alle standardized residuals squared (pear.res.kwad) voor elke judge
    outteller <- aggregate( misfit.df$gekwad_pearson, FUN = sum, by = judge.list )
    names( outteller ) <- c( "grouping", "value")
    
    ##noemer voor outfit berekenen
    ###aantal gemaakte vergelijkingen per judge
    outnoemer <- Ncomparisons
    names( outnoemer )[2] <- "value"
    
    ##teller voor infit berekenen per judge
    ###aggrereren van som van alle weighted squared residuals (wsr) voor elke judge
    inteller <- aggregate( misfit.df$wsr, FUN = sum, by = judge.list )
    names( inteller ) <- c( "grouping", "value" )
    
    ##noemer voor infit berekenen per judge
    ###aggrereren van som van de informatie voor dezelfde vergelijkingen als de teller 
    innoemer <- aggregate( misfit.df$info, FUN = sum, by = judge.list )
    names( innoemer ) <- c( "grouping", "value" )
  }
  
  ##outfit berekenen
  outvalues <- merge( outteller, outnoemer, by = "grouping",
                   suffixes = c( ".teller", ".noemer" ) )
  outfit <- outvalues$value.teller / outvalues$value.noemer
  outfit <- data.frame( outvalues$grouping,outfit )
  names( outfit ) <- c( "grouping", "Outfit" )
  
  ##infit berekenen
  invalues <- merge( inteller, innoemer, by = "grouping",
                     suffixes = c( ".teller", ".noemer" ) )
  infit <- invalues$value.teller / invalues$value.noemer
  infit <- data.frame( invalues$grouping, infit )
  names( infit ) <- c( "grouping", "Infit" )
  
  ###merge outfit en infit data by grouping
  misfit <- merge( outfit, infit, by = "grouping" )
  misfit <- merge( misfit, Ncomparisons, by = "grouping" )
  
  if( !by.Judge ) # script
  {
    names( misfit ) <- c( "Representation", "Outfit", "Infit", "Ncomparisons" )
  } else
  {
    names( misfit ) <- c( subject, "Outfit", "Infit", "Ncomparisons" ) 
  }
  
  rownames( misfit ) <- NULL
  
  ##ranges outfit en infit berekenen
  ###gemiddelde infit berekenen (infit.mean)
  outfit.mean <- mean( misfit[ , 2 ] )
  
  ###gemiddelde infit berekenen (infit.mean)
  infit.mean <- mean( misfit[ , 3 ] )
  
  ###sd van infit berekenen (infit.sd)
  outfit.sd <- sd( misfit[ , 2 ] )
  
  ###sd van infit berekenen (infit.sd)
  infit.sd <- sd( misfit[ , 3 ] )

  ###range acceptable misfit berekenen
  ####bovengrens (outfit.mean+(2*infit.sd)) en ondergrens (outfit.mean-(2*infit.sd)) 
  outupper <- outfit.mean + ( 2 * outfit.sd )
  outlower <- outfit.mean - ( 2 * outfit.sd ) 
  
  ####bovengrens (infit.mean+(2*infit.sd)) en ondergrens (infit.mean-(2*infit.sd)) 
  inupper <- infit.mean + ( 2 * infit.sd )
  inlower <- infit.mean - ( 2 * infit.sd ) 
  
  #OUTPUT CREËREN
  ##vectoren met outfit en infit statistieken maken
  outfit.stat <- c( outfit.mean, outfit.sd, outlower, outupper ) 
  infit.stat <- c( infit.mean, infit.sd, inlower, inupper )
  
  ##data frame maken van vectoren
  fit.stat <- data.frame( outfit.stat, infit.stat )
  rownames( fit.stat ) <- c( "mean", "sd", "lower", "upper" )
  colnames( fit.stat ) <- c( "outfit", "infit" )
  
  ##output list creëren
  misfit.list <- list( misfit, fit.stat )
  misfit.list <- setNames( misfit.list, nm = c( "misfit.data", "misfit statistics" ) )
  
  
  return(misfit.list)
  
}