
# ------------------------------------------------------
# get.noise
# ------------------------------------------------------
get.noise<-function( noise.list )  {
  if(length(noise.list)>0) {
    # lancia un dado
    dado <- as.integer(runif(1,1,length(noise.list)+1))
    # se è uscito il rumore Gaussiano
    if(names(noise.list)[dado]=="gaussian") {
      rumore <- rnorm(1,mean = noise.list[[dado]]$mean,sd = noise.list[[dado]]$sd)
    }
    # se è una uniforme
    if(names(noise.list)[dado]=="uniform") {
      rumore <- runif(1,min = noise.list[[dado]]$min,max = noise.list[[dado]]$max)
    }
    # distribuzione beta
    if(names(noise.list)[dado]=="beta") {
      rumore <- rbeta(1,shape = noise.list[[dado]]$shape,scale = noise.list[[dado]]$scale)
    }    
    # weibull
    if(names(noise.list)[dado]=="weibull") {
      rumore <- rweibull(1,shape = noise.list[[dado]]$shape,scale = noise.list[[dado]]$scale)
    }           
    
  } else rumore <- 0  
  return(rumore)
}



#'  Funzione per generare uno studio
#' 
#' @description  E' una funzione che genera le covariate e l'outcome, in accordo con le regole (di distorsione) passate.  L'outcome è memorizzato nella variabile Y, e si costruisce a partire da un numero di  covariate X + un eventuale bias di rumore addittivo. 
#' @param numeroCovariate (numeric) il numero di covariate
#' @param numeroCampioni.x.covariata (numeric) quanti campioni per covariata
#' @param noise.intra.X (list) il tipo di rumore che può affliggere la covariata. E' una lista di liste, che possono a loro volta essere composte come segue:
#'        \itemize{
#'              \item \code{ list("gaussian"=list("mean"= < mean value>, "sd"= <SD value>)) } please refer to nrnorm() function to see the meaning of 'mean' and 'sd' values
#'              \item \code{ list("uniform"=list("min"= < min value>, "max"= <max value>)) } please refer to runif() function to see the meaning of 'min' and 'max' values
#'              \item \code{ list("beta"=list("shape"= < shape value>, "scale"= <scale value>)) } please refer to rbeta() function to see the meaning of 'shape' and 'scale' values
#'              \item \code{ list("weibull"=list("shape"= < shape value>, "scale"= <scale value>)) } please refer to rweibull() function to see the meaning of 'shape' and 'scale' values
#'              }
#' @param noise.inter.X (list) il tipo di rumore che può affliggere la sommatoria finale. E' una lista la cui sintassi è esattamente identica a 'noise.intra.X'
#' @param intra.power.distorsion (array, default = c()) un array che indica l'esponenziale di ogni covariata. Il default (c() ) equivale a metterle tutte a 1.
#' @param inter.power.distorsion (array, default = 1) un array che indica l'esponenziale della somma dei contributi delle covariate.
#' @details La formula complessiva per il calcolo di Y è la seguente: \deqn{ y = \{noise.inter.X + \sum_{i = 1}^{numeroCovariate} [covariata(i)+noise.intra.X(i) ]^ {intra.power.distorsion}  \}^{inter.power.distorsion} }
#' dove iX.min = intra.min.power.distorsion, iX.max = intra.max.power.distorsion, eX.min = inter.min.power.distorsion e eX.max = inter.max.power.distorsion. Notare che se le distorsioni esponenziali vengono lasciate a default, la formula è perfettamente lineare.
#' 
#' @export
#' @examples \dontrun{
#' 
#' # il caso più semplice: lineare diretto di una covariata 
#' a<-generate.set()
#' plot(x = a$x$`1`, y = a$y)
#' 
#' # aggiungiamo un po' di rumore gaussiano alla covariata
#' a<-generate.set(noise.intra.X = list("gaussian"=list("mean"=0,"sd"=0.2)))
#' plot(x = a$x$`1`, y = a$y)
#' 
#' # .. o un po' di rumore a caso fra due distribuzioni gaussiane differenti ed una weibull
#' a<-generate.set(noise.intra.X = list("gaussian"=list("mean"=0,"sd"=0.2), "gaussian"=list("mean"=0.1,"sd"=0.3),"weibull"=list("shape"=1,"scale"=0) ))    
#' plot(x = a$x$`1`, y = a$y)
#' 
#' # ora una semplice monovariata con un pizzico di rumore gaussiano e esponenziale di 3.1 (per dare un po' di non linearità)
#' a<-generate.set( intra.power.distorsion=3.1 , noise.intra.X = list("gaussian"=list("mean"=0,"sd"=0.05)))
#' plot(x = a$x$`1`, y = a$y)
#' 
#' # Ovviamente possiamo fare una Y basata sulla somma di più covariate... 
#' # notate come la linearità fra x1 1 y è "abbattuta" dal peso dei coefficienti di x2
#' # (andateci piano ad elevare a potenza!)
#' a<-generate.set(numeroCovariate = 2, intra.power.distorsion=c(1,3) )
#' plot(x = a$x$`1`, y = a$y)
#' plot(x = a$x$`2`, y = a$y)
#' summary(glm(y ~ x1 + x2, data=data.frame("y"=a$y,"x1"=a$x$`1`,"x2"=a$x$`2`)))
#'  
#'  
#' } 
generate.set<-function( numeroCovariate = 1, 
                        numeroCampioni.x.covariata = 10, 
                        noise.intra.X=list(),
                        noise.inter.X=list(),
                        intra.power.distorsion = c(),
                        inter.power.distorsion = 1
                        )  {
  if(!is.list(noise.intra.X)) stop("\n\n ERRORE: 'noise.intra.X' deve essere una lista (al massimo vuota, ma una lista)")
  if(!is.list(noise.inter.X)) stop("\n\n ERRORE: 'noise.inter.X' deve essere una lista (al massimo vuota, ma una lista)")
  
  numberOfFeatures <- numeroCovariate
  campioni <- numeroCampioni.x.covariata
  x <- list()
  if(length(intra.power.distorsion)==0) intra.power.distorsion <- rep(1,numberOfFeatures)
  
  for(feature.n in seq(1,numberOfFeatures)) {
    feature.s <- as.character(feature.n)
    x[[feature.s]] <- runif(campioni,0,10)
  }
  
  # Costruisci le y
  y <- rep(0,campioni)
  
  for(campione.n in seq(1,campioni)) {
    campione.s <- as.character(campione.n)
    for(feature.n in seq(1,numberOfFeatures)) {
      feature.s <- as.character(feature.n)
      # scegli il rumore intra
      y[campione.n] <- y[campione.n] + (x[[feature.s]][campione.n] + get.noise(noise.list = noise.intra.X ))^intra.power.distorsion[feature.n] 
    }
  }
    
  # ora aggiungi i b0, ovvero il rumore addittivo finale (se indicato)
  for(campione.n in seq(1,campioni)) {
    noise.inter <- get.noise(noise.list = noise.inter.X )
    y[campione.n] <- (y[campione.n] + noise.inter)^inter.power.distorsion
  }
  
  return( list( "y" = y,
                "x" = x) ) 
}


#'  Funzione per generare dei dati di uno studio multicentrico
#' 
#' @description  E' una funzione che genera le covariate e l'outcome, ipotizzando il contributo di diversi centri (tutti con la stessa statistaica)
#' @param numero.centri (numeric) il numero di centri dello studio multicentrico da simulare
#' @param <tutti gli altri parametri> fate riferimento all'help della funzione 'generate.set()'
#' @export
#' @examples \dontrun{
#' 
#' # Multicentrico di due centri, con tre covariate lineari leggermente disturbate con una gaussiana
#' a<-genera.dati.multicentro(numero.centri = 3, noise.intra.X = list("gaussian"=list("mean"=0,"sd"=0.2)))
#'  
#' } 
genera.dati.multicentro<-function( numero.centri,
                                   numeroCovariate = 1, 
                                   numeroCampioni.x.covariata = 10, 
                                   noise.intra.X=list(),
                                   noise.inter.X=list(),
                                   intra.power.distorsion = c(),
                                   inter.power.distorsion = 1) {
  # inizializza
  dati.centro <-list()
  # per ogni centro, costruisci il dataset 
  for(i.n in seq(1,numero.centri)) {
    i.s <- as.character(i.n)
    dati.centro[[ i.s ]] <-  generate.set( numeroCovariate = numeroCovariate, 
                                              numeroCampioni.x.covariata = numeroCampioni.x.covariata, 
                                              noise.intra.X=noise.intra.X,
                                              noise.inter.X=noise.inter.X,
                                              intra.power.distorsion = intra.power.distorsion,
                                              inter.power.distorsion = inter.power.distorsion
    )
  }
  return(dati.centro)
}
