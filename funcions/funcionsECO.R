#' @export
roundup2 <- function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' @export
kakwani <- function(var1,var2,weights=1){
  
  kakwani<-sum(((cumsum(var1*(weights/sum(weights))))/sum(var1)) - 
        ((cumsum(var2*(weights/sum(weights))))/sum(var2)))/sum((weights/sum(weights)))
  
  return(kakwani)
}

#' @export
gini <- reldist::gini

#' @export
edat <- function(birth, base = Sys.Date()){
  #funció per calcular l'edat a un moment del temps
  i <- lubridate::interval(birth, base)
  p <- lubridate::as.period(i)
  y <- lubridate::year(p)
  return(y)
}

#' @export
trobaValorAlPercentil<-function(x,factor,percentil) {
  data<-data.frame(x,factor)
  data<-data %>% 
    arrange(x) %>% 
    mutate(acumulat_factor=cumsum(factor),
           percentil_x=acumulat_factor/sum(factor))
  
  which_p<-which.min(abs(data$percentil_x - percentil))
  valor<-data$x[which_p]
  if(length(valor)==0) valor<-NA
  return(valor)
 }

#' @export
calcula_qi<-function(x,limSup,valor){
  #x: valor sobre el qual calcular la quota
  #limSup: límits superior dels trams - normalment acaba amb Infinit
  #valor: percentatge que s'aplica a cada tram
  
  qi<-rep(0,length(x))
  for(i in 1:length(valor)){

    limSup_ant<-0
    if(i>1){
      limSup_ant<-limSup[i-1]
    }

    qi_i<-fifelse(x>=limSup[i], 
                  (limSup[i]-limSup_ant)*valor[i], #si queda per sobre del tram actual
                  fifelse(x>limSup_ant, 
                          (x-limSup_ant)*valor[i], #si està al tram actual
                          0)) #trams inferiors als que evaluem
   
    qi<-qi+qi_i
  }
  
  return(qi)
}


#' @export
catalan <- list(url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/ca.json')

#' @export
pctInput <- function(id,label,value){
  shinyWidgets::autonumericInput(id, label, value=value, align='left',
                                 decimalCharacter = ',',digitGroupSeparator='.',
                                 currencySymbol='%',currencySymbolPlacement='s')
}

#' @export
formatejaTaulaDT <- function(taulaDT,columnesPct=integer(0),columnesCurrency=integer(0)) {

  if(colnames(taulaDT$x$data)[1]==' '){
    taulaDT$x$data<-taulaDT$x$data[,-1]
    #eliminar la primera columna - originalment era els rownames
  }
  classes<-taulaDT$x$data %>% summarise_all(class) %>% unlist(use.names=F)
  taulaDT %>% 
    formatCurrency(columns=which(classes=='numeric'),currency='',mark='.',dec.mark=',',digits=2) %>% 
    formatCurrency(columns=which(classes=='integer'),currency='',mark='.',dec.mark=',',digits=0) %>% 
    formatCurrency(columns=columnesPct,currency='',mark='.',dec.mark=',',digits=4) %>% 
    formatCurrency(columns=columnesCurrency,currency='€',mark='.',dec.mark=',',digits=2,before=F)
}

#' @export
installifnot <- function(pckgName){
  for(i in pckgName){
    if(!require(i, character.only=T)){
      install.packages(i,dep=T)
    }
    library(i, character.only = T, quietly = T)
  }
}

#' @export
creaTaulaTarifa <- function(finsA,tipus){
  tarifa<-data.frame(`Fins a (euros)`=roundup2(finsA,2), #cal arrodonir en aquest pas
                     Tipus=roundup2(tipus,4),check.names = F) #cal arrodonir en aquest pas 
  resta<-roundup2(tarifa$`Fins a (euros)`[-1]-(tarifa$`Fins a (euros)`[-nrow(tarifa)]),2) #cal arrodonir en aquest pas
  qi_marginal<-roundup2(resta*tarifa$Tipus[-nrow(tarifa)],2) #cal arrodonir en aquest pas
  qi<-c(0,cumsum(qi_marginal))
  resta<-c(resta,NA)
  
  tarifa<-data.frame(`Fins a (euros)`=tarifa[,1],
                     `Quota íntegra (euros)`=qi,
                     `Resta fins a (euros)`=resta,
                     Tipus=tarifa[,2],
                     check.names = F)
  return(tarifa)
}








