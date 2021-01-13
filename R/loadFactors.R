#' @name loadFactors
#' @param dataCounts
#' @param configFile
#' @import NOISeq
#'
#' @return myFactors
#' @author Alberto Rodriguez-Izquierdo, 2021


loadFactors <- function(configFile){

  myfactor1 <- eval(parse(text=paste0('data.frame(sample=configFile$samples_1$name_samples,',configFile$treatment,'=configFile$samples_1$factor_1)')))

  myfactor2 <- eval(parse(text=paste0('data.frame(sample=configFile$samples_2$name_samples,',configFile$treatment,'=configFile$samples_2$factor_2)')))

  myfactor  <- rbind(myfactor1,myfactor2)

  return(myfactor)
}
