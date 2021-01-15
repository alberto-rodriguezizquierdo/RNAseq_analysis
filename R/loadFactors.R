
#' @name readFactors
#'
#' @param configFile
#' @import NOISeq
#'
#' @return myFactors
#' @author Alberto Rodriguez-Izquierdo, 2021



readFactors <- function(configFile){

  factors <- read.csv(configFile$samplesFactorFilename, sep=';')

  return(factors)
}



#' @name loadFactors
#' @param dataCounts
#' @param configFile
#' @import NOISeq
#'
#' @return myFactors
#' @author Alberto Rodriguez-Izquierdo, 2021


loadFactors <- function(factorFile,configFile){
  browser()
  myfactor1 <- dplyr::select(myFactorFile,configFile$name_samples_1)

  myfactor2 <- dplyr::select(myFactorFile,configFile$name_samples_2)

  myfactor_first  <- rbind(myfactor1,myfactor2)

  myfactor <- data.frame(myfactor_first, batch= c(rep(1,nrow(myfactor1)), rep(2,nrow(myfactor2))))

  return(myfactor)
}

