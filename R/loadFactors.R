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

  myfactor_first  <- rbind(myfactor1,myfactor2)

  myfactor <- data.frame(myfactor_first, batch= c(rep(1,nrow(myfactor1)), rep(2,nrow(myfactor2))))

  return(myfactor)
}

#' @name ReadNOISeqFactorsPCA
#' @param my
#' @param configFile
#' @import NOISeq
#'
#' @return myFactors
#' @author Alberto Rodriguez-Izquierdo, 2021


ReadNOISeqFactorsPCA <- function(myCounts, lengthGene, myFactor){

  myCounts2 <- myCounts

  myCounts2 <- myCounts2 + runif(nrow(myCounts2) * 4,3,5)

  mydata2   <- NOISeq::readData(data=myCounts2, length = lengthGene, factors=myFactor)


  return(mydata2)
}
