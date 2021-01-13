#' @name outputResults
#' @param data
#' @param configFile
#'
#' @import NOISeq
#' @import dplyr
#'
#' @return myData
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'


outputResults <- function(data, outputPathName,configFile){

  dirOutput <- paste0(configFile$output$outputDir,outputPathName)

  if (!dir.exists(dirOutput)){

    dir.create(dirOutput)

  }else{

    unlink(dirOutput, recursive = TRUE)

    dir.create(dirOutput)

  }

  write.table(data, file=paste0(dirOutput,'/', configFile$output$outputName), sep=';')


}
