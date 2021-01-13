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


outputResults <- function(data, data_noiseq, outputPathName,configFile, graph=FALSE){

  dirOutput <- paste0(configFile$output$outputDir,outputPathName)

  if (!dir.exists(dirOutput)){

    dir.create(dirOutput)

  }else{

    unlink(dirOutput, recursive = TRUE)

    dir.create(dirOutput)

  }

  write.table(data, file=paste0(dirOutput,'/', configFile$output$outputName), sep=';')

  if (isTRUE(graph)){

    tiff(filename=paste0(dirOutput,'/',outputPathName,'_expressionPlot.tiff'),units="in", width=5, height=5, res=300)

    DE.plot(data_noiseq, q = 0.9, graphic = "expr", log.scale = TRUE)

    dev.off()

    tiff(filename=paste0(dirOutput,'/',outputPathName,'_MDPlot.tiff'),units="in", width=5, height=5, res=300)

    DE.plot(data_noiseq, q = 0.9, graphic = "MD")

    dev.off()

#    tiff(filename=paste0(dirOutput,'/',outputPathName,'_BiotypesPlot.tiff'),units="in", width=5, height=5, res=300)

#    DE.plot(data_noiseq, chromosomes = c(1, 2), q = 0.9, graphic = "distr")

#    dev.off()
  }


}
