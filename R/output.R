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


outputResults <- function(data, data_noiseq, outputPathName,configFile){

  dirOutput         <- paste0(configFile$output$outputDir,outputPathName)

  graph             <- configFile$output$graph

  graphTypeExpr     <- configFile$output$graphType$expr

  graphTypeMD       <- configFile$output$graphType$MD

  graphTypeBiotypes <- configFile$output$graphType$biotypes

  graphTypeq        <- configFile$output$graphType$q

  if (!dir.exists(dirOutput)){

    dir.create(dirOutput)

  }else{

    unlink(dirOutput, recursive = TRUE)

    dir.create(dirOutput)

  }

  write.table(data, file=paste0(dirOutput,'/', configFile$output$outputName), sep=';')

  if (isTRUE(graph)){

    dir.create(paste0(dirOutput,'/figures'))

    if (isTRUE(graphTypeExpr)){

      tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_expressionPlot.tiff'),units="in", width=5, height=5, res=300)

      DE.plot(data_noiseq, q = graphTypeq, graphic = "expr", log.scale = TRUE)

      dev.off()

    }

    if (isTRUE(graphTypeMD)){

      tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_MDPlot.tiff'),units="in", width=5, height=5, res=300)

      DE.plot(data_noiseq, q = graphTypeq, graphic = "MD")

      dev.off()

    }

    if (isTRUE(graphTypeBiotypes)){

      tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_BiotypesPlot.tiff'),units="in", width=5, height=5, res=300)

      DE.plot(data_noiseq, chromosomes = c(1, 2), q = graphTypeq, graphic = "distr")

      dev.off()
    }
  }


}
