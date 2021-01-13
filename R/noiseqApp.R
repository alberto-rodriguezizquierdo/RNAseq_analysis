#' @name noiseqApp
#' @param root
#' @param datapath
#' @import dplyr
#' @author Alberto Rodriguez-Izquierdo, 2021

noiseqApp <- function(root){
  browser()

#  library(logging)

#  logging::loginfo('####--------Starting app---------------####',logger = logs)

  configFile <- getConfigFile(root)

  dataCounts <- loadData(configFile)
########---------------Processing data------------------########
####---------------------Building factors-------------------####

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  for (factors in names(bFactors)){

    eval(parse(text=paste0('myFactors',factors,' <- loadFactors(configFile$',factors,')')))

  }
####----------------Read data with NOISeq App--------------####

##---------------Calculating gene length---------------------##

  if (!file.exists(paste0(configFile$geneLength$geneLengthDir,configFile$geneLength$geneLengthFilename))){

    geneLength <- calculateGeneLength(configFile$geneLength$geneLengthAnnotation,
                                      configFile$geneLength$geneLengthExtAnnot,
                                      configFile$geneLength$geneLengthDir,
                                      configFile$geneLength$geneLengthOutputFilename)

  }else{
    geneLength <- read.csv(paste0(configFile$geneLength$geneLengthDir,configFile$geneLength$geneLengthFilename), sep= ';')
  }

####-------------Building data in NOISeq object------------####

  for (factors in names(bFactors)){

    eval(parse(text=paste0('myCounts', factors,' <- creatingCountsByFactors(dataCounts,myFactors',factors,')')))

    eval(parse(text=paste0('myData',factors,' <- ReadNOISeqFactors(myCounts',factors,',geneLength,myFactors',factors,')')))

    eval(parse(text=paste0('mySpecimen1 <- configFile$',factors,'$specimen1)')))

    eval(parse(text=paste0('mySpecimen2 <- configFile$',factors,'$specimen2)')))

    eval(parse(text=paste0('myComparison <- configFile$',factors,'$comparison)')))

    eval(parse(text=paste0(mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,' <- ProcessingNOISeqbio(myData',factors,',factors,myFactors',factors,',configFile)')))




  }
}



