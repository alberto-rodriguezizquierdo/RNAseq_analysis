#' @name noiseqApp
#' @param root
#' @param datapath
#' @import dplyr
#' @author Alberto Rodriguez-Izquierdo, 2021

RNASeqAnalysisApp <- function(root){


#  library(logging)

#  logging::loginfo('####--------Starting app---------------####',logger = logs)

  configFile <- getConfigFile(root)

  dataCounts <- loadData(configFile)
########---------------Processing data------------------########
####---------------------Building factors-------------------####

  factorFile <- readFactor(configFile)

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  for (factors in names(bFactors)){

    eval(parse(text=paste0('myFactors',factors,' <- loadFactors(factorFile,configFile$',factors,')')))

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

    if (isTRUE(configFile$analysis$noiseq)){

      ##---------------------Loading data from Factors------------------------##

      eval(parse(text=paste0('myCounts', factors,' <- creatingCountsByFactorsNoiseq(dataCounts,myFactors',factors,')')))

      eval(parse(text=paste0('myData',factors,' <- ReadNOISeqFactors(myCounts',factors,',geneLength,myFactors',factors,')')))

      eval(parse(text=paste0('myData',factors,'_2 <- ReadNOISeqFactorsPCA(myCounts',factors,',geneLength,myFactors',factors,')')))

      ##--------------Define variables to put into the analysis---------------##

#      eval(parse(text=paste0('mySpecimen1 <- configFile$',factors,'$specimen1')))

#      eval(parse(text=paste0('mySpecimen2 <- configFile$',factors,'$specimen2')))

#      eval(parse(text=paste0('myComparison <- configFile$',factors,'$comparison')))

      ##--------------Processing data and calculating DEGenes-----------------##

#      eval(parse(text=paste0(mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,' <- ProcessingNOISeqbio(myData',factors,',factors,myFactors',factors,',configFile)')))

#      eval(parse(text=paste0(mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,'.deg <- calculateDEGenes(',mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,',configFile)')))

      ##----------------------------Output Results----------------------------##

      outputPathName <- paste0(mySpecimen1,'_vs_', mySpecimen2,'_', myComparison)

      eval(parse(text=paste0('outputResults(',mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,'.deg,',mySpecimen1,'_vs_', mySpecimen2,'_', myComparison,',myData',factors,'_2, outputPathName, configFile, configFile$',factors,'$treatment)')))


    }

    if (isTRUE(configFile$analysis$deseq)){

      eval(parse(text=paste0('myCounts', factors,' <- creatingCountsByFactorsDeseq(dataCounts,myFactors',factors,')')))

      eval(parse(text=paste0('myData', factors,' <- ReadDeseqFactors(myCounts',factors,',myFactors',factors,', configFile, factors)')))
      
      usedFactor <- eval(parse(text= paste0('configFile$', factors,'$factor')))
      
      factorsToDeseq <- eval(parse(text=paste0('unique(myFactors',factors,'$',usedFactor,')')))
      
      factorsToDeseq <- append(usedFactor,factorsToDeseq)
      
      eval(parse(text=paste0('deseqResults',factors,' <- processingDeseq(myData',factors,', configFile, factorsToDeseq)')))
      browser()
      if (eval(parse(text=paste0('(length(unique(myFactors',factors,'$strain)))==1')))){
        
        outputSpecimen <- eval(parse(text=paste0('unique(myFactors',factors,'$strain)')))
        
      }else{
        
        outputSpecimen <- eval(parse(text=paste0('paste(unique(myFactors',factors,'$strain), sep="_vs_")')))
        
      }
      if (eval(parse(text=paste0('(length(unique(myFactors',factors,'$factor)))==1')))){
        
        outputfactor <- eval(parse(text=paste0('unique(myFactors',factors,'$factor)')))
        
      }else{
        
        factorvalues <- eval(parse(text=paste0('unique(myFactors',factors,'$factor)')))
        
        outputfactor <- paste(factorvalues[[1]],factorvalues[[2]], sep = "_vs_")
        
      }
      if (eval(parse(text=paste0('(length(unique(myFactors',factors,'$time)))==1')))){
        
        outputtime <- eval(parse(text=paste0('unique(myFactors',factors,'$time)')))
        
      }else{
        
        outputtime <- eval(parse(text=paste0('paste(unique(myFactors',factors,'$time), sep="_vs_")')))
        
      }
      
      if (eval(parse(text=paste0('(length(unique(myFactors',factors,'$treatment)))==1')))){
        
        outputtreatment <- eval(parse(text=paste0('unique(myFactors',factors,'$treatment)')))
        
      }else{
        
        outputtreatment <- eval(parse(text=paste0('paste(unique(myFactors',factors,'$treatment), sep="_vs_")')))
        
      }
      
      if (eval(parse(text=paste0('(length(unique(myFactors',factors,'$organ)))==1')))){
        
        outputorgan <- eval(parse(text=paste0('unique(myFactors',factors,'$organ)')))
        
      }else{
        
        outputorgan <- eval(parse(text=paste0('paste(unique(myFactors',factors,'$organ), sep="_vs_")')))
        
      }
      
      outputPathName <- paste0(outputSpecimen,'_',outputfactor,'_',outputtime,'_',outputtreatment,'_',outputorgan)
      
      eval(parse(text=paste0('outputResultsDESeq()')))
    }
  }

  print('App finished successfully!')
}



