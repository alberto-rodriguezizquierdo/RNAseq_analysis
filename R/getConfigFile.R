#' App structure
#' @name getConfigFile
#' @param root
#' @description App to start geoapp
#'
#' @import logging
#' @return configFile
#'
#' @author Alberto Rodriguez-Izquierdo, 2021

getConfigFile <- function(root){

  #loginfo('Reading configFile...')
  configFile <- readConfigFile(root)


  #####-------------------Validations----------------------######

  #loginfo('Validating configFile...')


  #------------------Validation nodes----------------------#

  configFile <- nodesValidation(configFile)


  ######--------------Validation nodes content --------------######

#  error <- FALSE

  #Validate generalParameters


  validateDataPath    <- validateCharacter(configFile$dataPath)

  configFile$dataPath <- validateDataPath

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  for (factors in names(bFactors)){

    validateSpecimen1     <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$specimen1)')))

    eval(parse(text=paste0('configFile$', factors,'$specimen1 <- validateSpecimen1')))

    validateSpecimen2     <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$specimen2)')))

    eval(parse(text=paste0('configFile$', factors,'$specimen2 <- validateSpecimen2')))

    validatecomparison   <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$comparison)')))

    eval(parse(text=paste0('configFile$', factors,'$comparison <- validatecomparison')))

    validateTreatment     <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$treatment)')))

    eval(parse(text=paste0('configFile$', factors,'$treatment <- validateTreatment')))

    validateNameSamples1  <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_1$name_samples)')))

    eval(parse(text=paste0('configFile$', factors,'$samples_1$name_samples <- validateNameSamples1')))

    validateNameSamples2 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_2$name_samples)')))

    eval(parse(text=paste0('configFile$', factors,'$samples_2$name_samples <- validateNameSamples2')))

    validateFactor1 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_1$factor_1)')))

    eval(parse(text=paste0('configFile$', factors,'$samples_1$factor_1 <- validateFactor1')))

    validateFactor2 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_2$factor_2)')))

    eval(parse(text=paste0('configFile$', factors,'$samples_2$factor_2 <- validateFactor2')))
    }


####-------------Validate condAnalysisNOISeqbio-------------------####

  validatenclust <- validateNumber(configFile$condAnalysisNOISeqbio$nclust)

  configFile$condAnalysisNOISeqbio$nclust <- validatenclust

  validate_k <- validateNumber(configFile$condAnalysisNOISeqbio$k)

  configFile$condAnalysisNOISeqbio$k <- validate_k

  validatenorm <- validateCharacter(configFile$condAnalysisNOISeqbio$norm)

  configFile$condAnalysisNOISeqbio$norm <- validatenorm

  validatefilter <- validateNumber(configFile$condAnalysisNOISeqbio$filter)

  configFile$condAnalysisNOISeqbio$filter <- validatefilter

  validate_r <- validateNumber(configFile$condAnalysisNOISeqbio$r)

  configFile$condAnalysisNOISeqbio$r <- validate_r

  validaterandom.seed <- validateNumber(configFile$condAnalysisNOISeqbio$random.seed)

  configFile$condAnalysisNOISeqbio$random.seed <- validaterandom.seed

####-------------------Validate geneLength------------------------####

  validategeneLengthDir <- validateCharacter(configFile$geneLength$geneLengthDir)

  configFile$geneLength$geneLengthDir <- validategeneLengthDir

  validategeneLengthFilename <- validateCharacter(configFile$geneLength$geneLengthFilename)

  configFile$geneLength$geneLengthFilename <- validategeneLengthFilename

  validategeneLengthAnnotation <- validateCharacter(configFile$geneLength$geneLengthAnnotation)

  configFile$geneLength$geneLengtAnnotation <- validategeneLengthAnnotation

  validategeneLengthExtAnnot <- validateCharacter(configFile$geneLength$geneLengtExtAnnot)

  configFile$geneLength$geneLengtExtAnnot <- validategeneLengthExtAnnot

  validategeneLengthOutputFilename <- validateCharacter(configFile$geneLength$geneLengtOutputFilename)

  configFile$geneLength$geneLengtOutputFilename <- validategeneLengthOutputFilename

####-------------------Validate degenesParam------------------------####

  validatedegenesParamq <- validateNumber(configFile$degenesParam$q)

  configFile$degenesParam$q <- validatedegenesParamq

  validatedegenesParamm <- validateCharacter(configFile$degenesParam$m)

  configFile$degenesParam$m <- validatedegenesParamm

####-------------------Validate Output------------------------####

  validateOutputName  <- validateCharacter(configFile$output$outputName)

  configFile$output$outputName <- validateOutputName

  validateOutputDir  <- validateCharacter(configFile$output$outputDir)

  configFile$output$outputDir <- validateOutputDir

  return (configFile)
}


#' App structure
#' @name readConfigFile
#' @param root
#' @description App to start geoapp
#'
#' @import logging, XML
#' @return configFile
#'
#' @author Alberto Rodriguez-Izquierdo, 2021

readConfigFile <- function(root){

  require(XML)

  myDirectoryConfigFile <- paste0(root, 'config/')

  if (!file.exists(paste0(myDirectoryConfigFile,"configFile.xml"))){

    #log_error <- 'File path does not exist! Please review the path'
    #logerror(log_error)

    #stop(log_error)
  }

  configFile <- xmlParse(file = paste0(myDirectoryConfigFile,"configFile.xml"))
  configFile <- xmlToList(configFile)
#  configFile <- data.frame(configFile)

  return (configFile)
}



#' @name nodesValidation
#' @param configFile
#' @return configFile
#'
#' @author Alberto Rodriguez-Izquierdo, 2021


nodesValidation <- function(configFile){

  #Building list with principal and secondary nodes for validation

  principalNodes          <- c('dataPath','output')

  bFactors                <- configFile[grepl("bFactor",names(configFile))]

  geneLengthNodes             <- c('geneLengthDir',
                                   'geneLengthFilename',
                                   'geneLengthAnnotation',
                                   'geneLengthExtAnnot',
                                   'geneLengthOutputFilename')

  condAnalysisNOISeqbioNodes  <- c('nclust',
                                   'k',
                                   'norm',
                                   'filter',
                                   'r',
                                   'random.seed')

  degenesParamNodes           <- c('q','m')

  outputNodes                 <- c('outputName','outputDir')

  #Validation principal nodes

  generalParametersNodes        <- validateConfigNodes (principalNodes, configFile)

  bFactorsNodes                 <- validateConfigNodes(bFactors,configFile)

  ValgeneLengthNodes            <- validateConfigNodes(geneLengthNodes, configFile$geneLength)

  ValcondAnalysisNOISeqbioNodes <- validateConfigNodes(condAnalysisNOISeqbioNodes, configFile$condAnalysisNOISeqbio)

  ValdegenesParamNodes          <- validateConfigNodes(degenesParamNodes, configFile$degenesParam)

  ValOutputNodes                <- validateConfigNodes(outputNodes, configFile$output)

  #Validation secondary nodes

  for (factors in names(bFactors)){

    factor <- eval(parse(text=paste0('configFile$',factors)))
    ##-----------------------Define name of samples------------------------##
    nameFactor1 <- strsplit(eval(parse(text=paste0('configFile$',factors, '$samples_1$name_samples'))),',')
    factor$samples_1$name_samples <- nameFactor1

    nameFactor2 <- strsplit(eval(parse(text=paste0('configFile$',factors, '$samples_2$name_samples'))),',')
    factor$samples_2$name_samples <- nameFactor2

    ##----------------------------Splitting by ','-------------------------##
    eval(parse(text=paste0('configFile$',factors,'$samples_1$name_samples <- ',factor$samples_1$name_samples)))

    eval(parse(text=paste0('configFile$',factors,'$samples_2$name_samples <- ',factor$samples_2$name_samples)))

    ##------------------------------Validate nodes--------------------------##

    bFactorsSpecimen1   <- eval(parse(text=paste0('configFile$',factors,'$specimen1')))

    bFactorsSpecimen2   <- eval(parse(text=paste0('configFile$',factors,'$specimen2')))

    bFactorscomparison <- eval(parse(text=paste0('configFile$',factors,'$comparison')))

    bFactorsTreatment   <- eval(parse(text=paste0('configFile$',factors,'$treatment')))

    bFactorsLevels1     <- eval(parse(text=paste0('configFile$',factors,'$samples_1$name_samples')))

    bFactorsLevels2     <- eval(parse(text=paste0('configFile$',factors,'$samples_2$name_samples')))

    bFactorsLevelsFact1 <- eval(parse(text=paste0('configFile$',factors,'$samples_1$factor_1')))

    bFactorsLevelsFact2 <- eval(parse(text=paste0('configFile$',factors,'$samples_2$factor_2')))

    eval(parse(text=paste0('validationNode', factors, 'specimen1 <- validateConfigNodes(bFactorsSpecimen1,configFile$', factors,'$specimen1)')))

    eval(parse(text=paste0('validationNode', factors, 'specimen2 <- validateConfigNodes(bFactorsSpecimen2,configFile$', factors,'$specimen2)')))

    eval(parse(text=paste0('validationNode', factors, 'comparison <- validateConfigNodes(bFactorscomparison,configFile$', factors,'$comparison)')))

    eval(parse(text=paste0('validationNode', factors, 'treatment <- validateConfigNodes(bFactorsTreatment,configFile$', factors,'$treatment)')))

    eval(parse(text=paste0('validationNode', factors, 'sampleName <- validateConfigNodes(bFactorsLevels1,configFile$', factors,'$samples_1$name_samples)')))

    eval(parse(text=paste0('validationNode', factors, 'sampleName <- validateConfigNodes(bFactorsLevels2,configFile$', factors,'$samples_2$name_samples)')))

    eval(parse(text=paste0('validationNode', factors, 'factor1 <- validateConfigNodes(bFactorsLevelsFact1,configFile$', factors,'$samples_1$factor_1)')))

    eval(parse(text=paste0('validationNode', factors, 'factor2 <- validateConfigNodes(bFactorsLevelsFact2,configFile$', factors,'$samples_2$factor_2)')))
  }

  return(configFile)
  #loginfo('Nodes validation success!')
}




#' App structure
#' @name validateNodes
#' @param configFile
#' @description App to start geoapp
#'
#' @import logging, XML
#' @return configFile
#'
#' @author Alberto Rodríguez Izquierdo


validateConfigNodes <- function (nodes, configFile){

  if (!is.list(nodes)){
    for (x in nodes){
      if (!(x %in% names(configFile))){
        if (!(x %in% configFile)){

#         log_error <- paste0('Node ',x,' does not exist! Please review configFile.xml')
#         logerror(log_error)
          print(paste0('Warning: Node ', nodes,' does not exist'))
#         stop(log_error)
        }
      }
    }
  }else{
    for (x in names(nodes)){
      if (!(x %in% names(configFile))){

        #       log_error <- paste0('Node ',x,' does not exist! Please review configFile.xml')
        #       logerror(log_error)
        print(paste0('Warning: Node ', nodes,' does not exist'))
        #       stop(log_error)
      }
    }
  }
}


#' App structure
#' @name ValidateCharacter
#' @param configFile
#' @description App to start geoapp
#'
#' @import logging, XML
#' @return configFile
#'
#' @author Alberto Rodríguez Izquierdo


validateCharacter <- function(configFile,
                              isNa = NULL){

  if (is.null(configFile)){
#6   log_error <- paste0('Value ', configFile, ' is null. Please check configFile')
#    logerror(log_error)

  }else{
    if (!is.null(isNa)){
      configFile <- is.character(configFile)
    }else{
      is.na(configFile)
    }
  }

  return(configFile)

}


#' App structure
#' @name ValidateNumber
#' @param configFile
#' @description App to start geoapp
#'
#' @import logging, XML
#' @return configFile
#'
#' @author Alberto Rodríguez Izquierdo



validateNumber <- function(configFile){



#  Error <- FALSE

  if (is.null(configFile)){

#    log_error <- paste0('Value ', configFile, ' is null. Please check configFile')
#    logerror(log_error)
    print('Warning: Node is null')
#    Error <- TRUE
  }else if (!as.numeric(configFile)){

#    log_error <- paste0('Value ', configFile, ' is not a number. Please check configFile')
#    logerror(log_error)
    print('Warning: Node is not numeric char')
#    Error <- TRUE
  }else{
    configFile <- as.numeric(configFile)
#  if (any(Error == TRUE)){
#    error <- Error
#    stop('Not possible to validate, please check configFile format')
#  }
  }
  return(configFile)
}

