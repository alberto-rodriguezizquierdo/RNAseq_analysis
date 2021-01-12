#' App structure
#' @name getConfigFile
#' @param root
#' @description App to start geoapp
#'
#' @import logging
#' @return results
#'
#' @author Alberto Rodríguez Izquierdo

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

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  for (factors in names(bFactors)){

    validateTreatment <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$treatment)')))

    validateNameSamples1 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_1$name_samples)')))

    validateNameSamples2 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_2$name_samples)')))

    validateFactor1 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_1$factor_1)')))

    validateFactor2 <- eval(parse(text=paste0('validateCharacter(configFile$', factors,'$samples_2$factor_2)')))
  }
####-------------------Validate Output------------------------####

  validateOutputPath  <- validateCharacter(configFile$output$outputName)

  validateOutputPath  <- validateCharacter(configFile$output$outputDir)

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
#' @author Alberto Rodríguez Izquierdo

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
#'


nodesValidation <- function(configFile){

  #Building list with principal and secondary nodes for validation

  principalNodes <- c('dataPath','output')

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  #Validation principal nodes

  generalParametersNodes <- validateConfigNodes (principalNodes, configFile)

  bFactorsNodes <- validateConfigNodes(bFactors,configFile)

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

    bFactorsTreatment <- eval(parse(text=paste0('configFile$',factors,'$treatment')))

    bFactorsLevels1 <- eval(parse(text=paste0('configFile$',factors,'$samples_1$name_samples')))

    bFactorsLevels2 <- eval(parse(text=paste0('configFile$',factors,'$samples_2$name_samples')))

    bFactorsLevelsFact1 <- eval(parse(text=paste0('configFile$',factors,'$samples_1$factor_1')))

    bFactorsLevelsFact2 <- eval(parse(text=paste0('configFile$',factors,'$samples_2$factor_2')))

    eval(parse(text=paste0('validationNode', factors, 'treatment <- validateConfigNodes(bFactorsTreatment,configFile$', factors,'$treatment)')))

    eval(parse(text=paste0('validationNode', factors, 'sampleName <- validateConfigNodes(bFactorsLevels1,configFile$', factors,'$samples_1$name_samples)')))

    eval(parse(text=paste0('validationNode', factors, 'sampleName <- validateConfigNodes(bFactorsLevels2,configFile$', factors,'$samples_2$name_samples)')))

    eval(parse(text=paste0('validationNode', factors, 'factor1 <- validateConfigNodes(bFactorsLevelsFact1,configFile$', factors,'$samples_1$factor_1)')))

    eval(parse(text=paste0('validationNode', factors, 'factor2 <- validateConfigNodes(bFactorsLevelsFact2,configFile$', factors,'$samples_2$factor_2)')))
  }
#  for ([bFactorsCount=='bFactor']){

#    print (names)

#  }

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
    log_error <- paste0('Value ', configFile, ' is null. Please check configFile')
    logerror(log_error)

  }else{
    if (!is.null(isNa)){
      configFile <- is.character(configFile)
    }else{
      is.na(configFile)
    }
  }

  return(configFile)

}
