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
  browser()
  #loginfo('Reading configFile...')
  configFile <- readConfigFile(root)


  #####-------------------Validations----------------------######

  #loginfo('Validating configFile...')


  #------------------Validation nodes----------------------#

  nodesValidation(configFile)


  ######--------------Validation nodes content --------------######

#  error <- FALSE

  #Validate generalParameters


  #validateGeneralParameters <- validateCharacter(configFile$fileName)

#  browser()

#  validateGeneralParameters <- configFile$fileParameters

#  validationFileEncoding    <- validateNumber (validateGeneralParameters$fileEncoding)

#  validationFileName        <- validateCharacter (validateGeneralParameters$fileName)

  ###---Final getConfigFile


#  if (any(error == TRUE)){
#    stop('Not possible to validate, please check configFile format')
#  }

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

  browser()

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

nodesValidation <- function(configFile){

  #Building list with principal and secondary nodes for validation

  principalNodes <- c('dataPath','output')

  fileParameters <- c('fileEncoding', 'fileName')

  dataNodes <- c('ons_label', 'name')

  #Validation principal nodes

  generalParametersNodes <- validateConfigNodes (principalNodes, configFile)

  #Validation secondary nodes
  browser()

  bFactors <- configFile[grepl("bFactor",names(configFile))]
  for (factors in names(bFactors)){

    factor <- eval(parse(text=paste0('configFile$',factors)))

    #paste0('configFile$',factors,'$samples_1$name_samples')
    nameFactor1 <- strsplit(eval(parse(text=paste0('configFile$',factors, '$samples_1$name_samples'))),',')
    factor$samples_1$name_samples <- nameFactor1

    nameFactor2 <- strsplit(eval(parse(text=paste0('configFile$',factors, '$samples_2$name_samples'))),',')
    factor$samples_2$name_samples <- nameFactor2

    configFile_ext <- paste0('configFile$',factors,'$',factor)

    eval(parse(text=paste0('configFile$',factors,'$samples_1$name_samples <- ',factor$samples_1$name_samples)))

    eval(parse(text=paste0('configFile$',factors,'$samples_2$name_samples <- ',factor$samples_2$name_samples)))
  }
#  for ([bFactorsCount=='bFactor']){

#    print (names)

#  }

  fileParametersNodes <- validateConfigNodes(fileParameters, configFile$fileParameters)

  dataNodes <- validateConfigNodes(dataNodes, configFile$data)

  loginfo('Nodes validation success!')
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

  for (x in nodes){
    if (!(x %in% names(configFile))){

#      log_error <- paste0('Node ',x,' does not exist! Please review configFile.xml')
#      logerror(log_error)

      stop(log_error)
    }
  }
}

