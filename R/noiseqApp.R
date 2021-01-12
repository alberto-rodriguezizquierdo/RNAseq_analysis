#' @name noiseqApp
#' @param root
#' @param datapath
#' @import dplyr

noiseqApp <- function(root){
  browser()

#  library(logging)

#  logging::loginfo('####--------Starting app---------------####',logger = logs)

  configFile <- getConfigFile(root)

  dataCounts <- loadData(configFile)
####---------------Processing data------------------####
  browser()

####---------------Building factors-----------------####

  bFactors <- configFile[grepl("bFactor",names(configFile))]

  for (factors in names(bFactors)){

    eval(parse(text=paste0('myFactors',factors,' <- loadFactors(configFile$',factors,')')))

  }
  browser()

}
