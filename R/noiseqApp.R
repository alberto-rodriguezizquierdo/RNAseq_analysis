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

  myFactors <- loadFactors(configFile)


}
