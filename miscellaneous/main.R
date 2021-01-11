#test libraries

rm(list=ls())

libInstalling <- c('dplyr','NOISeq', 'XML')

is.installed <- function(paquete) is.element(paquete, installed.packages())

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)
  }
  print (packages)
  library(packages,character.only=TRUE)
}


#Setting Working directories
root                  <- "C:/Users/Alberto/Documents/BrewPi/brewpi-devdocs/RNAseq_analysis/"
setwd(root)
rDirectory            <- 'R/'
myDirectoryR          <- paste0(root,rDirectory)


listFiles <- list.files(myDirectoryR, pattern = ".R")
for (rscripts in listFiles){
  source(paste0(myDirectoryR,rscripts))
}

#noiseqApp(root)
debug(noiseqApp(root))
#loadingScripts   <- source(myDirectoryR,)
