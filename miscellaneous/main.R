#test libraries

rm(list=ls())

libInstalling <- c('dplyr','NOISeq')

is.installed <- function(paquete) is.element(paquete, installed.packages())

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("NOISeq")

#library("NOISeq")

#Checking package installation. If not verify, install packages.

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)
  }
  print (packages)
  library(packages,character.only=TRUE)
}


#sapply(libInstalling, require, character.only = TRUE)

#Setting Working directories

dataPath                  <- 'C:/Users/Alberto/Documents/Beebliography/essay/'
root                      <- "C:/Users/Alberto/Documents/BrewPi/brewpi-devdocs/RNAseq_analysis/"
setwd(root)
#myDirectoryData       <- paste0(root,data)
rDirectory            <- 'R/'
myDirectoryR          <- paste0(root,rDirectory)
#logsDirectory         <- 'logs/'
#myDirectoryLogs       <- paste0(root,logsDirectory)


listFiles <- list.files(myDirectoryR, pattern = ".R")
for (rscripts in listFiles){
  source(paste0(myDirectoryR,rscripts))
}

#noiseqApp(root)
debug(noiseqApp(root, dataPath))
#loadingScripts   <- source(myDirectoryR,)
