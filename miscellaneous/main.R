#test libraries

rm(list=ls())

libInstalling <- c('rgdal','ggmap', 'XML','methods','sp', 'validate')

is.installed <- function(paquete) is.element(paquete, installed.packages())

#Checking package installation. If not verify, install packages.

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)}
}

sapply(libInstalling, require, character.only = TRUE)

#Setting Working directories

data                  <- '$LUSTRE/results/alignment'
root                  <- "$HOME/rnaseq_processing/NOISeq"
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
debug(noiseqApp(root))
#loadingScripts   <- source(myDirectoryR,)
