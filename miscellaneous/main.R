#test libraries

rm(list=ls())

libInstalling <- c('RColorBrewer','pheatmap','dplyr','NOISeq','DESeq2', 'XML', 'GenomicFeatures', 'BiocManager', 'tidyverse', 'PoiClaClu', 'ggplot2')

is.installed <- function(paquete) is.element(paquete, installed.packages())

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)
  }
  print (packages)
  library(packages,character.only=TRUE)
}


#Setting Working directories
root                  <- "C:/Users/178/Documents/RNAseq_analysis/"
setwd(root)
rDirectory            <- 'R/'
myDirectoryR          <- paste0(root,rDirectory)


listFiles <- list.files(myDirectoryR, pattern = ".R")
for (rscripts in listFiles){
  source(paste0(myDirectoryR,rscripts))
}

#noiseqApp(root)
debug(RNASeqAnalysisApp(root))
#loadingScripts   <- source(myDirectoryR,)
