#' @name ProcessingDESeqHeatMap
#' @param myData
#' @param configFile
#' @import DESeq2
#' @import PoiClaClu
#'
#' @return value_noiseqbio
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'


ProcessingDESeqHeatMap <- function(myData, configFile, dirOutput){
  browser()
  vsd <- vst(myData, blind = TRUE)
  
  sampleDists <- dist(t(assay(vsd)))
  
  sampleDistsMatrix <- as.matrix (sampleDists)
  
  if (isTRUE(configFile$deseqProperties$poisson)){
    
    poisd <- PoissonDistance(t(counts(myData)))
    
    poisdMatrix <- as.matrix(poisd$dd)
    
    colors <- colorRampPalette(rev(brewer.pal(9,'Blues')))(255)
    
    rownames(poisdMatrix) <- myData$factor
    
    colnames(poisdMatrix) <- NULL
    
    
    tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_heatMap_poisson.tiff'),units="in", width=5, height=5, res=300)
    
    pheatmap(poisdMatrix,
             clustering_distance_rows = poisd$dd,
             clustering_distance_cols = poisd$dd,
             col = colors)
    
    dev.off()
    
  }else{
    
    tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_heatMap.tiff'),units="in", width=5, height=5, res=300)
    
    pheatmap(sampleDistsMatrix, clustering_distance_rows = sampleDists, clustering_distance_cols = sampleDists, col = colors)
    
    dev.off()
    
  }

}

#' @name ProcessingDESeqPCA
#' @param myData
#' @param configFile
#' @import DESeq2
#' @import ggplot
#'
#' @return value_noiseqbio
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'

ProcessingDESeqPCA <- function(myData, configFile, dirOutput){
  
  outputPathName <- configFile$output$outputName
  
  vsd <- vst(myData, blind = TRUE)
  
  mypca <- plotPCA(vsd, intgroup=configFile, returnData == TRUE)
  
  percentVar <- round(100 * attr(mypca,'percentVar'))
  
  tiff(filename=paste0(dirOutput,'/figures/',outputPathName,'_PCA.tiff'),units="in", width=5, height=5, res=300)
  
  ggplot2(mypca, aes(x = PC1, y = PC2color = configFile)) +
    geom_point(size=3)+
    xlab(paste0('PC1: ', percentVar[1], '% variance')) +
    ylab(paste0('PC2: ', percentVar[1], '% variance')) +
    coord_fixed()
  
  dev.off()
}