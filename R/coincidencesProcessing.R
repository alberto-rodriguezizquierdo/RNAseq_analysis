#' @name loadFactors
#' @param dataNoiseq
#' @param dataDeseq
#' @param dataCounts 
#' @return dataCoincidences
#' @author Alberto Rodriguez-Izquierdo, 2021


coincidences <- function(dataNoiseq, dataDeseq, dataCounts, configFile){
  
  #Selection coincidences between NOISeq and DESeq2
  
  dataNoiseq$gene_id <- row.names(dataNoiseq)
  
  dataDeseq$gene_id <- dataDeseq@rownames
  
  dataDeseq <- data.frame(dataDeseq)
  
  dataCounts$gene_id <- row.names(dataCounts)
  
  coincidencesDataAnalysis <- merge(dataNoiseq,dataDeseq, by = "gene_id")
  
  #Selection coincidences in raw data
  
  rawdataCoincidences <- merge(coincidencesDataAnalysis, dataCounts, by = "gene_id")
  
  filtered_rawdataCoincidences <- rawdataCoincidences[rawdataCoincidences$prob > 0.95 &
                                                        (rawdataCoincidences$log2FC < -2 | rawdataCoincidences$log2FC > 2) &
                                                        (rawdataCoincidences$log2FoldChange < -2 | rawdataCoincidences$log2FoldChange > 2) &
                                                        rawdataCoincidences$padj < 0.05 &
                                                        !is.na(rawdataCoincidences$padj),]
  
  
  return(rawdataCoincidences)
}