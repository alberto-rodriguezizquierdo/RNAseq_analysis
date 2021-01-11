#' @name loadData
#' @param dataPaht
#' @import dplyr
#' @return dataCounts

loadData <- function(dataPath){
  dirsCounts <- list.dirs(path=dataPath, full.names=FALSE )

  for (nameSample in dirsCounts){

    if (!nameSample==""){

      mappingFile <- paste0(dataPath, nameSample, '/rescounts.csv')

      csv <- read.csv(mappingFile, sep=";", header=FALSE)
      df_csv <- data.frame(csv)
      clean_df_csv <- na.omit(df_csv)

      names(clean_df_csv)[names(clean_df_csv)=='V1'] <- 'gene_id'
      names(clean_df_csv)[names(clean_df_csv)=='V2'] <- nameSample

      if (exists("dataCounts")==FALSE){

        dataCounts <- clean_df_csv

      }else{

        dataCounts <- cbind(dataCounts, clean_df_csv$nameSample)

      }
    }
  }
  return(dataCounts)
}
