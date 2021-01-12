#' @name loadData
#' @param dataPaht
#' @import dplyr
#' @return dataCounts

loadData <- function(configFile){
  browser()
  dirsCounts <- list.dirs(path=configFile$dataPath, full.names=FALSE )

  for (nameSample in dirsCounts){

    if (!nameSample==""){

      mappingFile <- paste0(configFile$dataPath, nameSample, '/rescounts.csv')

      csv <- read.csv(mappingFile, sep=";", header=FALSE)
      df_csv <- data.frame(csv)
      clean_df_csv <- na.omit(df_csv)

      names(clean_df_csv)[names(clean_df_csv)=='V1'] <- 'gene_id'

      if (exists("dataCounts")==FALSE){

        dataCounts <- clean_df_csv
        names(dataCounts)[names(dataCounts)=='V2'] <- nameSample

      }else{

        dataCounts  <- cbind(dataCounts, V2 = clean_df_csv$V2)
        names(dataCounts)[names(dataCounts)=='V2'] <- nameSample
      }
    }
  }

  return(dataCounts)
}
