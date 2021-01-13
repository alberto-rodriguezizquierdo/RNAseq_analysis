#' @name loadData
#' @param dataPaht
#'
#'
#' @import dplyr
#'
#' @return dataCounts
#'
#' @author Alberto Rodriguez-Izquierdo, 2021

loadData <- function(configFile){

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

#' @name creatingCountsByFactors
#' @param dataCounts
#' @param factor
#' @import dplyr
#'
#' @return selDataCounts
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'

creatingCountsByFactors <- function(dataCounts,factor){

  SelDataCounts <- dplyr::select(dataCounts,factor$sample)

#  SelDataCounts <- cbind(gene_id=dataCounts$gene_id, SelDataCounts)

  return(SelDataCounts)
}


#' @name ReadNOISeqFactors
#' @param myCounts
#' @param myFactor
#' @param lengthGene
#' @import NOISeq
#' @import dplyr
#'
#' @return myData
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'



ReadNOISeqFactors <- function(myCounts, lengthGene, myFactor){

  myData <- NOISeq::readData(data=myCounts, length = lengthGene, factors=myFactor)

  return(myData)
}


#' @name ProcessingNOISeqbio
#' @param myCounts
#' @param myFactor
#' @param lengthGene
#' @import NOISeq
#' @import dplyr
#'
#' @return value_noiseqbio
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'


ProcessingNOISeqbio <- function(data,factor_nb,factor, configFile){

#-----------------specifying variables-------------------#
  nclust_val        <- configFile$condAnalysisNOISeqbio$nclust
  k_val             <- configFile$condAnalysisNOISeqbio$k
  norm_val          <- configFile$condAnalysisNOISeqbio$norm
  factor_val        <- eval(parse(text=paste0('configFile$',factor_nb,'$treatment')))
  conditions_val    <- eval(parse(text=paste0('c(configFile$',factor_nb,'$samples_1$factor_1,configFile$',factor_nb,'$samples_2$factor_2)')))
  filter_val        <- configFile$condAnalysisNOISeqbio$filter
  r_val             <- configFile$condAnalysisNOISeqbio$r
  random.seed_val   <- configFile$condAnalysisNOISeqbio$random.seed

  value_noiseqbio   <- noiseqbio(data,
                                 nclust = nclust_val,
                                 k = k_val,
                                 norm = norm_val,
                                 factor = factor_val,
                                 conditions = conditions_val,
                                 filter = filter_val,
                                 r = r_val,
                                 random.seed = random.seed_val)

  return(value_noiseqbio)

}
