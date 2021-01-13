
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
