##### Use cases 7 Extract the PPIs for specific brain region and specie #####

#' Get list of all Brain regions in the database
#'
#' @return data.frame
#' @export
#' @import dplyr
getBrainRegions<-function(){
    gns<-get_dbconn() %>% dplyr::tbl("BrainRegion") %>% collect
    return(gns)
}

#' Get list of all genes found in particular BrainRegion for
#' particular specie
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#'
#' @return data.frame
#' @export
#' @import dplyr
getAllGenes4BrainRegion<-function(brainRegion,taxID){
    idsC<-get_dbconn() %>% dplyr::tbl("FullGeneFullPaperFullRegion") %>%
        dplyr::filter(BrainRegion == brainRegion & SpeciesTaxID == taxID) %>%
        dplyr::select(GeneID) %>% dplyr::pull(GeneID) %>% unique
    return(idsC)
}

#' Select genes from list that found in particular BrainRegion
#' for particular specie
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return data.frame
#' @export
#' @import dplyr
getGenes4BrainRegion<-function(ids,brainRegion,taxID){
    idsC<-get_dbconn() %>% dplyr::tbl("FullGeneFullPaperFullRegion") %>%
        dplyr::filter(
            BrainRegion == brainRegion &
                SpeciesTaxID == taxID &
                GeneID %in% ids) %>%
        dplyr::select(GeneID) %>% dplyr::pull(GeneID) %>% unique
    return(idsC)
}

#' Prepare induces network for compartment
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return  tbl_lazy
#' @import dplyr
getInducedPPI4BrainRegion<-function(ids, brainRegion,taxID){
    cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
    aids<-getAllGenes4BrainRegion(brainRegion,taxID)
    gns<-getPPIQuery() %>% dplyr::filter(A %in% cids | B %in% cids) %>%
        dplyr::filter(A %in% aids & B %in% aids)
    return(gns)
}

#' Prepare limited network for compartment
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return  tbl_lazy
#' @import dplyr
getLimitedPPI4BrainRegion<-function(ids, brainRegion,taxID){
    cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
    gns<-getPPIQuery() %>% dplyr::filter(A %in% ids & B %in% ids)
    return(gns)
}
#' Extract the PPIs for specific compartment
#'
#' @param ids gene IDs
#' @param type  type of the PPI network
#' @param brainRegion  region ID
#' @param taxID  taxon ID
#'
#' @return data.frame
#' @export
#' @import dplyr
getPPIbyIDs4BrainRegion<-function(
    ids, brainRegion,taxID,type=c('induced','limited')){
    netType<-match.arg(type)
    gns<- switch (
        netType,
        induced = getInducedPPI4BrainRegion(ids, brainRegion,taxID),
        limited = getLimitedPPI4BrainRegion(ids, brainRegion,taxID)
    )
    df <- gns %>% collect
    return(df)
}

