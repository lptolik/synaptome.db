##### Use cases 6 Extract the PPIs for specific compartment #####

#' Provide full list of compartments (pres, post, synaptosome)
#'
#' @return data.frame
#' @export
#' @import dplyr
#' @examples
#' c<-getCompartments()
getCompartments<-function(){
    gns<-get_dbconn() %>% dplyr::tbl("Localisation") %>% collect
    return(gns)
}

#' Get list of all genes found in compartment
#'
#' @param compartmentID Compartment ID
#'
#' @return data.frame
#' @export
#' @import dplyr
getAllGenes4Compartment<-function(compartmentID){
    idsC<-get_dbconn() %>% dplyr::tbl("FullGenePaper") %>%
        dplyr::filter(LocalisationID == compartmentID) %>%
        dplyr::select(GeneID) %>% dplyr::pull(GeneID) %>% unique

    return(getGenesByID(idsC))
}

#' Select genes from list that found in compartment
#'
#' @param ids Gene IDs
#' @param compartmentID compartment ID
#'
#' @return data.frame
#' @export
#' @import dplyr
getGenes4Compartment<-function(ids,compartmentID){
    idsC<-get_dbconn() %>% dplyr::tbl("FullGenePaper") %>%
        dplyr::filter(LocalisationID == compartmentID & GeneID %in% ids) %>%
        dplyr::select(GeneID) %>% dplyr::pull(GeneID) %>% unique
    return(getGenesByID(idsC))
}

#' Prepare induces network for compartment
#'
#' @param ids gene IDs
#' @param compartmentID compartment ID
#'
#' @return  tbl_lazy
#' @import dplyr
getInducedPPI4Compartment<-function(ids, compartmentID){
    cids<-getGenes4Compartment(ids,compartmentID)
    aids<-getAllGenes4Compartment(compartmentID)
    gns<-getPPIQuery() %>% dplyr::filter(A %in% cids$GeneID | B %in% cids$GeneID) %>%
        dplyr::filter(A %in% aids$GeneID & B %in% aids$GeneID)
    return(gns)
}

#' Prepare limited network for compartment
#'
#' @param ids Gene IDs
#' @param compartmentID compartment ID
#'
#' @return tbl_lazy
#' @import dplyr
getLimitedPPI4Compartment<-function(ids, compartmentID){
    cids<-getGenes4Compartment(ids,compartmentID)
    gns<-getPPIQuery() %>% dplyr::filter(A %in% cids$GeneID & B %in% cids$GeneID)
    return(gns)
}

#' Extract the PPIs for specific compartment
#'
#' @param ids gene IDs
#' @param compartmentID compartment ID
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
#' @import dplyr
getPPIbyIDs4Compartment<-function(
    ids, compartmentID,type=c('induced','limited')){
    netType<-match.arg(type)
    gns<- switch (
        netType,
        induced = getInducedPPI4Compartment(ids, compartmentID),
        limited = getLimitedPPI4Compartment(ids, compartmentID)
    )
    df <- gns %>% collect
    return(df)
}
