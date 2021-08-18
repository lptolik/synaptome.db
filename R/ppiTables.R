##### Use cases 5 Extract the PPIs for my list of genes #####
#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
#' @importFrom dplyr tbl select filter pull collect
getPPIQuery<-function(){
    #  gns<-get_dbconn() %>% dplyr::tbl("AllPpiAllPapers") %>%
    gns<-get_dbconn() %>% dplyr::tbl("PPI") %>%
        dplyr::select(
            ID,#ppiID,
            A,
            B,
            method,
            type,
            taxID)
    return(gns)
}

#' Extract the PPIs for my list of genes defined by GeneID
#'
#' @param ids Gene IDs
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
getPPIbyIDs <- function(ids, type = c('induced', 'limited')) {
    netType <- match.arg(type)
    gns <- switch (
        netType,
        induced = getPPIQuery() %>%
            dplyr::filter(A %in% ids | B %in% ids),
        limited = getPPIQuery() %>%
            dplyr::filter(A %in% ids & B %in% ids)
    )
    df <- gns %>% collect
    return(df)
}

#' Extract the PPIs for my list of genes defined by Entrez IDs
#'
#' @param entrez Entrez IDs
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
getPPIbyEntrez<-function(entrez, type=c('induced','limited')){
    ids<-getGeneIdByEntrez(entrez)
    df<-getPPIbyIDs(ids,type)
    return(df)
}

#' Extract the PPIs for my list of genes defined by Gene name
#'
#' @param name Gene names
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
getPPIbyName<-function(name, type=c('induced','limited')){
    ids<-getGeneIdByName(name)
    df<-getPPIbyIDs(ids,type)
    return(df)
}
