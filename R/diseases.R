##### Use cases 3 Show disease info for my list of genes#####
#' Prepare query for Disease table
#'
#' Utility furnction for construction queries.
#'
#' @return tbl_lazy
#' @importFrom dplyr tbl select filter pull collect
getGeneDiseaseQuery <- function() {
    gns <- get_dbconn() %>% dplyr::tbl("FullGeneFullDisease")  %>%
        dplyr::select(
            HumanEntrez,
            HumanName,
            HDOID,
            Description)
    return(gns)
}

#' Disease information for internal gene IDs.
#'
#' Get Human disease information (HDO provided) for the set of internal
#' gene IDs. Function lookups for diseases associated with internal
#' GeneIDs and returns list of available diseases.
#'
#' @param ids \code{vector} of gene IDs.
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
#' @examples
#' t <- getGeneDiseaseByIDs (c(48, 585, 710))
getGeneDiseaseByIDs<-function(ids){
    ginf<-getGeneInfoByIDs(ids) %>% dplyr::pull(HumanEntrez)
    gns<-getGeneDiseaseQuery() %>% dplyr::filter(HumanEntrez %in% ginf)
    df<-gns %>% dplyr::collect()
    return(df)
}

#' Get data about Gene-Disease mapping by Entres IDs
#'
#' @param entrez entrez IDs
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
#'
getGeneDiseaseByEntres<-function(entrez){
    gns<-getGeneDiseaseQuery() %>% dplyr::filter(HumanEntrez %in% entrez)
    df<-gns %>% dplyr::collect()
    return(df)
}

#' Get data about Gene-Disease mapping by  Name.
#'
#' @param names disease name
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
#'
getGeneDiseaseByName<-function(names){
    gns<-getGeneDiseaseQuery() %>% dplyr::filter(HumanName %in% names)
    df<-gns %>% dplyr::collect()
    return(df)
}
