##### Use cases 3 Show disease info for my list of genes#####
#' Prepare query for Disease table
#'
#' Utility furnction for construction queries.
#'
#' @return tbl_lazy
#' @importFrom dplyr tbl select filter pull collect
#' @keywords internal
getGeneDiseaseQuery <- function() {
    gns <- get_dbconn() %>%
        dplyr::tbl("FullGeneFullDisease") %>%
        dplyr::select(
            HumanEntrez,
            HumanName,
            HDOID,
            Description
        )
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
#' @return data.frame with following columns:
#' * HumanEntrez
#' * HumanName
#' * HDOID
#' * Description
#'
#' @family {Disease functions}
#' @export
#'
#' @md
#' @importFrom dplyr tbl select filter pull collect
#' @examples
#' t <- getGeneDiseaseByIDs(c(48, 585, 710))
getGeneDiseaseByIDs <- function(ids) {
    ginf <- getGeneInfoByIDs(ids) %>% dplyr::pull(HumanEntrez)
    gns <- getGeneDiseaseQuery() %>% dplyr::filter(HumanEntrez %in% ginf)
    df <- gns %>% dplyr::collect()
    return(df)
}

#' Disease information for Human Entrez IDs
#'
#' Get Human disease information (HDO provided) for the set of
#' Human Entrez IDs. Function lookups for diseases associated with
#' Human Entrez IDs and returns list of available diseases.
#'
#' @param entrez `vector` of Human Entrez gene IDs
#'
#' @return data.frame with following columns:
#' * HumanEntrez
#' * HumanName
#' * HDOID
#' * Description
#'
#' @md
#' @export
#' @family {Disease functions}
#' @importFrom dplyr tbl select filter pull collect
#' @examples
#' t <- getGeneDiseaseByEntres(c(8573, 1742, 1739)) # (95 rows)
getGeneDiseaseByEntres <- function(entrez) {
    gns <- getGeneDiseaseQuery() %>% dplyr::filter(HumanEntrez %in% entrez)
    df <- gns %>% dplyr::collect()
    return(df)
}

#' Disease information for Human Gene Names
#'
#' Get Human disease information (HDO provided) for the set of
#' Human Gene Names. Function lookups for diseases associated with
#' internal Human gene names and returns list of available diseases.
#'
#' @param names `vector` of Human gene names
#'
#' @return data.frame
#' @export
#' @md
#' @family {Disease functions}
#' @importFrom dplyr tbl select filter pull collect
#' @examples
#' t <- getGeneDiseaseByName(c("CASK", "DLG2", "DLG1")) # (115 rows)
getGeneDiseaseByName <- function(names) {
    gns <- getGeneDiseaseQuery() %>% dplyr::filter(HumanName %in% names)
    df <- gns %>% dplyr::collect()
    return(df)
}
