##### Use cases 4 Show mutations for specific disease for my list of genes#####

#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
#' @importFrom dplyr tbl select filter pull collect
getMutDiseaseQuery <- function() {
    gns <- get_dbconn() %>% dplyr::tbl("AllMutationsAllPapers") %>%
        dplyr::select(
            GeneID,
            MGI,
            MouseEntrez,
            MouseName,
            HumanName,
            HumanEntrez,
            HDOID,
            Disease,
            Chromosome,
            Position,
            Variant,
            FunctionClass,
            cDNAvariant,
            ProteinVariant,
            DENOVO,
            SFARI,
            ClinVar,
            PMID,
            Paper
        )
    return(gns)
}

#' Show mutations for specific disease by GeneIDs
#'
#' @param ids Gene IDs
#' @param hdoid HDO IDs
#'
#' @return  data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
getMutations4DiseaseByIDs<-function(ids,hdoid){
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df<-gns %>% dplyr::collect()
    return(df)
}
#' Show mutations for specific disease by Entrez IDs
#'
#' @param entrez Entrez IDs
#' @param hdoid HDO IDs
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
getMutations4DiseaseByEntres<-function(entrez,hdoid){
    ids<-getGeneIdByEntrez(entrez)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df<-gns %>% dplyr::collect()
    return(df)
}
#' Show mutations for specific disease by gene name
#'
#' @param name Gene name
#' @param hdoid HDO IDs
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
getMutations4DiseaseByName<-function(name,hdoid){
    ids<-getGeneIdByName(name)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df<-gns %>% dplyr::collect()
    return(df)
}
