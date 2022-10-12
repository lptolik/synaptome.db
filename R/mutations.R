##### Use cases 4 Show mutations for specific disease for my list of genes#####

#' Prepare mutation access query
#'
#' @return \code{\link[dbplyr]{tbl_sql}} of table join
#' @keywords internal
getMutDiseaseQuery <- function() {
    mtbl <- get_dbconn() %>%
        dplyr::tbl('Mutation') %>% try(silent = TRUE)
    if(inherits(mtbl,'try-error')){
        stop('This version of DB do not support mutations.')
    }
    gtbl <- get_dbconn() %>%
        dplyr::tbl('Gene')
    dtbl <- get_dbconn() %>%
        dplyr::tbl('Disease')
    mptbl <- get_dbconn() %>%
        dplyr::tbl('PaperMutation')
    ptbl <- get_dbconn() %>%
        dplyr::tbl('Paper')
    gns <- mtbl %>%
        dplyr::inner_join(gtbl, by = c('GeneID' = 'ID')) %>%
        dplyr::inner_join(dtbl, by = c('HDOID' = 'HDOID')) %>%
        dplyr::rename(Disease = Description) %>%
        dplyr::inner_join(mptbl, by = c('ID' = 'MutationID')) %>%
        dplyr::inner_join(ptbl, by = c('PMID' = 'PMID')) %>%
        dplyr::rename(Paper = Name) %>%
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
            EpilepsyGene,
            ClinVar,
            PMID,
            Paper
        )
    return(gns)
}

#' Get mutational information for Gene and Disease
#'
#' Function looks up the information for particular set of GeneIDs
#' and disease HDOID.
#'
#' This function then returns
#' following features for all found genes:
#' \itemize{
#' \item GeneID,
#' \item MGI,
#' \item MouseEntrez,
#' \item MouseName,
#' \item HumanName,
#' \item HumanEntrez,
#' \item HDOID,
#' \item Disease,
#' \item Chromosome,
#' \item Position,
#' \item Variant,
#' \item FunctionClass,
#' \item cDNAvariant,
#' \item ProteinVariant,
#' \item DENOVO,
#' \item SFARI,
#' \item EpilepsyGene,
#' \item ClinVar,
#' \item PMID,
#' \item Paper
#' }
#'
#' @param ids internal GeneIDs to filter mutation data
#' @param hdoid disease HDOID to get mutational association.
#'
#' @return \code{data.frame} with fields specified above.
#' @family {Mutation functions}
#' @export
#'
#' @examples
#' hdoid<-'DOID:0060041'
#' ids<-c(6,32,127,181,240,267,558)
#' mdf<-getMutations4DiseaseByIDs(ids, hdoid)
getMutations4DiseaseByIDs <- function(ids, hdoid) {
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect() %>% as.data.frame
    return(df)
}

#' Get mutational information for Gene and Disease
#'
#' Function looks up the information for particular set of Entrez IDs
#' and disease HDOID.
#'
#' @param entrez list of Entrez IDs for genes to select
#' @param hdoid  disease HDOID to get mutational association.
#'
#' @return \code{data.frame} as described in
#'         \code{\link{getMutations4DiseaseByIDs}}.
#' @export
#' @seealso findGenesByEntrez
#' @family {Mutation functions}
#'
#' @examples
#' hdoid<-'DOID:0060041'
#' entrez<-c("23859", "17754", "18673", "268566", "12293", "320840", "24012")
#' mdf<-getMutations4DiseaseByEntres(entrez, hdoid)
getMutations4DiseaseByEntres <- function(entrez, hdoid) {
    ids <- getGeneIdByEntrez(entrez)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect() %>% as.data.frame
    return(df)
}

#' Get mutational information for Gene and Disease
#'
#' Function lookups for provided values in Human Name, Mouse Name and
#' Rat Name columns, extract known mutations, filter requested HDOID and return
#' data.frame in the format described in
#' \code{\link{getMutations4DiseaseByIDs}}.
#'
#' @param name gene names to look for
#' @param hdoid  disease HDOID to get mutational association.
#'
#' @return \code{data.frame} as described in
#'         \code{\link{getMutations4DiseaseByIDs}}.
#' @export
#' @seealso findGenesByName
#' @family {Mutation functions}
#'
#' @examples
#' hdoid<-'DOID:0060041'
#' name<-c("Dlg2", "Map1a", "Phb", "Gphn", "Cacna2d1", "Negr1", "Rgs7")
#' mdf<-getMutations4DiseaseByName(name, hdoid)
getMutations4DiseaseByName <- function(name, hdoid) {
    ids <- getGeneIdByName(name)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect() %>% as.data.frame
    return(df)
}
