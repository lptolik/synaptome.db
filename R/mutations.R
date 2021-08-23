##### Use cases 4 Show mutations for specific disease for my list of genes#####

getMutDiseaseQuery <- function() {
    gns <- get_dbconn() %>%
        dplyr::tbl("AllMutationsAllPapers") %>%
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

getMutations4DiseaseByIDs <- function(ids, hdoid) {
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect()
    return(df)
}

getMutations4DiseaseByEntres <- function(entrez, hdoid) {
    ids <- getGeneIdByEntrez(entrez)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect()
    return(df)
}

getMutations4DiseaseByName <- function(name, hdoid) {
    ids <- getGeneIdByName(name)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df <- gns %>% dplyr::collect()
    return(df)
}
