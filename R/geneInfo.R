##### Use cases 1 and 2 Show my favourite gene info#####
#' Get gene information for set of gene names.
#'
#' Function lookup for name in Human Gene name, Rat Gene name and
#' Mouse Gene name data and return following features for all found genes:
#' GeneID (internal database ID), Localisation (one of the following:
#' presynaptic, postsynaptic, synaptosome),
#' MGI (MGI ID), HumanEntrez (Human Entrez ID), MouseEntrez (Mouse Entrez ID),
#' HumanName (Human gene name), MouseName (Mouse gene name),
#' PaperPMID (PMID IDs for the publications where the genes were reported),
#' Paper (papers where specific genes were reported in a format
#' FIRSTAUTHOR_YEAR), Year, SpeciesTaxID (specie the original experiment
#' was performed on), BrainRegion (Brain region where the specific genes
#' were identified, according to the paper)
#'
#' This function then returns
#' following features for all found genes:
#' \itemize{
#' \item GeneID,
#' \item Localisation,
#' \item MGI,
#' \item HumanEntrez,
#' \item MouseEntrez,
#' \item HumanName,
#' \item MouseName,
#' \item PaperPMID,
#' \item Paper,
#' \item Year,
#' \item SpeciesTaxID,
#' \item BrainRegion
#' }
#'
#' @param name \code{vector} of gene names
#'
#' @return \code{data.frame} with fields specified above.
#' @export
#' @family {GeneInfo functions}
#'
#' @examples
#' # get information for specific gene
#' t <- getGeneInfoByName("CASK")
#'
#' # get information for the list of genes
#' t <- getGeneInfoByName(c("CASK", "DLG2"))
getGeneInfoByName <- function(name) {
    ids <- getGeneIdByName(name)
    df <- getGeneInfoByIDs(ids)
    return(df)
}


#' Get gene information for set of genes mentioned by certain papers.
#'
#' Function lookup for specified PubMedIDs in the gene reference data and
#' return following features for genes referenced by requested papers at
#' least \code{cnt} times:
#' GeneID (internal database ID), Localisation (one of the following:
#' presynaptic, postsynaptic, synaptosome),
#' MGI (MGI ID), HumanEntrez (Human Entrez ID), MouseEntrez (Mouse Entrez ID),
#' HumanName (Human gene name), MouseName (Mouse gene name),
#' PaperPMID (PMID IDs for the publications where the genes were reported if
#' it is within \code{pmids} list),
#' Paper (papers where specific genes were reported in a format
#' FIRSTAUTHOR_YEAR), Year, SpeciesTaxID (specie the original experiment
#' was performed on), BrainRegion (Brain region where the specific genes
#' were identified, according to the paper)
#'
#' This function then returns
#' following features for all found genes:
#' \itemize{
#' \item GeneID,
#' \item Localisation,
#' \item MGI,
#' \item HumanEntrez,
#' \item MouseEntrez,
#' \item HumanName,
#' \item MouseName,
#' \item PaperPMID,
#' \item Paper,
#' \item Year,
#' \item SpeciesTaxID,
#' \item BrainRegion
#' }
#'
#' @param pmids vector of PMIDs to search for genes
#' @param cnt minimal number of papers that mentioned gene
#'
#' @return \code{data.frame} with fields specified above.
#' @export
#' @family {GeneInfo functions}
#'
#' @examples
#' res<- getAllGenes4Compartment(compartmentID = 1)
#' gnt<-getGeneInfoByIDs(res$GeneID)
#' pmids<-names(sort(table(gnt$PaperPMID))[1:5])
#' cntT <- getGeneInfoByPapers(pmids,cnt=3)
#' head(cntT)
getGeneInfoByPapers <- function(pmids,cnt=1) {
    ids <- getGeneIdByPapers(pmids,cnt)
    df <- getGeneInfoByIDs(ids$GeneID) %>% dplyr::filter(PaperPMID %in% pmids)
    return(df)
}

#' Gene information for given list of gene Entrez IDs
#'
#' Get gene information for set of gene Entrez IDs. Function lookup for
#' name in Human Entrez ID and Mouse Entrez Id  data and return following
#' features for all found genes: GeneID (internal database ID), Localisation
#' (presynaptic, postsynaptic, synaptosome), MGI (MGI ID),
#' HumanEntrez (Human Entrez ID), MouseEntrez (Mouse Entrez ID),
#' HumanName (Human gene name), MouseName (Mouse gene name),
#' PaperPMID (PMID IDs for the publications where the genes were reported),
#' Paper (papers where specific genes were reported in a format
#' FIRSTAUTHOR_YEAR), Year, SpeciesTaxID (specie the original experiment
#' was performed on), BrainRegion (Brain region where the specific genes
#' were identified, according to the paper)
#'
#' @param entrez \code{vector} of Entres IDs. Function accepts both
#' integers and characters.
#'
#' @return  \code{data.frame}  with fields specified above.
#' @export
#' @family {GeneInfo functions}
#'
#' @examples
#' # get information for specific gene
#' t <- getGeneInfoByEntrez(1742)
#' # get information for specific character string Entres representation
#' t <- getGeneInfoByEntrez("1742")
#'
#' # get information for the list of genes
#' t <- getGeneInfoByName(c(1741, 1742, 1739, 1740))
getGeneInfoByEntrez <- function(entrez) {
    ids <- getGeneIdByEntrez(entrez)
    df <- getGeneInfoByIDs(ids)
    return(df)
}

#' Internal Gene representation for given list of gene Entrez IDs
#'
#' Get internal gene representation for set of gene Entrez IDs.
#' Function lookups for provided values in Human Entrez ID, Mouse Entrez ID
#' and Rat Entrez ID columns and return following features for all found
#' genes: GeneID (internal database ID), MGI ID, Human Entrez ID, Mouse
#' Entrez ID, Rat Entrez ID, Human gene name, Mouse gene name and Rat
#' gene name.
#'
#' Could be used as an intermediate step for building Protein-Protein
#' interaction map from the list of Gene IDs returned in the first column.
#' Also, this function provides a useful sanity check, e.g. how many Gene IDs
#' correspond to the
#' specific gene name or Entrez ID, which could be specie-specific.
#'
#' @param entrez \code{vector} of Entres IDs. Function accepts both
#' integers and characters.
#'
#' @return  \code{data.frame} with columns specified above.
#' @export
#' @importFrom dplyr tbl select filter pull collect
#' @family {Lookup functions}
#' @family {Gene functions}
#'
#'
#' @md
#' @examples
#' # get information for specific gene
#' t <- findGenesByEntrez(c(1742, 1741, 1739, 1740))
findGenesByEntrez <- function(entrez) {
    ids <- getGeneIdByEntrez(entrez)
    return(getGenesByID(ids))
}

#' Get list of GeneIDs corresponding to provided Entrez IDs.
#'
#' Get internal GeneID values for set of gene Entrez IDs. Function
#' lookups for provided values in Human Entrez ID, Mouse Entrez ID and
#' Rat Entrez ID columns and returns obtained GeneIDs.
#'
#' @param entrez \code{vector} of Entres IDs. Function accepts both
#' integers and characters.
#'
#' @return \code{vector} of GeneID values.
#'
#' @keywords internal
#' @examples
#' t <- synaptome.db:::getGeneIdByEntrez(c(1742, 1741, 1739, 1740))
getGeneIdByEntrez <- function(entrez) {
    idsH <- get_dbconn() %>%
        dplyr::tbl("Gene") %>%
        dplyr::filter(
            HumanEntrez %in% entrez |
                MouseEntrez %in% entrez |
                RatEntrez %in% entrez
        ) %>%
        dplyr::select(ID) %>%
        dplyr::pull(ID) %>%
        unique()
    return(idsH)
}

#' Find GeneIDs for names
#'
#' Get internal gene representation for set of gene names. Function lookups
#' for provided values in Human Name, Mouse Name and Rat Name columns and
#' return following features for all found genes: GeneID (internal database
#' ID), MGI ID, Human Entrez ID, Mouse Entrez ID, Rat Entrez ID, Human gene
#' name, Mouse gene name and Rat gene name.
#'
#' Could be used as an intermediate step for building Protein-Protein
#' interaction map from the list of Gene IDs returned in the first column.
#' Also, this function provides a useful sanity check, e.g. how many Gene
#' IDs correspond to the specific gene name or Entrez ID, which could be
#' specie-specific.
#'
#'
#' @param name \code{vector} of gene names.
#'
#' @return  \code{data.frame} with columns specified above.
#' @export
#' @importFrom dplyr tbl select filter pull collect
#' @family {Lookup functions}
#' @family {Gene functions}
#' @examples
#' # Find GeneIDs for names
#' t <- findGenesByName(c("Src", "Srcin1", "Fyn"))
findGenesByName <- function(name) {
    ids <- getGeneIdByName(name)
    return(getGenesByID(ids))
}

#' Get gene table from list of GeneIDs.
#'
#' Takes internal gene IDs as input and return the following features for
#' all found genes:
#' \describe{
#' \item{GeneID}{ internal database ID}
#' \item{MGI}{ MGI ID}
#' \item{HumanEntrez}{Human Entrez ID}
#' \item{MouseEntrez}{Mouse Entrez ID}
#' \item{HumanName}{Human gene name}
#' \item{MouseName}{Mouse gene name}
#' \item{RatEntrez}{Rat Entrez ID}
#' \item{Rat Name}{Rat gene name}
#' }
#'
#' @param ids \code{vector} of GeneID values.
#'
#' @return \code{data.frame} with 8 columns specified above.
#'
#' @importFrom dplyr tbl select filter rename collect
#' @family {Gene functions}
#' @export
#' @examples
#' gdf <- getGenesByID(c(46, 6, 15, 1))
getGenesByID <- function(ids) {
    #TODO: fix the DB and remove suppressWarnings
    genes <- suppressWarnings(
        get_dbconn() %>%
        dplyr::tbl("Gene") %>%
        dplyr::select(
            ID, MGI,
            HumanEntrez, MouseEntrez, RatEntrez,
            HumanName, MouseName, RatName
        ) %>%
        dplyr::filter(ID %in% ids) %>%
        dplyr::rename(GeneID = ID) %>%
        dplyr::collect()
    )
    return(genes)
}

#' Get list of GeneIDs corresponding to provided gene names.
#'
#'
#' @param name \code{vector} of gene names.
#'
#' @return \code{vector} of GeneID values.
#'
#' @keywords internal
#' @examples
#' t <- synaptome.db:::getGeneIdByName(c("Src", "Srcin1", "Fyn"))
getGeneIdByName <- function(name) {
    idsH <- get_dbconn() %>%
        dplyr::tbl("Gene") %>%
        dplyr::filter(
            HumanName %in% name |
                MouseName %in% name |
                RatName %in% name
        ) %>%
        dplyr::select(ID) %>%
        dplyr::pull(ID) %>%
        unique()
    return(idsH)
}

#' Get list of GeneIDs for genes found in specified papers
#'
#' @param pmids vector of PMIDs to search for genes
#' @param cnt minimal number of papers that mentioned gene
#'
#' @return tibble wiht GeneID and Npmid columns for genes and paper count
#'         data respectively.
#' @keywords internal
#'
#' @examples
#' res<- getAllGenes4Compartment(compartmentID = 1)
#' gnt<-getGeneInfoByIDs(res$GeneID)
#' pmids<-names(sort(table(gnt$PaperPMID))[1:5])
#' cntT<-synaptome.db:::getGeneIdByPapers(pmids,3)
getGeneIdByPapers <- function(pmids, cnt = 1) {
    if (length(pmids) < 1) {
        stop(
            'At least one paper should be specified.\n',
            'To search in all papers use getGeneIdByPaperCnt instead.\n'
        )
    }
    if (!is.numeric(cnt)) {
        stop('Count shauld be natural number.\n')
    }
    if (length(cnt) > 1) {
        cnt <- cnt[1]
        warning("Count should be a single value. First element is used.\n")
    }
    if (cnt < 1) {
        stop('Count shauld be natural number. (', cnt, ')\n')
    }
    idsCnt <- get_dbconn() %>%
        dplyr::tbl('PaperGene') %>%
        dplyr::filter(PaperPMID %in% pmids) %>%
        dplyr::group_by(GeneID) %>%
        dplyr::summarise(Npmid = n_distinct(PaperPMID)) %>%
        dplyr::filter(Npmid >= cnt) %>%
        dplyr::collect()
    return(idsCnt)
}

#' Get list of frequently found GeneIDs
#'
#' @param cnt minimal number of papers that mentioned gene
#'
#' @return tibble wiht GeneID and Npmid columns for genes and paper count
#'         data respectively.
#' @keywords internal
#'
#' @examples
#' cntT<-synaptome.db:::getGeneIdByPaperCnt(47)
getGeneIdByPaperCnt <- function(cnt=1) {
    if(!is.numeric(cnt)){
        stop('Count shauld be natural number.\n')
    }
    if(length(cnt)>1){
        cnt<-cnt[1]
        warning("Count should be a single value. First element is used.\n")
    }
    if(cnt < 1){
        stop('Count shauld be natural number. (',cnt,')\n')
    }
    idsCnt <- get_dbconn() %>%
        dplyr::tbl('PaperGene') %>%
        dplyr::group_by(GeneID) %>%
        dplyr::summarise(Npmid=n_distinct(PaperPMID)) %>%
        dplyr::filter( Npmid>=cnt) %>%
        dplyr::collect()
    return(idsCnt)
}

#' Get synaptome papers overview
#'
#' @return data.frame with following columns:
#' \itemize{
#' \item PaperPMID
#' \item SpeciesTaxID
#' \item Year
#' \item Name
#' \item Localisation
#' \item BrainRegion
#' \item Method
#' \item Ngenes
#' }
#' @export
#'
#' @examples
#' p <- getPapers()
#' head(p)
getPapers <- function() {
    p <- get_dbconn() %>%
        dplyr::tbl('Paper') %>%
        dplyr::select(PMID, Year, Name)
    b <- get_dbconn() %>%
        dplyr::tbl("BrainRegion") %>%
        dplyr::select(ID, Name) %>%
        dplyr::rename("BrainRegion" = 'Name')
    c <- get_dbconn() %>%
        dplyr::tbl("Localisation") %>%
        dplyr::select(ID, Name) %>%
        dplyr::rename("Localisation" = 'Name')
    m <- get_dbconn() %>%
        dplyr::tbl("Method") %>%
        dplyr::select(ID, Name) %>%
        dplyr::rename("Method" = 'Name')

    papers <- get_dbconn() %>%
        dplyr::tbl('PaperGene') %>%
        dplyr::group_by(PaperPMID,
                        #Dataset,
                        SpeciesTaxID,
                        BrainRegionID,
                        LocalisationID,
                        MethodID) %>%
        dplyr::summarise(Ngenes = n_distinct(GeneID)) %>%
        dplyr::inner_join(p, by = c('PaperPMID' = 'PMID')) %>%
        dplyr::inner_join(b, by = c('BrainRegionID' = 'ID')) %>%
        dplyr::inner_join(c, by = c('LocalisationID' = 'ID')) %>%
        dplyr::inner_join(m, by = c('MethodID' = 'ID')) %>%
        dplyr::collect()
    papers <- papers %>% as.data.frame %>%
        dplyr::select(PaperPMID,
                      SpeciesTaxID,
                      Year,
                      Name,
                      Localisation,
                      BrainRegion,
                      Method,
                      Ngenes)
    return(papers)
}

#' Get list of frequently found in `Compartment GeneIDs
#'
#' @param cnt minimal number of papers that mentioned gene
#'
#' @return tibble wiht GeneID, LocalisationID, and Npmid
#'         columns for genes and paper count
#'         data respectively.
#' @keywords internal
#'
#' @examples
#' cntT<-synaptome.db:::getGeneIdByCompartmentPaperCnt(4)
getGeneIdByCompartmentPaperCnt <- function(cnt=1) {
    if(!is.numeric(cnt)){
        stop('Count shauld be natural number.\n')
    }
    if(length(cnt)>1){
        cnt<-cnt[1]
        warning("Count should be a single value. First element is used.\n")
    }
    if(cnt < 1){
        stop('Count shauld be natural number. (',cnt,')\n')
    }
    idsCnt <- get_dbconn() %>%
        dplyr::tbl('PaperGene') %>%
        dplyr::group_by(GeneID,LocalisationID) %>%
        dplyr::summarise(Npmid=n_distinct(PaperPMID)) %>%
        dplyr::filter( Npmid>=cnt) %>%
        dplyr::collect()
    return(idsCnt)
}

#' Get gene table of frequently found genes
#'
#' Get gene table and paper count for genes mentioned \code{cnt}
#' or more times in different papers.
#'
#' @param cnt  minimal number of papers that mentioned gene
#'
#' @return \code{data.frame} with 9 columns: 8 specified in
#'         \code{\link{getGenesByID}} and \code{Npmid} column for the paper
#'         count.
#' @export
#' @seealso getGenesByID
#' @family {Gene functions}
#'
#' @examples
#' cntT <- findGeneByPaperCnt(47)
#' head(cntT)
findGeneByPaperCnt <- function(cnt=1) {
    ids<-getGeneIdByPaperCnt(cnt)
    gnt<-getGenesByID(ids$GeneID) %>% dplyr::left_join(ids,by='GeneID')
    return(gnt)
}


#' Get gene table of frequently found genes within compartments
#'
#' Get gene table and paper count for genes mentioned \code{cnt}
#' or more times in different compartment-paper pairs.
#'
#' @param cnt  minimal number of times mentioned gene
#'
#' @return \code{data.frame} with 9 columns: 8 specified in
#'         \code{\link{getGenesByID}} and \code{Npmid} column for the paper
#'         count.
#' @export
#' @seealso getGenesByID
#' @family {Gene functions}
#'
#' @examples
#' cntT <- findGeneByPaperCnt(47)
#' head(cntT)
findGeneByCompartmentPaperCnt <- function(cnt = 1) {
    ids <- getGeneIdByCompartmentPaperCnt(cnt) %>%
        dplyr::left_join(getCompartments(),
                         by = c("LocalisationID" = 'ID')) %>%
        dplyr::rename('Localisation' = 'Name') %>%
        dplyr::select('GeneID', 'Localisation', 'Npmid')
    gnt <- getGenesByID(ids$GeneID) %>%
        dplyr::left_join(ids, by = 'GeneID')
    return(gnt)
}

#' Get gene table of frequently found genes
#'
#' Get gene table and paper count for genes mentioned \code{cnt}
#' or more times in different papers.
#'
#' @param pmids vector of PMIDs to search for genes
#' @param cnt  minimal number of papers that mentioned gene
#'
#' @return \code{data.frame} with 9 columns: 8 specified in
#'         \code{\link{getGenesByID}} and \code{Npmid} column for the paper
#'         count.
#' @export
#' @seealso getGenesByID
#' @family {Gene functions}
#'
#' @examples
#' res<- getAllGenes4Compartment(compartmentID = 1)
#' gnt<-getGeneInfoByIDs(res$GeneID)
#' pmids<-names(sort(table(gnt$PaperPMID))[1:5])
#' cntT <- findGeneByPapers(pmids,cnt=3)
#' head(cntT)
findGeneByPapers <- function(pmids,cnt=1) {
    ids<-getGeneIdByPapers(pmids,cnt)
    gnt<-getGenesByID(ids$GeneID) %>% dplyr::left_join(ids,by='GeneID')
    return(gnt)
}

#' Get GeneInfo table for set of GeneIDs
#'
#' Function lookup for internal GeneID values and return following
#' features for all found genes:
#' GeneID (internal database ID), Localisation (one of the following:
#' presynaptic, postsynaptic, synaptosome),
#' MGI (MGI ID), HumanEntrez (Human Entrez ID), MouseEntrez (Mouse Entrez ID),
#' HumanName (Human gene name), MouseName (Mouse gene name),
#' PaperPMID (PMID IDs for the publications where the genes were reported),
#' Paper (papers where specific genes were reported in a format
#' FIRSTAUTHOR_YEAR), Year, SpeciesTaxID (specie the original experiment
#' was performed on), BrainRegion (Brain region where the specific genes
#' were identified, according to the paper)
#'
#' This function then returns
#' following features for all found genes:
#' \itemize{
#' \item GeneID,
#' \item Localisation,
#' \item MGI,
#' \item HumanEntrez,
#' \item MouseEntrez,
#' \item HumanName,
#' \item MouseName,
#' \item PaperPMID,
#' \item Paper,
#' \item Year,
#' \item SpeciesTaxID,
#' \item BrainRegion
#' }
#'
#' @param ids \code{vector} of Gene IDs.
#'
#' @return \code{data.frame} with column specified above.
#' @export
#' @importFrom dplyr tbl select filter pull collect
#' @family {GeneInfo functions}
#'
#' @examples
#' gdf <- getGeneInfoByIDs(c(46, 6, 15, 1))
getGeneInfoByIDs <- function(ids) {
    if("Dataset" %in% DBI::dbListFields(get_dbconn(),'PaperGene')){
    gtbl <- get_dbconn() %>%
        dplyr::tbl('Gene') %>%
        dplyr::filter(ID %in% ids)
    ltbl <- get_dbconn() %>%
        dplyr::tbl('Localisation')
    pgtbl <- get_dbconn() %>%
        dplyr::tbl('PaperGene')
    ptbl <- get_dbconn() %>%
        dplyr::tbl('Paper')
    brtbl <- get_dbconn() %>%
        dplyr::tbl('BrainRegion')
    gns <- gtbl %>%
        dplyr::inner_join(pgtbl, by = c('ID' = 'GeneID')) %>%
        dplyr::rename(GeneID = ID) %>%
        dplyr::inner_join(ltbl, by = c('LocalisationID' = 'ID')) %>%
        dplyr::rename(Localisation = Name) %>%
        dplyr::inner_join(ptbl, by = c('PaperPMID' = 'PMID')) %>%
        dplyr::rename(Paper = Name) %>%
        dplyr::inner_join(brtbl, by = c('BrainRegionID' = 'ID')) %>%
        dplyr::rename(BrainRegion = Name) %>%
        dplyr::select(
        # gns <- get_dbconn() %>%
        # dplyr::tbl("FullGeneFullPaperFullRegion") %>%
        # dplyr::filter(GeneID %in% ids) %>%
        # dplyr::select(
            "GeneID",
            "Localisation",
            "MGI",
            "HumanEntrez",
            "MouseEntrez",
            "HumanName",
            "MouseName",
            "PaperPMID",
            "Paper",
            'Dataset',
            "Year",
            "SpeciesTaxID",
            "BrainRegion"
        )
    }else{
        warning('Old DB structure is used.\n',
                'Call: AnnotationHub::AnnotationHub() to update the cache.\n')
        gns <- get_dbconn() %>%
        dplyr::tbl("FullGeneFullPaperFullRegion") %>%
        dplyr::filter(GeneID %in% ids) %>%
        dplyr::select(
        "GeneID",
        "Localisation",
        "MGI",
        "HumanEntrez",
        "MouseEntrez",
        "HumanName",
        "MouseName",
        "PaperPMID",
        "Paper",
        "Year",
        "SpeciesTaxID",
        "BrainRegion"
        )
    }
    df <- gns %>% dplyr::collect() %>% unique
    return(df)
}
