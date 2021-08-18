##### Use cases 1 and 2 Show my favourite gene info#####
#' Get gene information for set of gene names.
#'
#'
#' Function lookup for name in Human Gene name and Mouse Gene name data
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
#'
#' @examples
#' #get information for specific gene
#' t <- getGeneInfoByName('CASK')
#'
#' #get information for the list of genes
#' t <- getGeneInfoByName(c('CASK', 'DLG2'))
getGeneInfoByName<-function(name){
    ids<-getGeneIdByName(name)
    df<-getGeneInfoByIDs(ids)
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
#'
#' @examples
#' #get information for specific gene
#' t <- getGeneInfoByEntrez(1742)
#' #get information for specific character string Entres representation
#' t <- getGeneInfoByEntrez('1742')
#'
#' #get information for the list of genes
#' t <- getGeneInfoByName(c(1741, 1742, 1739, 1740))
getGeneInfoByEntrez<-function(entrez){
    ids<-getGeneIdByEntrez(entrez)
    df<-getGeneInfoByIDs(ids)
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
#' @import dplyr
#' @seealso \code{\link{findGenesByName}}
#' @examples
#' #get information for specific gene
#' t <- findGenesByEntrez(c(1742, 1741, 1739, 1740))
findGenesByEntrez<-function(entrez){
    ids<-getGeneIdByEntrez(entrez)
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
#' @examples
#' t <- getGeneIdByEntrez(c(1742, 1741, 1739, 1740))
getGeneIdByEntrez<-function(entrez){
    idsH<-get_dbconn() %>% dplyr::tbl("Gene") %>%
        dplyr::filter(
            HumanEntrez %in% entrez |
                MouseEntrez %in% entrez |
                RatEntrez %in% entrez) %>%
        dplyr::select(ID) %>% dplyr::pull(ID) %>% unique
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
#' @import dplyr
#' @seealso \code{\link{findGenesByEntrez}}
#' @examples
#' # Find GeneIDs for names
#' t <- findGenesByName(c('Src', 'Srcin1', 'Fyn'))
findGenesByName<-function(name){
    ids<-getGeneIdByName(name)
    return(getGenesByID(ids))
}

#' Get gene table from list of GeneIDs.
#'
#'  Takes internal gene IDs as input and return the following features for
#'  all found genes:
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
#' @examples
#' gdf<-getGenesByID(c(46,6,15,1))
getGenesByID<-function(ids){
    genes<-get_dbconn() %>% dplyr::tbl("Gene") %>%
        dplyr::select(
            ID,MGI,
            HumanEntrez,MouseEntrez,
            HumanName,MouseName,
            RatEntrez,RatName) %>%
        dplyr::filter(ID %in% ids) %>% rename(GeneID=ID) %>%
        collect
    return(genes)
}

#' Get list of GeneIDs corresponding to provided gene names.
#'
#'
#' @param name \code{vector} of gene names.
#'
#' @return \code{vector} of GeneID values.
#'
#' @examples
#' t <- getGeneIdByName(c('Src', 'Srcin1', 'Fyn'))
getGeneIdByName<-function(name){
    idsH<-get_dbconn() %>% dplyr::tbl("Gene") %>%
        dplyr::filter(
            HumanName %in% name |
                MouseName %in% name |
                RatName %in% name) %>%
        dplyr::select(ID) %>% dplyr::pull(ID) %>% unique
    return(idsH)
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
#' @import dplyr
#'
#' @examples
#' gdf<-getGeneInfoByIDs(c(46,6,15,1))
getGeneInfoByIDs <- function(ids) {
    gns <- get_dbconn() %>% dplyr::tbl("FullGeneFullPaperFullRegion") %>%
        dplyr::filter(GeneID %in% ids) %>%
        dplyr::select(
            'GeneID',
            'Localisation',
            'MGI',
            'HumanEntrez',
            'MouseEntrez',
            'HumanName',
            'MouseName',
            'PaperPMID',
            'Paper',
            'Year',
            'SpeciesTaxID',
            'BrainRegion'
        )
    df <- gns %>% dplyr::collect()
    return(df)
}
