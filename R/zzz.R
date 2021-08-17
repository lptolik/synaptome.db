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
        dplyr::filter(GeneID %in% ids) %>%
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

##### Use cases 3 Show disease info for my list of genes#####
#' Prepare query for Disease table
#'
#' @return tbl_lazy
#' @import dplyr
getGeneDiseaseQuery <- function() {
    gns <- get_dbconn() %>% dplyr::tbl("FullGeneFullDisease")  %>%
        dplyr::select(
            HumanEntrez,
            HumanName,
            HDOID,
            Description)
    return(gns)
}

#' Get data about Gene-Disease mapping by GeneIDs
#'
#' @param ids gene IDs
#'
#' @return data.frame
#' @export
#' @import dplyr
#'
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
#' @import dplyr
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
#' @import dplyr
#'
getGeneDiseaseByName<-function(names){
    gns<-getGeneDiseaseQuery() %>% dplyr::filter(HumanName %in% names)
    df<-gns %>% dplyr::collect()
    return(df)
}

##### Use cases 4 Show mutations for specific disease for my list of genes#####

#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
#' @import dplyr
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
#' @import dplyr
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
#' @import dplyr
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
#' @import dplyr
getMutations4DiseaseByName<-function(name,hdoid){
    ids<-getGeneIdByName(name)
    gns <- getMutDiseaseQuery() %>%
        dplyr::filter(GeneID %in% ids & HDOID == hdoid)
    df<-gns %>% dplyr::collect()
    return(df)
}

##### Use cases 5 Extract the PPIs for my list of genes #####
#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
#' @import dplyr
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
#' @import dplyr
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

##### Use cases 6 Extract the PPIs for specific compartment #####

#' Provide full list of compartments (pres, post, synaptosome)
#'
#' @return data.frame
#' @export
#' @import dplyr
#'
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
    return(idsC)
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
    return(idsC)
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
    gns<-getPPIQuery() %>% dplyr::filter(A %in% cids | B %in% cids) %>%
        dplyr::filter(A %in% aids & B %in% aids)
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
    gns<-getPPIQuery() %>% dplyr::filter(A %in% ids & B %in% ids)
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


#' Get dbcon. Return connection to the database.
#'
#' @return dbConnect
#' @export
#' @import DBI
#' @import RSQLite
#'
get_dbconn <- function() {
    if (!exists('dbconn') || !DBI::dbIsValid(dbconn)) {
        pkgname = getPackageName()
        dbfile <- system.file("extdata", "synaptome.sqlite", package = pkgname)
        dbconn <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        cat('DB is connected with ',dbfile)
    }
    return(dbconn)
}

#' @export
.onLoad <- function(libname, pkgname)
{
    dbfile <- system.file(
        "extdata", "synaptome.sqlite", package=pkgname, lib.loc=libname)
    cat(pkgname,libname)
    db <- dbfile
    dbconn <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
    packageStartupMessage(paste("Package Synaptome.DB loaded with db fram",db))
}


#' @export
.onUnload <- function(libpath)
{
    dbDisconnect(get_dbconn())
}
