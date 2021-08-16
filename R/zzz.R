datacache <- new.env(hash=TRUE, parent=emptyenv())

##### Use cases 1 and 2 Show my favourite gene info#####
#' Get gene information for set of gene names.
#' Function lookup for name in Human Gene name and Mouse Gene name data and return
#' following features for all found genes:
#' GeneID,
#' Localisation,
#' MGI,
#' HumanEntrez,
#' MouseEntrez,
#' HumanName,
#' MouseName,
#' PaperPMID,
#' Paper,
#' Year,
#' SpeciesTaxID,
#' BrainRegion
#'
#' @param name vector of gene names
#'
#' @return data.frame with fields specified above
#' @export
#'
getGeneInfoByName<-function(name){
  ids<-findGenesByName(name)
  df<-getGeneInfoByIDs(ids)
  return(df)
}


#' Get GeneInfo table for set of Entres IDs
#'
#' @param entrez Entres IDs
#'
#' @return  data.frame
#' @export
getGeneInfoByEntrez<-function(entrez){
  ids<-findGenesByEntrez(entrez)
  df<-getGeneInfoByIDs(ids)
  return(df)
}

#' Find GeneIDs for Entrez
#'
#' @param entrez Entres IDs
#'
#' @return  data.frame
#' @export
findGenesByEntrez<-function(entrez){
  idsH<-get_dbconn() %>% tbl("Gene") %>% filter(HumanEntrez %in% entrez | MouseEntrez %in% entrez) %>% select(ID) %>% pull(ID) %>% unique
  return(idsH)
}

#' Find GeneIDs for names
#'
#' @param name gene names
#'
#' @return  data.frame
#' @export
findGenesByName<-function(name){
  idsH<-get_dbconn() %>% tbl("Gene") %>% filter(HumanName %in% name | MouseName %in% name) %>% select(ID) %>% pull(ID) %>% unique
  return(idsH)
}


#' Get GeneInfo table for set of GeneIDs
#'
#' @param ids gene ids
#'
#' @return data.frame
#' @export
#' @import dplyr
#'
getGeneInfoByIDs<-function(ids){
  gns<-get_dbconn() %>% tbl("FullGeneFullPaperFullRegion") %>% filter(GeneID %in% ids) %>%
    select('GeneID',
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
           'BrainRegion')
  df<-gns %>% collect()
  return(df)
}

##### Use cases 3 Show disease info for my list of genes#####
#' Prepare query for Disease table
#'
#' @return tbl_lazy
getGeneDiseaseQuery<-function(){
  gns<-get_dbconn() %>% tbl("FullGeneFullDisease")  %>%
    select(HumanEntrez,
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
  ginf<-getGeneInfoByIDs(ids) %>% pull(HumanEntrez)
  gns<-getGeneDiseaseQuery() %>% filter(HumanEntrez %in% ginf)
  df<-gns %>% collect()
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
  gns<-getGeneDiseaseQuery() %>% filter(HumanEntrez %in% entrez)
  df<-gns %>% collect()
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
  gns<-getGeneDiseaseQuery() %>% filter(HumanName %in% names)
  df<-gns %>% collect()
  return(df)
}

##### Use cases 4	Show mutations for specific disease for my list of genes#####

#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
getMutDiseaseQuery<-function(){
  gns<-get_dbconn() %>% tbl("AllMutationsAllPapers") %>%
    select(GeneID,
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
           Paper)
  return(gns)
}

#' Show mutations for specific disease by GeneIDs
#'
#' @param ids Gene IDs
#' @param hdoid HDO IDs
#'
#' @return  data.frame
#' @export
getMutations4DiseaseByIDs<-function(ids,hdoid){
  gns <- getMutDiseaseQuery() %>% filter(GeneID %in% ids & HDOID == hdoid)
  df<-gns %>% collect()
  return(df)
}
#' Show mutations for specific disease by Entrez IDs
#'
#' @param entrez Entrez IDs
#' @param hdoid HDO IDs
#'
#' @return data.frame
#' @export
getMutations4DiseaseByEntres<-function(entrez,hdoid){
  ids<-findGenesByEntrez(entrez)
  gns <- getMutDiseaseQuery() %>% filter(GeneID %in% ids & HDOID == hdoid)
  df<-gns %>% collect()
  return(df)
}
#' Show mutations for specific disease by gene name
#'
#' @param name Gene name
#' @param hdoid HDO IDs
#'
#' @return data.frame
#' @export
getMutations4DiseaseByName<-function(name,hdoid){
  ids<-findGenesByName(name)
  gns <- getMutDiseaseQuery() %>% filter(GeneID %in% ids & HDOID == hdoid)
  df<-gns %>% collect()
  return(df)
}

##### Use cases 5 Extract the PPIs for my list of genes #####
#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
getPPIQuery<-function(){
  #  gns<-get_dbconn() %>% tbl("AllPpiAllPapers") %>%
  gns<-get_dbconn() %>% tbl("PPI") %>%
    select(ID,#ppiID,
           A,
           B,
           method,
           type,
           taxID#,
           #           PMID,
           #           Paper)
    )
  return(gns)
}

#' Extract the PPIs for my list of genes defined by GeneID
#'
#' @param ids Gene IDs
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
getPPIbyIDs<-function(ids, type=c('induced','limited')){
  netType<-match.arg(type)
  gns<- switch (netType,
                induced = getPPIQuery() %>% filter(A %in% ids | B %in% ids),
                limited = getPPIQuery() %>% filter(A %in% ids & B %in% ids)
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
  ids<-findGenesByEntrez(entrez)
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
  ids<-findGenesByName(name)
  df<-getPPIbyIDs(ids,type)
  return(df)
}

##### Use cases 6 Extract the PPIs for specific compartment (pres, post, synaptosome) #####

#' Provide full list of compartments
#'
#' @return data.frame
#' @export
#'
getCompartments<-function(){
  gns<-get_dbconn() %>% tbl("Localisation") %>% collect
  return(gns)
}

#' Get list of all genes found in compartment
#'
#' @param compartmentID Compartment ID
#'
#' @return data.frame
#' @export
getAllGenes4Compartment<-function(compartmentID){
  idsC<-get_dbconn() %>% tbl("FullGenePaper") %>%
    filter(LocalisationID == compartmentID) %>%
    select(GeneID) %>% pull(GeneID) %>% unique
  return(idsC)
}

#' Select genes from list that found in compartment
#'
#' @param ids Gene IDs
#' @param compartmentID compartment ID
#'
#' @return data.frame
#' @export
getGenes4Compartment<-function(ids,compartmentID){
  idsC<-get_dbconn() %>% tbl("FullGenePaper") %>%
    filter(LocalisationID == compartmentID & GeneID %in% ids) %>%
    select(GeneID) %>% pull(GeneID) %>% unique
  return(idsC)
}

#' Prepare induces network for compartment
#'
#' @param ids gene IDs
#' @param compartmentID compartment ID
#'
#' @return  tbl_lazy
getInducedPPI4Compartment<-function(ids, compartmentID){
  cids<-getGenes4Compartment(ids,compartmentID)
  aids<-getAllGenes4Compartment(compartmentID)
  gns<-getPPIQuery() %>% filter(A %in% cids | B %in% cids) %>% filter(A %in% aids & B %in% aids)
  return(gns)
}

#' Prepare limited network for compartment
#'
#' @param ids Gene IDs
#' @param compartmentID compartment ID
#'
#' @return tbl_lazy
getLimitedPPI4Compartment<-function(ids, compartmentID){
  cids<-getGenes4Compartment(ids,compartmentID)
  gns<-getPPIQuery() %>% filter(A %in% ids & B %in% ids)
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
getPPIbyIDs4Compartment<-function(ids, compartmentID, type=c('induced','limited')){
  netType<-match.arg(type)
  gns<- switch (netType,
                induced = getInducedPPI4Compartment(ids, compartmentID),
                limited = getLimitedPPI4Compartment(ids, compartmentID)
  )
  df <- gns %>% collect
  return(df)
}

##### Use cases 7 Extract the PPIs for specific brain region for specific specie #####

#' Get list of all Brain regions in the database
#'
#' @return data.frame
#' @export
getBrainRegions<-function(){
  gns<-get_dbconn() %>% tbl("BrainRegion") %>% collect
  return(gns)
}

#' Get list of all genes found in particular BrainRegion for particular specie
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#'
#' @return data.frame
#' @export
getAllGenes4BrainRegion<-function(brainRegion,taxID){
  idsC<-get_dbconn() %>% tbl("FullGeneFullPaperFullRegion") %>%
    filter(BrainRegion == brainRegion & SpeciesTaxID == taxID) %>%
    select(GeneID) %>% pull(GeneID) %>% unique
  return(idsC)
}

#' Select genes from list that found in particular BrainRegion for particular specie
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return data.frame
#' @export
getGenes4BrainRegion<-function(ids,brainRegion,taxID){
  idsC<-get_dbconn() %>% tbl("FullGeneFullPaperFullRegion") %>%
    filter(BrainRegion == brainRegion & SpeciesTaxID == taxID & GeneID %in% ids) %>%
    select(GeneID) %>% pull(GeneID) %>% unique
  return(idsC)
}

#' Prepare induces network for compartment
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return  tbl_lazy
getInducedPPI4BrainRegion<-function(ids, brainRegion,taxID){
  cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
  aids<-getAllGenes4BrainRegion(brainRegion,taxID)
  gns<-getPPIQuery() %>% filter(A %in% cids | B %in% cids) %>%
    filter(A %in% aids & B %in% aids)
  return(gns)
}

#' Prepare limited network for compartment
#'
#' @param brainRegion region ID
#' @param taxID taxon ID
#' @param ids gene IDs
#'
#' @return  tbl_lazy
getLimitedPPI4BrainRegion<-function(ids, brainRegion,taxID){
  cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
  gns<-getPPIQuery() %>% filter(A %in% ids & B %in% ids)
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
getPPIbyIDs4BrainRegion<-function(ids, brainRegion,taxID, type=c('induced','limited')){
  netType<-match.arg(type)
  gns<- switch (netType,
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
get_dbconn <- function(){
  get("dbconn", envir=datacache)
}

.onLoad <- function(libname, pkgname)
{
  #  require("methods", quietly=TRUE)
  #  require("DBI", quietly=TRUE)
  #require("methods", quietly=TRUE)
  dbfile <- system.file("extdata", "synaptome.sqlite", package=pkgname, lib.loc=libname)
  assign("dbfile", dbfile, envir=datacache)

  db <- dbfile
  dbconn <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
  assign("dbconn", dbconn, envir=datacache)

  dbschema <-  function() {
    cat(dbGetQuery(dbconn, "SELECT * FROM sqlite_master;")$sql)
  }
  assign("dbschema", dbschema, envir=datacache)

  dbInfo <- function() dbGetQuery(dbconn, "SELECT * FROM METADATA;")
  assign("dbInfo", dbInfo, envir=datacache)
}


.onUnload <- function(libpath)
{
  dbDisconnect(get_dbconn())
}
