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
#' @param entrez
#'
#' @return
#' @export
getGeneInfoByEntrez<-function(entrez){
  ids<-findGenesByEntrez(entrez)
  df<-getGeneInfoByIDs(ids)
  return(df)
}

#' Find GeneIDs for Entrez
#'
#' @param entrez
#'
#' @return
#' @export
findGenesByEntrez<-function(entrez){
  idsH<-get_dbconn() %>% tbl("Gene") %>% filter(HumanEntrez %in% entrez | MouseEntrez %in% entrez) %>% select(ID) %>% pull(ID) %>% unique
  return(idsH)
}

#' Find GeneIDs for names
#'
#' @param name
#'
#' @return
#' @export
findGenesByName<-function(name){
  idsH<-get_dbconn() %>% tbl("Gene") %>% filter(HumanName %in% name | MouseName %in% name) %>% select(ID) %>% pull(ID) %>% unique
  return(idsH)
}


#' Get GeneInfo table for set of GeneIDs
#'
#' @param ids
#'
#' @return
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
           d.HDOID,
           d.Description)
  return(gns)
}

#' Get data about Gene-Disease mapping by GeneIDs
#'
#' @param ids
#'
#' @return
#' @export
#' @import dplyr
#'
getGeneDiseaseByIDs<-function(ids){
  gns<-getGeneDiseaseQuery() %>% filter(GeneID %in% ids)
  df<-gns %>% collect()
  return(df)
}

#' Get data about Gene-Disease mapping by Entres IDs
#'
#' @param ids
#'
#' @return
#' @export
#' @import dplyr
#'
getGeneDiseaseByEntres<-function(entrez){
  ids<-findGenesByEntrez(entrez)
  df<-getGeneDiseaseByIDs(ids)
  return(df)
}

#' Get data about Gene-Disease mapping by  Name.
#'
#' @param ids
#'
#' @return
#' @export
#' @import dplyr
#'
getGeneDiseaseByName<-function(names){
  ids<-findGenesByName(names)
  df<-getGeneDiseaseByIDs(ids)
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
#' @param ids
#' @param hdoid
#'
#' @return
#' @export
getMutations4DiseaseByIDs<-function(ids,hdoid){
  gns <- getMutDiseaseQuery() %>% filter(GeneID %in% ids & HDOID == hdoid)
  df<-gns %>% collect()
  return(df)
}
#' Show mutations for specific disease by Entrez IDs
#'
#' @param entrez
#' @param hdoid
#'
#' @return
#' @export
getMutations4DiseaseByEntres<-function(entrez,hdoid){
  ids<-findGenesByEntrez(entrez)
  gns <- getMutDiseaseQuery() %>% filter(GeneID %in% ids & HDOID == hdoid)
  df<-gns %>% collect()
  return(df)
}
#' Show mutations for specific disease by gene name
#'
#' @param name
#' @param hdoid
#'
#' @return
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
  gns<-get_dbconn() %>% tbl("AllPpiAllPapers") %>%
    select(ppiID,
           A,
           B,
           method,
           type,
           taxID,
           PMID,
           Paper)
  return(gns)
}

#' Extract the PPIs for my list of genes defined by GeneID
#'
#' @param ids
#' @param type
#'
#' @return
#' @export
#'
#' @examples
getPPIbyIDs<-function(ids, type=c('induced','limited')){
  netType<-match.arg(type)
  gns<- switch (netType,
    induced = getPPIQuery %>% filter(A %in% ids | B %in% ids),
    limited = getPPIQuery %>% filter(A %in% ids & B %in% ids)
  )
  df <- gns %>% collect
  return(df)
}

#' Extract the PPIs for my list of genes defined by Entrez IDs
#'
#' @param entrez
#' @param type
#'
#' @return
#' @export
#'
#' @examples
getPPIbyEntrez<-function(entrez, type=c('induced','limited')){
  ids<-findGenesByEntrez(entrez)
  df<-getPPIbyIDs(ids,type)
  return(df)
}

#' Extract the PPIs for my list of genes defined by Gene name
#'
#' @param name
#' @param type
#'
#' @return
#' @export
#'
#' @examples
getPPIbyName<-function(name, type=c('induced','limited')){
  ids<-findGenesByName(name)
  df<-getPPIbyIDs(ids,type)
  return(df)
}

##### Use cases 6 Extract the PPIs for specific compartment (pres, post, synaptosome) #####

#' Provide full list of compartments
#'
#' @return
#' @export
#'
#' @examples
getCompartments<-function(){
  gns<-get_dbconn() %>% tbl("Localisation") %>% collect
  return(gns)
}

#' Get list of all genes found in compartment
#'
#' @param compartmentID
#'
#' @return
#' @export
getAllGenes4Compartment<-function(compartmentID){
  idsC<-get_dbconn() %>% tbl("FullGenePaper") %>% filter(LocalisationID == compartmentID) %>% select(ID) %>% pull(ID) %>% unique
  return(idsC)
}

#' Select genes from list that found in compartment
#'
#' @param ids
#' @param compartmentID
#'
#' @return
#' @export
getGenes4Compartment<-function(ids,compartmentID){
  idsC<-get_dbconn() %>% tbl("FullGenePaper") %>% filter(LocalisationID == compartmentID & GeneID %in% ids) %>% select(ID) %>% pull(ID) %>% unique
  return(idsC)
}

#' Prepare induces network for compartment
#'
#' @param ids
#' @param compartmentID
#'
#' @return
getInducedPPI4Compartment<-function(ids, compartmentID){
  cids<-getGenes4Compartment(ids,compartmentID)
  aids<-getAllGenes4Compartment(compartmentID)
  gns<-getPPIQuery %>% filter(A %in% cids | B %in% cids) %>% filter(A %in% aids & B %in% aids)
  return(gns)
}

#' Prepare limited network for compartment
#'
#' @param ids
#' @param compartmentID
#'
#' @return
getLimitedPPI4Compartment<-function(ids, compartmentID){
  cids<-getGenes4Compartment(ids,compartmentID)
  gns<-getPPIQuery %>% filter(A %in% ids & B %in% ids)
  return(gns)
}
#' Extract the PPIs for specific compartment
#'
#' @param ids
#' @param compartmentID
#' @param type
#'
#' @return
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
#' @return
#' @export
getBrainRegions<-function(){
  gns<-get_dbconn() %>% tbl("BrainRegion") %>% collect
  return(gns)
}

#' Get list of all genes found in particular BrainRegion for particular specie
#'
#' @param brainRegion
#' @param taxID
#'
#' @return
#' @export
getAllGenes4BrainRegion<-function(brainRegion,taxID){
  idsC<-get_dbconn() %>% tbl("FullGeneFullPaperFullRegion") %>% filter(BrainRegion == brainRegion & SpeciesTaxID == taxID) %>% select(ID) %>% pull(ID) %>% unique
  return(idsC)
}

#' Select genes from list that found in particular BrainRegion for particular specie
#'
#' @param brainRegion
#' @param taxID
#' @param ids
#'
#' @return
#' @export
getGenes4BrainRegion<-function(ids,brainRegion,taxID){
  idsC<-get_dbconn() %>% tbl("FullGenePaper") %>% filter(BrainRegion == brainRegion & SpeciesTaxID == taxID & GeneID %in% ids) %>% select(ID) %>% pull(ID) %>% unique
  return(idsC)
}

#' Prepare induces network for compartment
#'
#' @param brainRegion
#' @param taxID
#' @param ids
#'
#' @return
getInducedPPI4BrainRegion<-function(ids, brainRegion,taxID){
  cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
  aids<-getAllGenes4BrainRegion(brainRegion,taxID)
  gns<-getPPIQuery %>% filter(A %in% cids | B %in% cids) %>% filter(A %in% aids & B %in% aids)
  return(gns)
}

#' Prepare limited network for compartment
#'
#' @param brainRegion
#' @param taxID
#' @param ids
#'
#' @return
getLimitedPPI4BrainRegion<-function(ids, brainRegion,taxID){
  cids<-getGenes4BrainRegion(ids,brainRegion,taxID)
  gns<-getPPIQuery %>% filter(A %in% ids & B %in% ids)
  return(gns)
}
#' Extract the PPIs for specific compartment
#'
#' @param ids
#' @param type
#' @param brainRegion
#' @param taxID
#'
#' @return
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
