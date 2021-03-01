datacache <- new.env(hash=TRUE, parent=emptyenv())

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
#' PMID,
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
  df<-.getfGfPfR(ids)
  return(df)
}


getGeneInfoByEntrez<-function(entrez){
  ids<-findGenesByEntrez(entrez)
  df<-.getfGfPfR(ids)
  return(df)
}

findGenesByEntrez<-function(entrez){

}

findGenesByName<-function(name){

}


#' Get data from FullGeneFullPaperFullRegion view.
#'
#' @param ids
#'
#' @return
#' @export
#' @import dplyr
#'
getfGfPfR<-function(ids){
  gns<-get_dbconn() %>% tbl("FullGeneFullPaperFullRegion") %>% filter(GeneID %in% ids) %>%
    select('GeneID',
           'Localisation',
           'MGI',
           'HumanEntrez',
           'MouseEntrez',
           'HumanName',
           'MouseName',
           'PMID',
           'Paper',
           'Year',
           'SpeciesTaxID',
           'BrainRegion')
  df<-gns %>% collect()
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
