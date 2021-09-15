
#' Get dbcon. Return connection to the database.
#'
#' @return dbConnect
#' @import DBI
#' @import RSQLite
#' @import dbplyr
#' @keywords internal
get_dbconn <- function() {
    if (!exists("snptmdb") || !DBI::dbIsValid(snptmdb)) {
        if(!exists("snptmdbfile") || !file.exists(snptmdbfile)){
            snptmdbfile <<-  .getdbfile()
        }
        snptmdb <<- DBI::dbConnect(RSQLite::SQLite(), snptmdbfile)
        # cat('DB is connected with ',dbfile)
    }
    return(snptmdb)
}

#' Hidden load function.
#' Check the presence of the database and load it with AnnotationHub
#' if required.
#' TODO: add timestamp validation.
#'
#' @param libname the path where the package is installed
#' @param pkgname name of the package
#'
#' @return dbConnect
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
    snptmdbfile <<-  .getdbfile()
    snptmdb <<- DBI::dbConnect(RSQLite::SQLite(), snptmdbfile)
}

#' Hidden function that creates a local copy of the database.
#'
#' @return path to the newly created database
#' @import AnnotationHub
#' @import synaptome.data
#' @importFrom utils unzip
#' @keywords internal
.getdbfile <- function() {
    # ahub <- AnnotationHub::AnnotationHub(localHub=TRUE)
    ahub <- AnnotationHub(hub='http://127.0.0.1:9393/')
    sdb<-AnnotationHub::query(ahub,'SynaptomeDB')
    zipF<-sdb[[1]]
    l<-unzip(zipF,list=TRUE)
    fname<-l$Name[which.max(l$Length)]
    dbpath<-file.path(hubCache(sdb),fname)
    if(!file.exists(dbpath)){
        dbpath<-unzip(zipF,files=fname,exdir=hubCache(sdb))
    }
    return(dbpath)
}

.onUnload <- function(libpath) {
    DBI::dbDisconnect(get_dbconn())
}
