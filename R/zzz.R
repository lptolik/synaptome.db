
#' Get dbcon. Return connection to the database.
#'
#' @return dbConnect
#' @import DBI
#' @import RSQLite
#' @import dbplyr
#' @keywords internal
get_dbconn <- function() {
    if (!exists("mydb") || !DBI::dbIsValid(mydb)) {
        pkgname <- "synaptome.db" # methods::getPackageName()
        dbfile <- system.file("extdata", "synaptome.sqlite", package = pkgname)
        mydb <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        # cat('DB is connected with ',dbfile)
    }
    return(mydb)
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
#' @import AnnotationHub
#' @import synaptome.data
#' @importFrom utils unzip
#' @keywords internal
.onLoad <- function(libname, pkgname) {
    dbfile <- system.file(
        "extdata", "synaptome.sqlite",
        package = pkgname, lib.loc = libname
    )
    if(!file.exists(dbfile)){
        ahub <- AnnotationHub::AnnotationHub(localHub=TRUE)
    sdb<-AnnotationHub::query(ahub,'SynaptomeDB')
        zipF<-sdb[[1]]
        l<-unzip(zipF,list=TRUE)
        fname<-l$Name[which.max(l$Length)]
        dbpath<-unzip(zipF,files=fname,exdir=tempdir())
        file.copy(dbpath,paste0(
            system.file(
                "extdata",
                package = pkgname, lib.loc = libname
            ),.Platform$file.sep,
            "synaptome.sqlite")
        )
        dbfile <- system.file(
            "extdata", "synaptome.sqlite",
            package = pkgname, lib.loc = libname
        )
    }
    # cat(pkgname,libname)
    db <- dbfile
    mydb <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
}


.onUnload <- function(libpath) {
    DBI::dbDisconnect(get_dbconn())
}
