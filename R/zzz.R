
#' Get dbcon. Return connection to the database.
#'
#' @return dbConnect
#' @export
#' @import DBI
#' @import RSQLite
#'
get_dbconn <- function() {
    if (!exists('dbconn') || !DBI::dbIsValid(dbconn)) {
        pkgname = "synaptome.db"#methods::getPackageName()
        dbfile <- system.file("extdata", "synaptome.sqlite", package = pkgname)
        dbconn <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        #cat('DB is connected with ',dbfile)
    }
    return(dbconn)
}

#' @export
.onLoad <- function(libname, pkgname)
{
    dbfile <- system.file(
        "extdata", "synaptome.sqlite", package=pkgname, lib.loc=libname)
    #cat(pkgname,libname)
    db <- dbfile
    dbconn <<- DBI::dbConnect(RSQLite::SQLite(), dbfile)
    #packageStartupMessage(paste("Package Synaptome.DB loaded with db fram",db))
}


#' @export
.onUnload <- function(libpath)
{
    dbDisconnect(get_dbconn())
}
