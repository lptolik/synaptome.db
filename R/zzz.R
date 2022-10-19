snptm_env <- new.env(parent = emptyenv())

#' Get dbcon. Return connection to the database.
#'
#' @return dbConnect
#' @import DBI
#' @import RSQLite
#' @import dbplyr
#' @keywords internal
get_dbconn <- function() {
    if (!exists("snptmdb", envir = snptm_env) ||
        !DBI::dbIsValid(get("snptmdb", envir = snptm_env))) {
        if (!exists("snptmdbfile", envir = snptm_env) ||
            !file.exists(get("snptmdbfile", envir = snptm_env))) {
            f <- .getdbfile()
            assign("snptmdbfile", f, envir = snptm_env)
        }
        dbc <- DBI::dbConnect(RSQLite::SQLite(),
                              get("snptmdbfile", envir = snptm_env))
        assign("snptmdb", dbc, envir = snptm_env)
        # cat('DB is connected with ',dbfile)
    } else{
        dbc <- get("snptmdb", envir = snptm_env)
    }
    return(dbc)
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
    f <- .getdbfile()
    assign("snptmdbfile",f,envir = snptm_env)
}

#' Hidden function that creates a local copy of the database.
#'
#' @return path to the newly created database
#' @import AnnotationHub
#' @import synaptome.data
#' @importFrom utils unzip
#' @keywords internal
.getdbfile <- function() {
    o<-options(show.error.messages = FALSE)
    ahub <- try(AnnotationHub::AnnotationHub(localHub=TRUE))
    if(inherits(ahub, "try-error") ||
       !"AH107282" %in% names(ahub)){
        ahub <- AnnotationHub::AnnotationHub()#(hub='http://127.0.0.1:9393/')
    }
    sdb<-AnnotationHub::query(ahub,"synaptome.data")
    if(!"AH107282" %in% names(sdb)){
        warning("You're using old version of the AnnotationHub,\n",
                "not all functionality is available.\n",
                "Please update your cache by connecting to the network\n",
                "and calling 'AnnotationHub::AnnotationHub()'\n",
                "make sure that snapshotDate() is not before 2022-10-18.\n")
        zipF<-sdb[[1]]
    }else{
        zipF<-sdb[["AH107282"]]
    }
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
