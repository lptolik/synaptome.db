datacache <- new.env(hash=TRUE, parent=emptyenv())

.onLoad <- function(libname, pkgname)
{
  require("methods", quietly=TRUE)
  dbfile <- system.file("extdata", "mesh.sqlite", package=pkgname, lib.loc=libname)
  assign("dbfile", dbfile, envir=datacache)

  driver <- dbDriver("SQLite")
  db <- dbfile
  dbconn <- dbConnect(driver, db)
  assign("dbconn", dbconn, envir=datacache)

  dbshow <- function(){
    cat("Quality control information for MeSH:\n\n\n")
    cat("This package has the following mappings:\n\n")
    print(dbGetQuery(dbconn, "SELECT * FROM MAPCOUNTS;"))
    cat("Additional Information about this package:\n")
    cat("Date for MeSH data: 20120907\n")
  }
  assign("dbshow", dbshow, envir=datacache)

  dbschema <-  function() cat(dbGetQuery(dbconn, "SELECT * FROM sqlite_master;")$sql)
  assign("dbschema", dbschema, envir=datacache)

  dbInfo <- function() dbGetQuery(dbconn, "SELECT * FROM METADATA;")
  assign("dbInfo", dbInfo, envir=datacache)
}


.onUnload <- function(libpath)
{
  dbDisconnect(MeSH_dbconn())
}
