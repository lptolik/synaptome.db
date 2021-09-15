library(synaptome.db)
library(testthat)
context("Testing connection functions")

test_that("Default connection is working", {
    expect_true(exists("snptmdbfile",envir = synaptome.db:::snptm_env))
    snptmdb<-DBI::dbConnect(RSQLite::SQLite(),
                        get("snptmdbfile",envir = synaptome.db:::snptm_env))
    expect_true(DBI::dbIsValid(snptmdb))
    con <- synaptome.db:::get_dbconn()
    expect_equal(con, snptmdb)
})

test_that("get_dbconn setup new connection", {
    synaptome.db:::get_dbconn()
    DBI::dbDisconnect(get("snptmdb",envir = synaptome.db:::snptm_env))
    con <- synaptome.db:::get_dbconn()
    expect_true(DBI::dbIsValid(con))
    expect_identical(con, get("snptmdb",envir = synaptome.db:::snptm_env))
})

