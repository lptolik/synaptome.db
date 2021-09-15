library(synaptome.db)
library(testthat)
context("Testing connection functions")

test_that("Default connection is working", {
    expect_true(exists("snptmdb"))
    expect_true(DBI::dbIsValid(snptmdb))
    con <- synaptome.db:::get_dbconn()
    expect_identical(con, snptmdb)
})

test_that("get_dbconn setup new connection", {
    DBI::dbDisconnect(snptmdb)
    con <- synaptome.db:::get_dbconn()
    expect_true(DBI::dbIsValid(con))
    expect_identical(con, snptmdb)
})
