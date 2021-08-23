library(synaptome.db)
library(testthat)
context("Testing connection functions")

test_that("Default connection is working", {
    expect_true(exists("dbconn"))
    expect_true(DBI::dbIsValid(dbconn))
    con <- synaptome.db:::get_dbconn()
    expect_identical(con, dbconn)
})

test_that("get_dbconn setup new connection", {
    DBI::dbDisconnect(dbconn)
    con <- synaptome.db:::get_dbconn()
    expect_true(DBI::dbIsValid(con))
    expect_identical(con, dbconn)
})
