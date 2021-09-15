library(synaptome.db)
library(testthat)
context("Testing connection functions")

test_that("Default connection is working", {
    expect_true(exists("mydb"))
    expect_true(DBI::dbIsValid(mydb))
    con <- synaptome.db:::get_dbconn()
    expect_identical(con, mydb)
})

test_that("get_dbconn setup new connection", {
    DBI::dbDisconnect(mydb)
    con <- synaptome.db:::get_dbconn()
    expect_true(DBI::dbIsValid(con))
    expect_identical(con, mydb)
})
