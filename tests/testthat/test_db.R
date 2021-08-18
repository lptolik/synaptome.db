library(synaptome.db)
library(testthat)
context("Testing number of returned rows")

test_that("Proper row number in geneInfo", {
    #get information for specific gene
    t <- getGeneInfoByName('CASK')
    expect_equal(dim(t)[1], 37)
    #get information for the list of genes
    t <- getGeneInfoByName(c('CASK', 'DLG2'))
    expect_equal(dim(t)[1], 87)
    #get information for specific gene
    t <- getGeneInfoByEntrez(1742)
    expect_equal(dim(t)[1], 55)
    t <- getGeneInfoByEntrez('1742')
    #get information for the list of genes
    t <-
        getGeneInfoByName(c('DLG4', "Dlg2", 'Dlg3', "Dlg1"))
    expect_equal(dim(t)[1], 195)
    #get information for specific gene
    t <- findGenesByEntrez(c(1742, 1741, 1739, 1740))
    expect_equal(dim(t)[1], 4)
    #' Find information for gene IDs
    t <- getGeneInfoByIDs (c(48, 585, 710))
    expect_equal(dim(t)[1], 110)
})

test_that("Proper row number in compartments",{
    c<-getCompartments()
    expect_equal(dim(c)[1], 3)
})
