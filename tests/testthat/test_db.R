library(synaptome.db)
library(testthat)
context("Testing number of returned rows")

test_that("Proper row number in geneInfo", {
    #get information for specific gene
    t <- getGeneInfoByName('CASK')
    expect_equal(dim(t), c(37,12))
    #get information for the list of genes
    t <- getGeneInfoByName(c('CASK', 'DLG2'))
    expect_equal(dim(t), c(87,12))
    #get information for specific gene
    t <- getGeneInfoByEntrez(1742)
    expect_equal(dim(t), c(55,12))
    t <- getGeneInfoByEntrez('1742')
    #get information for the list of genes
    t <-
        getGeneInfoByName(c('DLG4', "Dlg2", 'Dlg3', "Dlg1"))
    expect_equal(dim(t), c(195,12))
    #get information for specific gene
    t <- findGenesByEntrez(c(1742, 1741, 1739, 1740))
    expect_equal(dim(t), c(4,8))
    #' Find information for gene IDs
    t <- getGeneInfoByIDs (c(48, 585, 710))
    expect_equal(dim(t), c(110,12))
})

test_that("Proper row number in compartments",{
    c<-getCompartments()
    expect_equal(dim(c), c(3,3))
})

# test_that("Proper row numbers in PPIs",{
#     t <- getPPIbyIDs(c(48, 585, 710), type='limited') #(16 rows)
#     expect_equal(dim(t),c(16,6))
#     t <- getPPIbyIDs(c(48, 585, 710), type='induced') #306 rows
#     expect_equal(dim(t),c(306,6))
#     tbl<-getTableFromPPI(getPPIbyIDs(c(48, 585, 710), type='limited'))
#     expect_equal(dim(t),c(16,22))
# })
