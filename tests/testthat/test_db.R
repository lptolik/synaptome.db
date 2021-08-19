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

test_that("Proper row numbers in PPIs",{
    expect_equal(
        dim(getPPIbyIDs(c(48, 585, 710), type='limited')),
        c(2,2))
    expect_equal(
        dim(getPPIbyIDs(c(48, 585, 710), type='induced')),
        c(306,2))
    expect_equal(
        dim(getTableFromPPI(getPPIbyIDs(c(48, 585, 710),type='limited'))),
        c(2,16))
    expect_equal(
        dim(getPPIbyIDs(c(48, 129,  975,  4422, 5715, 5835), type='lim')),
        c(12,2))
    expect_equal(
        dim(
            getPPIbyName(c('CASK', 'DLG4', 'GRIN2A', 'GRIN2B', 'GRIN1'),
                         type='lim')),
        c(10,2))
    expect_equal(
        dim(getPPIbyEntrez(c(1739, 1740, 1742, 1741), type='ind')),
        c(342,2))

 })

test_that("Proper graph created from PPI",{
    g<-getIGraphFromPPI(
        getPPIbyIDs(c(48, 129,  975,  4422, 5715, 5835), type='lim'))
    expect_s3_class(g,'igraph')
    expect_equal(igraph::vcount(g),6)
    expect_equal(igraph::ecount(g),12)
    expect_equal(
        igraph::vertex_attr_names(g),
        c( "name",
           "MGI",
           "HumanEntrez",
           "MouseEntrez",
           "RatEntrez",
           "HumanName",
           "MouseName",
           "RatName" ))
})
