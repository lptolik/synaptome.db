library(synaptome.db)
library(testthat)
#get information for specific gene
t <- getGeneInfoByName('CASK') #(37 rows)
expect_equal(dim(t)[1],37)
#get information for the list of genes
t <- getGeneInfoByName(c('CASK', 'DLG2')) #(87 rows)
expect_equal(dim(t)[1],87)
#get information for specific gene
t <- getGeneInfoByEntrez(1742) #(55 rows)
expect_equal(dim(t)[1],55)
t <- getGeneInfoByEntrez('1742')
#get information for the list of genes
t <- getGeneInfoByName(c(1741, 1742, 1739, 1740)) #(195 rows)
expect_equal(dim(t)[1],195)
#get information for specific gene
t <- findGenesByEntrez(c(1742, 1741, 1739, 1740)) #(4 rows)
expect_equal(dim(t)[1],4)
#' Find information for gene IDs
t <- getGeneInfoByIDs (c(48, 585, 710)) #(110 rows)
expect_equal(dim(t)[1],110)
