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
t <- getGeneInfoByName(c('DLG4', "Dlg2", 'Dlg3', "Dlg1")) #(195 rows)
expect_equal(dim(t)[1],195)
cmp<-list(
    GeneID=1,
    Localisation='Postsynaptic',
    MGI='MGI:1277959',
    HumanEntrez=1742,
    MouseEntrez=13385,
    HumanName='DLG4',
    MouseName='Dlg4',
    PaperPMID=10818142,
    Paper='WALIKONIS_2000',
    Year=2000,
    SpeciesTaxID=10116,
    BrainRegion='Forebrain')
expect_equivalent(t[1,],cmp)
#get information for specific gene
t <- findGenesByEntrez(c(1742, 1741, 1739, 1740)) #(4 rows)
expect_equal(dim(t)[1],4)
#' Find information for gene IDs
t <- getGeneInfoByIDs (c(48, 585, 710)) #(110 rows)
expect_equal(dim(t)[1],110)
