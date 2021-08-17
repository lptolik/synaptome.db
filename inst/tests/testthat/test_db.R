#get information for specific gene
t <- getGeneInfoByName('CASK') #(37 rows)
#get information for the list of genes
t <- getGeneInfoByName(c('CASK', 'DLG2')) #(87 rows)
#get information for specific gene
t <- getGeneInfoByEntrez(1742) #(55 rows)
t <- getGeneInfoByEntrez('1742')
#get information for the list of genes
t <- getGeneInfoByName(c(1741, 1742, 1739, 1740)) #(195 rows)
#get information for specific gene
t <- findGenesByEntrez(c(1742, 1741, 1739, 1740)) #(4 rows)
