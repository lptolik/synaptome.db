context("Testing access to mutation table")

hdoid<-'DOID:0060041'
ids<-c(6,32,127,181,240,267,558)
entrez<-c("23859", "17754", "18673", "268566", "12293", "320840", "24012")
name<-c("Dlg2", "Map1a", "Phb", "Gphn", "Cacna2d1", "Negr1", "Rgs7")

test_that("Test mutation query", {
    expect_no_warning(q<-getMutDiseaseQuery())#,'do not support mutations')
    expect_false(is.null(q))
})

test_that("Test getMutations4DiseaseByIDs", {
    expect_no_warning(mdf<-getMutations4DiseaseByIDs(ids, hdoid))#,'do not support mutations')
    expect_s3_class(mdf,"data.frame")
    expect_equal(dim(mdf),c(7,20))
    expect_match(mdf$HDOID,hdoid)
    expect_false(any(is.na(match(ids,mdf$GeneID))))
})

test_that("Test getMutations4DiseaseByEntres", {
    expect_no_warning(mdf<-getMutations4DiseaseByEntres(entrez, hdoid))#,'do not support mutations')
    expect_s3_class(mdf,"data.frame")
    expect_equal(dim(mdf),c(7,20))
    expect_match(mdf$HDOID,hdoid)
    expect_false(any(is.na(match(entrez,c(mdf$MouseEntrez,mdf$HumanEntrez)))))
})

test_that("Test getMutations4DiseaseByName", {
    expect_no_warning(mdf<-getMutations4DiseaseByName(name, hdoid))#,'do not support mutations')
    expect_s3_class(mdf,"data.frame")
    expect_equal(dim(mdf),c(7,20))
    expect_match(mdf$HDOID,hdoid)
    expect_false(any(is.na(match(name,c(mdf$MouseName,mdf$HumanName)))))
})

