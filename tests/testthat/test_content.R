context("Testing content of the returned tables")

test_that("Test first line of the content",{
    t <- getGeneInfoByName(c('DLG4', "Dlg2", 'Dlg3', "Dlg1")) #(195 rows)
    dlg4_geneinfo<-list(
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
    expect_equivalent(t[1,],dlg4_geneinfo)
})
