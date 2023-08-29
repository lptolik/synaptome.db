library(synaptome.db)
library(testthat)
context("Testing number of returned rows")

test_that("Proper row number in geneInfo", {
    #' Find information for gene IDs
    t <- getGeneInfoByIDs(c(48, 585, 710))
    expect_equal(dim(t), c(161, 13))
    # get information for specific gene
    t <- getGeneInfoByName("CASK")
    expect_equal(dim(t), c(54, 13))
    # get information for the list of genes
    t <- getGeneInfoByName(c("CASK", "DLG2"))
    expect_equal(dim(t), c(121, 13))
    # get information for specific gene
    t <- getGeneInfoByEntrez(1742)
    expect_equal(dim(t), c(72, 13))
    ts <- getGeneInfoByEntrez("1742")
    expect_equal(t,ts)
    # get information for the list of genes
    t <-
        getGeneInfoByName(c("DLG4", "Dlg2", "Dlg3", "Dlg1"))
    expect_equal(dim(t), c(263, 13))
    # get information for specific gene
    t <- findGenesByEntrez(c(1742, 1741, 1739, 1740))
    expect_equal(dim(t), c(4, 8))
    t <- findGenesByName(c("DLG4", "Dlg2", "Dlg3", "Dlg1"))
    expect_equal(dim(t), c(4, 8))
})

test_that("Proper row number in brain regions", {
    expect_equal(dim(getBrainRegions()), c(18, 5))
    expect_equal(
        dim(
            getAllGenes4BrainRegion(brainRegion = "Striatum", taxID = 10090)
        ),
        c(4386, 12)
    )
    expect_equal(
        dim(
            getGenes4BrainRegion(
                c(1, 15, 156, 1500, 3000, 7000),
                brainRegion = "Striatum",
                taxID = 10090
            )
        ),
        c(10, 12)
    )
    expect_equal(
        dim(
            getPPIbyIDs4BrainRegion(
                1:7,
                brainRegion = "Striatum",
                taxID = 10090,
                type = "limited"
            )
        ), c(11, 2)
    )
    expect_equal(
        dim(
            getPPIbyIDs4BrainRegion(
                1:7,
                brainRegion = "Striatum",
                taxID = 10090,
                type = "ind"
            )
        ), c(236, 2)
    )
})
test_that("Proper row number in compartments", {
    c <- getCompartments()
    expect_equal(dim(c), c(4, 3))
    expect_equal(dim(getAllGenes4Compartment(compartmentID = 1)), c(5568, 8))
    expect_equal(
        dim(
            getGenes4Compartment(
                c(1, 15, 156, 1500, 3000, 7000),
                compartmentID = 1
            )
        ), c(5, 8)
    )
    expect_equal(
        dim(
            getPPIbyIDs4Compartment(
                c(1, 15, 156, 1500, 3000, 7000),
                compartmentID = 1, type = "induced"
            )
        ), c(249, 2)
    )
    expect_equal(
        dim(
            getPPIbyIDs4Compartment(
                c(1, 15, 156, 1500, 3000, 7000),
                compartmentID = 1, type = "lim"
            )
        ), c(7, 2)
    )
})

test_that("Proper row number in diseases", {
    expect_equal(dim(getGeneDiseaseByIDs(c(48, 585, 710))), c(262, 4))
    expect_equal(dim(getGeneDiseaseByEntres(c(8573, 1742, 1739))), c(95, 4))
    expect_equal(dim(getGeneDiseaseByName(c("CASK", "DLG2", "DLG1"))), c(99, 4))
})

test_that("Proper row numbers in PPIs", {
    expect_equal(
        dim(getPPIbyIDs(c(48, 585, 710), type = "limited")),
        c(2, 2)
    )
    expect_equal(
        dim(getPPIbyIDs(c(48, 585, 710), type = "induced")),
        c(304, 2)
    )
    expect_equal(
        dim(getTableFromPPI(getPPIbyIDs(c(48, 585, 710), type = "limited"))),
        c(2, 16)
    )
    expect_equal(
        dim(getPPIbyIDs(c(48, 129, 975, 4422, 5715, 5835), type = "lim")),
        c(6, 2)
    )
    expect_equal(
        dim(
            getPPIbyName(c("CASK", "DLG4", "GRIN2A", "GRIN2B", "GRIN1"),
                type = "lim"
            )
        ),
        c(10, 2)
    )
    expect_equal(
        dim(getPPIbyEntrez(c(1739, 1740, 1742, 1741), type = "ind")),
        c(342, 2)
    )
})

test_that("Proper graph created from PPI", {
    g <- getIGraphFromPPI(
        getPPIbyIDs(c(48, 129, 975, 4422, 5715, 5835), type = "lim")
    )
    expect_s3_class(g, "igraph")
    expect_equal(igraph::vcount(g), 4)
    expect_equal(igraph::ecount(g), 6)
    expect_equal(
        igraph::vertex_attr_names(g),
        c(
            "name",
            "MGI",
            "HumanEntrez",
            "MouseEntrez",
            "RatEntrez",
            "HumanName",
            "MouseName",
            "RatName"
        )
    )
})
