#' Utility function to create network from
#' \code{\link[synaptome.db]{synaptome.db}} data
#'
#' @param entrez vector of EntrezIDs for network vertices
#' @param LCC if TRUE only largest connected component is returned
#' @param simplify if TRUE loops and multiple edges will be removed
#'
#' @return network defined by the gene table
#' @export
#'
#' @examples
#' library(synaptome.db)
#' cid<-match('Presynaptic', getCompartments()$Name)
#' geneTable<-getAllGenes4Compartment(cid)
#' gg<-graphFromSynaptomeByEntrez(geneTable$HumanEntrez)
graphFromSynaptomeByEntrez<-function(entrez,LCC=TRUE,simplify=TRUE){
    geneTable<-findGenesByEntrez(entrez)
    gg<-buildFromSynaptomeGeneTable(geneTable)
    return(gg)
}

#' Utility function to create network from
#' \code{\link{synaptome.db}} data
#'
#' @param geneTable data.frame described in
#'        \code{\link{getGenesByID}}
#' @param LCC if TRUE only largest connected component is returned
#' @param simplify if TRUE loops and multiple edges will be removed
#'
#' @return network defined by the gene table
#' @importFrom BioNAR buildNetwork
#' @export
#'
#' @examples
#' library(synaptome.db)
#' cid<-match('Presynaptic', getCompartments()$Name)
#' geneTable<-getAllGenes4Compartment(cid)
#' gg<-graphFromSynaptomeGeneTable(geneTable)
graphFromSynaptomeGeneTable<-function(geneTable,LCC=TRUE,simplify=TRUE){
    p<-getPPIbyIDs(geneTable$GeneID, type = 'limited')
    aidx<-match(p$A, geneTable$GeneID)
    bidx<-match(p$B, geneTable$GeneID)
    # TODO: uncomment once BioNAR>= 1.3.7 in Bioconductor
    gg<-buildNetwork(data.frame(A=geneTable$HumanEntrez[aidx],
                                B=geneTable$HumanEntrez[bidx]))#,
                     #LCC=LCC,simplify=simplify)
    return(gg)
}
