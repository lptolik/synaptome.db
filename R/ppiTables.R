##### Use cases 5 Extract the PPIs for my list of genes #####
#' Prepare query for AllMutationAllPapers table
#'
#' @return tbl_lazy
#' @importFrom dplyr tbl select filter pull collect distinct
getPPIQuery<-function(){
    #  gns<-get_dbconn() %>% dplyr::tbl("AllPpiAllPapers") %>%
    gns<-get_dbconn() %>% dplyr::tbl("PPI") %>%
        dplyr::select(
            # ID,#ppiID,
            A,
            B) %>% distinct
            #,
            # method,
            # type,
            # taxID)
    return(gns)
}

#' Extract the PPIs for my list of genes defined by GeneID
#'
#' @param ids Gene IDs
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
#' @importFrom dplyr tbl select filter pull collect
#' @examples
#' t <- getPPIbyIDs(c(48, 585, 710), type='limited') #(16 rows)
#' t <- getPPIbyIDs(c(48, 585, 710), type='induced') #306 rows
getPPIbyIDs <- function(ids, type = c('induced', 'limited')) {
    netType <- match.arg(type)
    gns <- switch (
        netType,
        induced = getPPIQuery() %>%
            dplyr::filter(A %in% ids | B %in% ids),
        limited = getPPIQuery() %>%
            dplyr::filter(A %in% ids & B %in% ids)
    )
    df <- gns %>% collect
    return(df)
}

#' Extract the PPIs for my list of genes defined by Entrez IDs
#'
#' @param entrez Entrez IDs
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
getPPIbyEntrez<-function(entrez, type=c('induced','limited')){
    ids<-getGeneIdByEntrez(entrez)
    df<-getPPIbyIDs(ids,type)
    return(df)
}

#' Extract the PPIs for my list of genes defined by Gene name
#'
#' @param name Gene names
#' @param type type of the PPI network
#'
#' @return data.frame
#' @export
getPPIbyName<-function(name, type=c('induced','limited')){
    ids<-getGeneIdByName(name)
    df<-getPPIbyIDs(ids,type)
    return(df)
}

#' Get table representation of the PPI.
#'
#' @param ppi PPI \code{data.frame} with columns A and B, obtaioed
#' from functions like \code{\link{getPPIbyName}}
#'
#' @return table with the following columns added for both interactors (A and B):
#' \itemize{
#' \item{GeneID internal database ID}
#' \item{MGI MGI ID}
#' \item{HumanEntrez Human Entrez ID}
#' \item{MouseEntrez Mouse Entrez ID}
#' \item{HumanName Human gene name}
#' \item{MouseName Mouse gene name}
#' \item{RatEntrez Rat Entrez ID}
#' \item{Rat Name Rat gene name}
#' }

#' @export
#' @importFrom dplyr inner_join
#'
#' @examples
#' tbl<-getTableFromPPI(getPPIbyIDs(c(48, 585, 710), type='limited'))
getTableFromPPI<-function(ppi){
    aTbl<-getGenesByID(ppi$A)
    names(aTbl)<-paste0(names(aTbl),'.A')
    bTbl<-getGenesByID(ppi$B)
    names(bTbl)<-paste0(names(bTbl),'.B')
    res<- ppi %>%
        dplyr::inner_join(aTbl,by=c('A'='GeneID.A')) %>%
        dplyr::inner_join(bTbl,by=c('B'='GeneID.B'))
    return(res)
}

#' Get Igraph representation of PPI
#'
#' @param ppi PPI \code{data.frame} with columns A and B, obtaioed
#' from functions like \code{\link{getPPIbyName}}
#'
#' @return
#' @export
#' @import igraph
#'
#' @examples
#'
getIGraphFromPPI<-function(ppi){
    nids<-unique(c(ppi$A,ppi$B))
    nodes<-getGenesByID(nids)
    g<-graph_from_data_frame(ppi,directed = FALSE,vertices = nodes)
}
