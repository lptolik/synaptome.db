#' synamptome.db: programmatic access to the Synaptic proteome database
#'
#' The package contains local copy of the Synaptic proteome database together
#' with a set of utility R functions to query and analyse its content.
#'
#' @docType package
#' @name synamptome.db
"_PACKAGE"

# Update this function call
utils::globalVariables(c(
    "ID","A",'B',"method",'type','taxID','HDOID',
    'GeneID',"PMID","Paper","Disease","HumanEntrez",
    "HumanName","MouseName","MouseEntrez","MGI",
    "BrainRegion","Description","LocalisationID",
    "SpeciesTaxID","Chromosome","Variant","cDNAvariant",
    "ProteinVariant","FunctionClass","DENOVO","SFARI",
    "ClinVar"))
