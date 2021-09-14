#' synaptome.db: programmatic access to the Synaptic proteome database
#'
#' The package obtains a local copy of the Synaptic proteome database
#' from AnnotationHub package  \code{\link{synaptome.data}} and provides
#' a set of utility R functions to query and analyse its content.
#'
#' @docType package
#' @name synaptome.db
#'
#' @references
#' \insertRef{Sorokina:2021hl}{synaptome.db}
#' @importFrom Rdpack reprompt
"_PACKAGE"

# Update this function call
utils::globalVariables(c(
    "ID", "A", "B", "method", "type", "taxID", "HDOID",
    "GeneID", "PMID", "Paper", "Disease", "HumanEntrez",
    "HumanName", "MouseName", "MouseEntrez", "MGI",
    "BrainRegion", "Description", "LocalisationID",
    "SpeciesTaxID", "Chromosome", "Variant", "cDNAvariant",
    "ProteinVariant", "FunctionClass", "DENOVO", "SFARI",
    "RatEntrez", "RatName", "Localisation", "PaperPMID",
    "Year", "ClinVar", "dbconn"
))
