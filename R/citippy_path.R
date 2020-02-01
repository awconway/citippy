#' add citation to rmarkdown document that will show as tooltip on hover
#' 
#' @name citippy_path
#' @rdname citippy_path
#' 
#' @param ref_path Path to your .bib file with bibtex entries
#' @example 
#' citippy_path(ref_path = "references.bib")
#' 
#' @export
#' 
citippy_path <- function(ref_path){
  .bibdf <<- bib2df::bib2df(ref_path, separate_names = TRUE)
  .bibref <<- RefManageR::ReadBib(ref_path)
}
