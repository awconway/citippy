#' use crossref to find a citation and add to references.bib file
#' 
#' @rdname citippy_ref
#' @name citippy_ref
#' 
#' @param query Search string
#' @param ref_path Default is references.bib
#' @export

citippy_ref <- function(query, ref_path = "references.bib"){
  
  res <- rcrossref::cr_works(query = query)
  res1 <- res$data %>% 
    dplyr::select(container.title, title, doi)
  author_last_name <- function(x){
    paste(unlist(res$data$author[[x]][,2]), collapse=', ')
  }
  authors <- data.frame(purrr::map_chr(seq(1:nrow(res$data)), author_last_name))
  colnames(authors) <- "authors"
  res1$authors <- authors$authors
  print(dplyr::select(res1[1:5,], container.title, title, authors, doi))
  selection <- readline(prompt = "Select the reference you want:")
  bibentry_selection <- RefManageR::GetBibEntryWithDOI(res1[selection,]$doi)
  print(bibentry_selection)
  selection2 <- menu(c("Yes", "No"), title=glue::glue("Add to {ref_path}?"))
  if(selection2==TRUE){
    RefManageR::WriteBib(bibentry_selection, file = ref_path, append = TRUE)
    bib <- bib2df::bib2df(ref_path)
    print(glue::glue("`r citippy('{tail(bib$BIBTEXKEY, n=1)}')`"))
    selection3 <- menu(c("Yes", "No"), 
    title="Add citation to current cursor position in active source editor?")
    if(selection3==TRUE){
      context <- rstudioapi::getSourceEditorContext()
      id <- context$id
      rstudioapi::insertText(text = glue::glue(
        "`r citippy('{tail(bib$BIBTEXKEY, n=1)}')` "), id = id)
    }
  }
}
