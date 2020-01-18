#' citippy_ref
#' 
#' @rdname citippy_ref
#' @name citippy_ref
#' 
#' @param query Search string
#' 
#' @example
#' citippy_ref(query = "Midazolam for sedation before procedures")
#' @export

citippy_ref <- function(query){
  
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
  selection2 <- menu(c("Yes", "No"), title="Add to references.bib?")
  if(selection2==TRUE){
    RefManageR::WriteBib(bibentry_selection, append = TRUE)
  }
}
