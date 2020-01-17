#' citippy_doi
#' 
#' @rdname citippy_doi
#' @name citippy doi
#' 
#' @param doi doi string
#' 
#' @export



citippy_doi <- function(doi){
  
  successes <- logical(length(doi))
  for (i in seq_along(doi)){
    temp <- httr::GET(httr::modify_url('https://doi.org/', path = doi[i]),
                      config = list(followlocation = TRUE),
                      httr::add_headers(Accept = "application/x-bibtex"))
    if (!httr::http_error(temp)){
      clipr::write_clip(httr::content(temp, as = "text", encoding = "UTF-8"))
      cat(httr::content(temp, as = "text", encoding = "UTF-8"))
      message("The bibtex entry above has been copied to your clipboard")
      successes[i] <- TRUE
      ## if (is.raw(temp))
      ##   temp <- rawToChar(temp)
    }
  }
  if (!all(successes)){
    failures <- paste(sQuote(doi[!successes]), collapse = ", ")
    message(gettextf("unable to retrieve bibliographic data for %s%s",
                     "the following supplied DOIs: ",
                     failures))
  }
}

citippy_doi("10.1136/heartjnl-2013-305289")
