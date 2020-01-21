#' add citation to rmarkdown document that will show as tooltip on hover
#' 
#' @name citippy
#' @rdname citippy
#' 
#' @param link Bibtex key
#' @param ref_path Default is references.bib.
#' @param pandoc Use pandoc to handle the citations and references (TRUE) or 
#' display citations with tooltips showing references on hover (FALSE) in html 
#' documents. Default is FALSE.
#' @param cite_style Options for styling of in-text citations. Options are 
#' "numeric", "authoryear". Default is set to "numeric".
#' @param ... Additional arguments to pass to `tippy::tippy`
#' 
#' @export
#' 
citippy <- function(link, 
                    ref_path = "references.bib", 
                    pandoc = FALSE, 
                    cite_style = "numeric",
                    textual = FALSE, ...){
  
  bib <- bib2df::bib2df(ref_path, separate_names = TRUE)
  bibref <- RefManageR::ReadBib(ref_path)
  
  if (pandoc==TRUE){
    
    RefManageR::BibOptions(cite.style = "pandoc")
    RefManageR::Cite(bibref, link, .opts = list(cite.style = "pandoc"),
                     textual = textual)
    
  } else {
    
    RefManageR::BibOptions(style = "html", hyperlink = FALSE,
                           cite.style = cite_style
    )
    
    tooltip_function <- function(x){
      
      
      
      if(dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
         nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])>3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                                  <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if(dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==2){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==1){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])>3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bib,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bib,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==2){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bib,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==1){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bib,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='https://doi.org/{dplyr::filter(bib,BIBTEXKEY==link[x])$DOI}' target='_blank'>DOI Link</a>]
                   <br>"
        )
      } 
    }
    
    tooltip <- paste(unlist(purrr::map(seq(1:length(link)), tooltip_function)), collapse='')
    
    
    tippy::tippy(text = RefManageR::Citep(bibref, link, 
                                          .opts = list(
                                            bibpunct = c("(", ")","[", "]", ","))
    ),
    interactive = TRUE,
    tooltip = tooltip
    , size = "large", ...
    )
  }
  
}

