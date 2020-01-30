#' add citation to rmarkdown document that will show as tooltip on hover
#' 
#' @name citippy
#' @rdname citippy
#' 
#' @param link Bibtex key
#' @param bibdf dataframe of bibentries returned by bib2df::bib2df
#' @param bibref bibentries returned by RefManageR::ReadBib
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
                    bibdf = bibdf,
                    bibref = bibref,
                    pandoc, 
                    cite_style = "numeric",
                    textual = FALSE, ...){
  
  pandoc <- cittipy_options()$pandoc
  
  if (pandoc==TRUE & textual==FALSE){
    
    RefManageR::BibOptions(cite.style = "pandoc")
    RefManageR::Cite(bibref, link, 
                     .opts = list(cite.style = "pandoc"),
                     textual = TRUE)
    
  } else if (pandoc==TRUE & textual==TRUE){
    
    RefManageR::BibOptions(cite.style = "pandoc")
    RefManageR::Cite(bibref, link, 
                     .opts = list(cite.style = "pandoc"),
                     textual = FALSE)
    
  } else {
    
    RefManageR::BibOptions(style = "html", hyperlink = FALSE,
                           cite.style = cite_style
    )
    
    tooltip_function <- function(x){
      
      
      
      if(dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
         nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])>3){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                                  <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==3){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if(dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==2){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==1){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$JOURNAL}</em> ({dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR})<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])>3){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bibdf,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bibdf,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==3){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bibdf,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bibdf,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                 nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==2){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bibdf,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bibdf,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } else if (dplyr::filter(bibdf, BIBTEXKEY==link[x])$CATEGORY=="BOOK" & 
                nrow(dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]])==1){
        glue::glue("<strong>{dplyr::filter(bibdf, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bibdf,BIBTEXKEY==link[x])$BOOKTITLE},</em> 
                                  Chapter {dplyr::filter(bibdf,BIBTEXKEY==link[x])$CHAPTER}, pp. {dplyr::filter(bibdf,BIBTEXKEY==link[x])$PAGES}.
                                  {dplyr::filter(bibdf, BIBTEXKEY==link[x])$PUBLISHER}, {dplyr::filter(bibdf, BIBTEXKEY==link[x])$YEAR}<br>
                                  [<a href='{dplyr::filter(bibdf,BIBTEXKEY==link[x])$URL}' target='_blank'>Link</a>]
                   <br>"
        )
      } 
    }
    
    tooltip <- paste(unlist(purrr::map(seq(1:length(link)), tooltip_function)), collapse='')
    
    
    tippy::tippy(text = RefManageR::Cite(bibref, link, textual = textual,
                                          .opts = list(
                                            bibpunct = c("(", ")","[", "]", ","))
    ),
    interactive = TRUE,
    tooltip = tooltip
    , size = "large", ...
    )
  }
  
}

