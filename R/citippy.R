#' citippy
#' @name citippy
#' @rdname citippy
#' 
#' @param ref path to .bib reference file
#' @param pandoc logical but best to set as parameter in 
#' rmarkdown yaml
#' @param cite_style character string; bibliography style to use to 
#' generate citations.  
#' 
#' @export
#' 
citippy <- function(link, 
                    ref = "references.bib", 
                    pandoc = params$pandoc, 
                    cite_style = "numeric"){
  
  bib <- bib2df::bib2df(ref, separate_names = TRUE)
  bibref <- RefManageR::ReadBib(ref)
  
  if (pandoc==TRUE){
    
    RefManageR::BibOptions(cite.style = "pandoc")
    RefManageR::Citet(bibref, link, .opts = list(cite.style = "pandoc"))
    
  } else {
    
    RefManageR::BibOptions(style = "html", hyperlink = "to.doc",
                           cite.style = cite_style
    )
    
    tippy::tippy(text = RefManageR::Citep(bibref, link),
                 interactive = TRUE,
                 tooltip = if(dplyr::filter(bib, BIBTEXKEY==link)$CATEGORY=="ARTICLE" & 
                              nrow(dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]])>3){
                   glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link)$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link)$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link)$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link)$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link)$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link)$YEAR}<br>
                                  "
                   )
                 } else if (dplyr::filter(bib, BIBTEXKEY==link)$CATEGORY=="ARTICLE" & 
                            nrow(dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]])==3){
                   glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link)$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link)$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link)$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link)$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link)$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link)$YEAR}<br>
                                  "
                   )
                 } else if(dplyr::filter(bib, BIBTEXKEY==link)$CATEGORY=="ARTICLE" & 
                           nrow(dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]])==2){
                   glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link)$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link)$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link)$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link)$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link)$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link)$YEAR}<br>
                                  "
                   )
                 } else if (dplyr::filter(bib, BIBTEXKEY==link)$CATEGORY=="ARTICLE" & 
                            nrow(dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]])==1){
                   glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link)$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link)$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link)$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link)$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link)$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link)$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link)$YEAR}<br>
                                  "
                   )
                 }
    )
  }
  
}