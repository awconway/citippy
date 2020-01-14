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
    
    tooltip_function <- function(x){
      
      
      
      if(dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
         nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])>3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]} et al.<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link[x])$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link[x])$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  "
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==3){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}, {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[3]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link[x])$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link[x])$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  "
        )
      } else if(dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==2){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]} & {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[2]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link[x])$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link[x])$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  "
        )
      } else if (dplyr::filter(bib, BIBTEXKEY==link[x])$CATEGORY=="ARTICLE" & 
                 nrow(dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]])==1){
        glue::glue("<strong>{dplyr::filter(bib, BIBTEXKEY==link[x])$TITLE}</strong><br>
                                  {dplyr::filter(bib, BIBTEXKEY==link[x])$AUTHOR[[1]]$full_name[1]}<br>
                                  <em>{dplyr::filter(bib,BIBTEXKEY==link[x])$JOURNAL},</em> 
                                  Vol {dplyr::filter(bib,BIBTEXKEY==link[x])$VOLUME}({dplyr::filter(bib,BIBTEXKEY==link[x])$NUMBER}), pp. {dplyr::filter(bib,BIBTEXKEY==link[x])$PAGES}. {dplyr::filter(bib, BIBTEXKEY==link[x])$YEAR}<br>
                                  "
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
    )
  }
  
}