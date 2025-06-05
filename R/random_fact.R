#' Random fact
#' 
#' This function provides the first or the first two paragraphs of a random wikipedia page.
#' @return Two paragraphs of a random wikipedia page
#' @example example.R
#' @export
random_fact<-function(){
  url <- "https://en.wikipedia.org/wiki/Special:Random" 
  webpage <- rvest::read_html(url)
  paragraphs <- webpage %>%
    rvest::html_nodes("p")
  if (length(paragraphs) >= 2) {
    first_paragraph_text <- paragraphs[1] %>%
      rvest::html_text(trim = TRUE) 
    second_paragraph_text <- paragraphs[2] %>%
      rvest::html_text(trim = TRUE)
    cat("Random fact: \n")
    cat(first_paragraph_text, "\n\n")
    cat("")
    cat(second_paragraph_text, "\n")
  } else if (length(paragraphs) == 1) {
    first_paragraph_text <- paragraphs[1] %>%
      rvest::html_text(trim = TRUE)
    cat("Random fact:\n")
    cat(first_paragraph_text, "\n")
  } else {
    cat("No paragraphs found on the page.\n")
  }
}
#