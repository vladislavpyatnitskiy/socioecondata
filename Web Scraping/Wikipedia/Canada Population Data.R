library("rvest") # Library

population.ca <- function(x){ # Canadian provinces and their population
  
  y <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text()
  
  v <- data.frame(y[seq(from=1,to=length(y),by=6)],
                  as.numeric(gsub(",", "", y[seq(from=4,to=length(y),by=6)])))
  
  colnames(v) <- c("Province", "Population") # Column names
  
  v # Display
}
population.ca("List_of_Canadian_provinces_and_territories_by_gross_domestic_product")
