library("rvest") # Library

population.ca <- function(x){ # Canadian provinces and their population
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # variables to store data frames
  
  for (n in 0:(length(y) / 6)){ # Put data in columns
    
    d <- y[1 + n * 6] # Provinces names
  
    d <- read.fwf(textConnection(d), widths = c(nchar(1), nchar(d)),
                  colClasses = "character")[2]
    
    D <- as.numeric(gsub(",", "", y[4 + n * 6])) # Population data
    
    v <- rbind.data.frame(v, data.frame(d, D)) } # Join data
  
  colnames(v) <- c("Province", "Population") # Column names
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  rownames(v) <- seq(nrow(v)) # numbers instead of row names
  
  v # Display
}
population.ca("List_of_Canadian_provinces_and_territories_by_gross_domestic_product")
