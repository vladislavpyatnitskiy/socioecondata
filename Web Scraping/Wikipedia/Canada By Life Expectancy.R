library("rvest") # Library

life.exp.ca <- function(x){ # Canadian Provinces by life expectancy
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Form a Data Frame
  
  for (n in 0:(length(y) / 8)){ # Provinces and Life Expectancy Data
    
    d <- read.fwf(textConnection(y[2+n*8]), widths = c(nchar(y[2+n*8]) - 0, 1),
                  colClasses = "character")[,-2]
    
    if (isFALSE(gsub("[[]", "", y[2 + n * 8]) == y[2 + n * 8])) {
      
      d <- read.fwf(textConnection(d), widths=c(nchar(d) - 3, 1),
                    colClasses = "character")[,-2] }
    
    d <- read.fwf(textConnection(d), widths = c(nchar(1), nchar(d)),
                  colClasses = "character")[2]
      
    D <- read.fwf(textConnection(y[4+n*8]), widths=c(nchar(y[4+n*8]) - 0, 1),
                  colClasses = "character")[,-2]
      
    v <- rbind.data.frame(v, data.frame(d, as.numeric(D))) } 
  
  colnames(v) <- c("Province", "Life Expectancy") # Column names
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  rownames(v) <- seq(nrow(v)) # numbers instead of row names
  
  v
}
life.exp.ca("List_of_Canadian_provinces_and_territories_by_life_expectancy")
