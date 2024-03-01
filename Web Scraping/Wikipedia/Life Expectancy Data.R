library("rvest") # Library

life.expectancy.df <- function(x){ # Data Frame of Countries & life expectancy
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Form a Data Frame
  
  for (n in 0:(length(y) / 16)){ # Country and Life Expectancy Data
    
    v <- rbind(v, data.frame(y[1 + n * 16], as.numeric(y[2 + n * 16]))) }
  
  colnames(v) <- c("Country", "Life Expectancy") # Column Names
  rownames(v) <- seq(nrow(v)) # Numbers as row names
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
life.expectancy.df("List_of_countries_by_life_expectancy")
