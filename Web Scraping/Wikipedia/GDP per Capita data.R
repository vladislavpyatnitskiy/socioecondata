library("rvest") # Library

gdp.capita.df <- function(x){ # Get countries according to GDP per Capita
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Create lists to contain variables 
  
  l <- c("Europe", "Americas", "Africa", "Oceania", "Asia") # Regions
  
  for (n in 1:length(y)){ # Sort data by Country, Region and GDP per Capita
    
   if (any(l == y[n])){ if (isTRUE(y[n+1] == "—") && isFALSE(y[n+2] == "—")){
     
     d <- read.fwf(textConnection(y[n - 1]),
                   widths=c(nchar(1),nchar(y[n - 1])),
                   colClasses = "character")[2]
     
     D <- as.numeric(gsub(",", "", y[n + 2])) # Join data 
     
     v <- rbind.data.frame(v, data.frame(d, y[n], D))
     
   } else { d <- read.fwf(textConnection(y[n - 1]),
                          widths=c(nchar(1),nchar(y[n - 1])),
                          colClasses = "character")[2]
     
     D <- as.numeric(gsub(",", "", y[n + 1])) # Join data 
     
     v <- rbind.data.frame(v, data.frame(d, y[n], D)) } } }
  
  colnames(v) <- c("Country", "Region", "GDP per Capita") # Column names
  rownames(v) <- seq(nrow(v)) 
  
  v <- v[apply(v, 1, function(row) all(row != "—")),] # Remove zeros & NA
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
gdp.capita.df("List_of_countries_by_GDP_(nominal)_per_capita")
