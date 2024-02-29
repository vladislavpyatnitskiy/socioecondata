library("rvest") # Library

gdp.capita.df <- function(x){ # Get countries according to GDP per Capita
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Create lists to contain variables & Subset excess row
  
  l <- c("Europe", "Americas", "Africa", "Oceania", "Asia")
  
  for (n in 1:length(y)){ # Sort data by Country, Region and GDP per Capita
    
   if (any(l == y[n])){ if (isTRUE(y[n+1] == "—") && isFALSE(y[n+2] == "—")){
     
     v <- rbind(v, cbind(y[n-1], y[n], y[n+2])) } else {
       
       v <- rbind(v, cbind(y[n-1], y[n], y[n+1])) } } }
  
  colnames(v) <- c("Country", "Region", "GDP per Capita") # Column names
  rownames(v) <- seq(nrow(v)) 
  
  v <- v[apply(v, 1, function(row) all(row != "—")),] # Remove zeros & NA
  
  v # Display
}
gdp.capita.df("List_of_countries_by_GDP_(nominal)_per_capita")
