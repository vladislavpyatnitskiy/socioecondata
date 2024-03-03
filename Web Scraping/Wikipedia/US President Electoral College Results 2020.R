library("rvest") # Library

representatives.us <- function(x){ # Party data by States and 2020 elections
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[8]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  w <- tab %>% html_nodes('tr') %>% html_nodes('th') %>% html_text()
  
  v <- NULL # store party info
  
  for (n in 0:(length(y) / 10)){ # Put data in columns
  
    v <- rbind.data.frame(v, y[6 + n * 10]) } 
  
  w <- gsub("[\n]", "", w[15:64]) # States info 
  v <- gsub("[\n]", "", v[1:50,]) # Party info
  
  b <- NULL # store party info
  
  for (n in 1:length(v)){ # Clean voted party data
  
    if (isFALSE(gsub("[[]", "", v[n]) == v[n])){
      
      b <- rbind(b, read.fwf(textConnection(v[n]), widths=c(nchar(v[n])-3,1),
                    colClasses = "character")[,-2]) }
    
    else { b <- rbind(b, v[n]) } } # Ready data frame
  
  df <- data.frame(w, b) # Join Data
  
  colnames(df) <- c("State", "Party") # Column names
  
  df # Display
}
representatives.us("2020_United_States_elections")
