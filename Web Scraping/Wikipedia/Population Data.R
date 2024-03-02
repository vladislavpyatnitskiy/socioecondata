library("rvest") # Library

population.df <- function(x){ # Get data of countries and their population
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  m <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # variables to store data frames
  w <- NULL
  
  for (n in 0:(length(m) / 6)){ # Put data in columns
    
    if (isTRUE(gsub("[[]", "", m[7 + n * 6]) == m[7 + n * 6])){
      
      v <- rbind.data.frame(v, data.frame(m[7 + n * 6],
                                          as.numeric(gsub(",","",m[11+n*6]))))  
      
    } else if (isFALSE(gsub("[[]", "", m[7 + n * 6]) == m[7 + n * 6])){
      
      d <- read.fwf(textConnection(m[7+n*6]), widths=c(nchar(m[7+n*6])-3,1),
                    colClasses = "character")[,-2]
      
      w <- rbind.data.frame(w,
                            data.frame(as.data.frame(d),
                                       as.numeric(gsub(",","",m[11+n*6])))) } }
  
  colnames(w) <- c("Country", "Population") # Column names
  colnames(v) <- c("Country", "Population")
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  df <- rbind(v, w) # Join two data frames
  
  df[order(-df$Population), ] # Order in a descending way
  
  rownames(df) <- seq(nrow(df)) # numbers instead of row names
  
  df # Display
}
population.df("List_of_countries_by_population_(United_Nations)")
