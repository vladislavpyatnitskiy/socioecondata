library("rvest") # Library

state.income <- function(x){ # Get States data according to Household Income
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[10]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # variables to store data frames
  
  for (n in 0:(length(y) / 13)){ # Put data in columns
    
    d <- read.fwf(textConnection(y[2+n*13]), widths=c(nchar(y[2+n*13])-1,1),
                  colClasses = "character")[,-2]
    
    D <- as.numeric(gsub("[$/,]", "", y[3 + n * 13]))
    
    v <- rbind.data.frame(v, data.frame(d, gsub("[$/,]", "", y[3 + n * 13]))) }
  
  colnames(v) <- c("State", "Household income")
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  rownames(v) <- seq(nrow(v)) # numbers instead of row names
  
  v # Display
}
state.income("Household_income_in_the_United_States")
