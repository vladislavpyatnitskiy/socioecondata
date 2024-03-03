library("rvest") # Library

population.us <- function(x){ # Get data of states and their population
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # variables to store data frames
  
  for (n in 0:(length(y) / 11)){ # Put data in columns
    
    d <- read.fwf(textConnection(y[1+n*11]), widths=c(nchar(y[1+n*11])-0,1),
                  colClasses = "character")[,-2]
    
    d <- read.fwf(textConnection(d), widths=c(nchar(1),nchar(d)),
                  colClasses = "character")[2]
    
    D <- read.fwf(textConnection(y[2+n*11]), widths=c(nchar(y[2+n*11])-1,1),
                  colClasses = "character")[,-2]
    
    v <- rbind.data.frame(v,data.frame(d,as.numeric(gsub(",", "", D)))) }
  
  colnames(v) <- c("State", "Population")
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA

  rownames(v) <- seq(nrow(v)) # numbers instead of row names
  
  v
}
population.us("List_of_U.S._states_and_territories_by_population")
