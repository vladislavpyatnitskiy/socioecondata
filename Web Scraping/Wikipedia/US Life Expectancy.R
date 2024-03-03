library("rvest") # Library

life.expectancy.us <- function(x){ # Data Frame of States & life expectancy
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  g <- tab %>% html_nodes('tr') %>% html_nodes('a') %>% html_text()
  
  v <- NULL # Form a Data Frame
  w <- NULL
  
  for (n in 0:(length(y) / 5)){ # States and Life Expectancy Data
    
    if (isTRUE(gsub("[[]", "", y[2 + n * 5]) == y[2 + n * 5])){
      
      d <- read.fwf(textConnection(y[2+n*5]), widths=c(nchar(y[2+n*5])-0,1),
                    colClasses = "character")[,-2]
      
      d <- read.fwf(textConnection(d), widths=c(nchar(1),nchar(d)),
                    colClasses = "character")[2]
      
      D <- read.fwf(textConnection(y[3+n*5]), widths=c(nchar(y[3+n*5])-0,1),
                    colClasses = "character")[,-2]
      
      v <- rbind.data.frame(v, data.frame(d, as.numeric(D)))  
      
    } else if (isFALSE(gsub("[[]", "", y[2+n*5]) == y[2+n*5])){
      
      d <- read.fwf(textConnection(y[2+n*5]), widths=c(nchar(y[2+n*5])-3,1),
                    colClasses = "character")[,-2]
      
      d <- read.fwf(textConnection(d), widths=c(nchar(1),nchar(d)),
                    colClasses = "character")[2]
      
      D <- read.fwf(textConnection(y[3+n*5]), widths=c(nchar(y[3+n*5])-2,1),
                    colClasses = "character")[,-2]
      
      w <- rbind.data.frame(w, data.frame(d, as.numeric(D))) } }
  
  colnames(w) <- c("State", "Life Expectancy") # Column names
  colnames(v) <- c("State", "Life Expectancy")
  
  df <- rbind(v, w) # Join two data frames
  
  df[order(-df$`Life Expectancy`), ] # Order in a descending way
  
  df <- df[!duplicated(df[c('State')]), ]
  
  df <- df[apply(df, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  rownames(df) <- seq(nrow(df)) # numbers instead of row names
                 
  df # Display
}
life.expectancy.us("List_of_U.S._states_and_territories_by_life_expectancy")
