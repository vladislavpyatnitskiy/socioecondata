library("rvest") # Library

life.expectancy.df <- function(x){ # Data Frame of Countries & life expectancy
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Form a Data Frame
  w <- NULL
  
  for (n in 0:(length(y) / 16)){ # Country and Life Expectancy Data
  
    if (isTRUE(gsub("[[]", "", y[1 + n * 16]) == y[1 + n * 16])){
      
      d <- read.fwf(textConnection(y[1 + n * 16]),
                    widths=c(nchar(1),nchar(y[1 + n * 16])),
                    colClasses = "character")[2]
      
      v <- rbind.data.frame(v, data.frame(d,
                                          as.numeric(gsub(",","",y[2+n*16]))))  
      
    } else if (isFALSE(gsub("[[]", "", y[1 + n * 16]) == y[1 + n * 16])){
      
      d <- read.fwf(textConnection(y[1+n*16]), widths=c(nchar(y[1+n*16])-3,1),
                    colClasses = "character")[,-2]
      
      d <- read.fwf(textConnection(d), widths=c(nchar(1),nchar(d)),
                    colClasses = "character")[2]
      
      w <- rbind.data.frame(w,
                            data.frame(as.data.frame(d),
                                       as.numeric(gsub(",","",y[2+n*16])))) } }
  
  colnames(w) <- c("Country", "Life Expectancy") # Column names
  colnames(v) <- c("Country", "Life Expectancy")
  
  df <- rbind(v, w) # Join two data frames
  
  df[order(-df$`Life Expectancy`), ] # Order in a descending way
  
  rownames(df) <- seq(nrow(df)) # numbers instead of row names
  
  df <- df[apply(df, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  df[order(-df$`Life Expectancy`), ] # Order in a descending way
  
  df # Display
}
life.expectancy.df("List_of_countries_by_life_expectancy")
