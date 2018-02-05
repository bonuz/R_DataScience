Scrapper de Hacker News!
------------------------
install.packages('rvest')
library(rvest)

content <- read_html('https://news.ycombinator.com/')
title <- content %>% html_nodes('a.storylink') %>% html_text()

link_domain <- content %>% html_nodes('span.sitestr') %>% html_text()
score <- content %>% html_nodes('span.score') %>% html_text()
age <- content %>% html_nodes('span.age') %>% html_text()

df <- data.frame(title = title, link_domain = link_domain, score = score, age = age)
## hay un error con score que trae 29 registros, lo saco del data.frame

Scrapper de lanacion.com.ar!
-----------------------
## limpio variables
rm(list=ls())

## instalo librerias
install.packages('rvest')
library(rvest)

content <- read_html('https://www.lanacion.com.ar/')

## obtengo titulares
title <- content %>% html_nodes('h2') %>% html_text()

## convierto a dataframe
title <- as.data.frame(title)

## limpio saltos de linea/trim
title <- trimws(title[,1])



## sub("\r\n", "", title[1]) >> reemplaza \r\n por ""
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
# este trim limpia tambien \r\n
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## tambien se puede usar trimws
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/trimws.html





https://datascienceplus.com/building-a-hacker-news-scraper-with-8-lines-of-r-code-using-rvest-library/

