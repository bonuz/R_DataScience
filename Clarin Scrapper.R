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

Scrapper de Clarin.com!
-----------------------
## limpio variables
rm(list=ls())

## instalo librerias
install.packages('rvest')
library(rvest)

content <- read_html('https://www.clarin.com/')
## content <- read_html('https://www.clarin.com/economia/')

title <- content %>% html_nodes('h2') %>% html_text()
category <- content %>% html_nodes('p.section') %>% html_text()
volanta <- content %>% html_nodes('p.volanta') %>% html_text()

df <- data.frame(title = title, volanta = volanta, category = category)


https://datascienceplus.com/building-a-hacker-news-scraper-with-8-lines-of-r-code-using-rvest-library/

