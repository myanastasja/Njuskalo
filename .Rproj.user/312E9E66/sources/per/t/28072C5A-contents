#### SET UP
library(tidyverse) # Load core Tidyverse packages, including dplyr
library(rvest)     # Additional Tidyverse packages for web scraping
library(xml2)      # Package to work with XML files


#### SCRAPE - "skoda-octavia" to become input parameter in future upgrades

##### Until I figure out how to deal with captcha, I will manually copy/paste page source code into a local file

# myurl <- 'https://www.njuskalo.hr/auti/skoda-octavia'
# myurl <- 'https://www.njuskalo.hr/auti/skoda-octavia?page=16'
myurl <- "skoda-octavia-16.html" ##### THIS IS FALLBACK TO FILE


 mypage <- read_html(myurl)

vauvauarticles <- mypage %>%
  html_elements(css=".EntityList-item--VauVau .entity-body")

regulararticles <- mypage %>%
  html_elements(css=".EntityList-item--Regular .entity-body")

#First go through Vau Vau articles
fullprice <- c()
price_EUR <- c()
price_HRK <- c()
description <- c()
kilometers <- c()
year <- c()

for (i in 1:length(vauvauarticles)) {
  
  fullprice[i] <- html_text(html_nodes(vauvauarticles, css=".price")[i]) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  price_EUR[i] <- substr(fullprice[i], 1, unlist(gregexpr("€", fullprice[i]))[1]-2) %>% ifelse(is.na(unlist(gregexpr("\\,", .))[1]), ., substr(., 1, str_length(.)-3)) %>% as.numeric()*1000 
  price_HRK[i] <- substr(fullprice[i], unlist(gregexpr("/", fullprice[i]))[1]+2, str_length(fullprice[i])-3) %>%
    gsub("\\.", "", .) %>% gsub("\\,","", .) %>% as.numeric()/100

  description[i] <- html_text(html_nodes(vauvauarticles, css=".entity-description-main")[i]) %>%
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  kilometers[i] <- substring(description[i], 1, unlist(gregexpr("\n", description[i]))[1]-1) %>% 
    substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
    strtoi()
  year[i] <- substr(description[i], unlist(gregexpr("\n", description[i]))[1], unlist(gregexpr("\\.", description[i]))[1]) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
    substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
  print(year[i])
}

#Then go through regular articles

###### This is in 2 loops instead of one as binding these two types of articles with c(,) creates "list" and gives following error
###### Error in UseMethod("xml_find_all") : no applicable method for 'xml_find_all' applied to an object of class "list"

for (i in (length(vauvauarticles)+1):(length(vauvauarticles)+length(regulararticles))) {
  
  fullprice[i] <- html_text(html_nodes(regulararticles, css=".price")[i-length(vauvauarticles)]) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  price_EUR[i] <- substr(fullprice[i], 1, unlist(gregexpr("€", fullprice[i]))[1]-2) %>% ifelse(is.na(unlist(gregexpr("\\,", .))[1]), ., substr(., 1, str_length(.)-3)) %>% as.numeric()*1000 
  price_HRK[i] <- substr(fullprice[i], unlist(gregexpr("/", fullprice[i]))[1]+2, str_length(fullprice[i])-3) %>%
    gsub("\\.", "", .) %>% gsub("\\,","", .) %>% as.numeric()/100
  
  description[i] <- html_text(html_nodes(regulararticles, css=".entity-description-main")[i-length(vauvauarticles)]) %>%
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  kilometers[i] <- substring(description[i], 1, unlist(gregexpr("\n", description[i]))[1]-1) %>% 
    substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
    strtoi()
  year[i] <- substr(description[i], unlist(gregexpr("\n", description[i]))[1], unlist(gregexpr("\\.", description[i]))[1]) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
    substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
}


df <- data.frame(year, kilometers, fullprice, price_EUR, price_HRK, row.names = NULL) %>% arrange(price_EUR)

inputyear <- 2017
df[df$year == inputyear,]
