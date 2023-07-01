#### SET UP
library(tidyverse) # Load core Tidyverse packages, including dplyr
library(rvest)     # Additional Tidyverse packages for web scraping
library(xml2)      # Package to work with XML files


#### SCRAPE - "skoda-octavia" to become input parameter in future upgrades
myurl <- 'https://www.njuskalo.hr/auti/skoda-octavia'

mypage <- read_html(myurl)

vauvauarticles <- mypage %>%
  html_elements(css=".EntityList-item--VauVau .entity-body")

regulararticles <- mypage %>%
  html_elements(css=".EntityList-item--Regular .entity-body")

#First go through Vau Vau articles
price <- c()
kilometers <- c()

for (i in 1:length(vauvauarticles)) {
  
  price <- c(price, html_text(html_nodes(vauvauarticles, css=".price")[i])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  
  description <- html_text(html_nodes(vauvauarticles, css=".entity-description-main")[i])
  thiskilometers <- substring(trimws(description),1,unlist(gregexpr("\n",trimws(description))[1])-1)
  kilometers <- c(kilometers, thiskilometers)
  
  print(price[i])
  print(kilometers[i])
}

#Then go through regular articles

###### This is in 2 loops instead of one as bindindg these two types of articles with c(,) creates "list" and gives following error
###### Error in UseMethod("xml_find_all") : no applicable method for 'xml_find_all' applied to an object of class "list"

for (i in (length(vauvauarticles)+1):(length(vauvauarticles)+length(regulararticles))) {
  
  price <- c(price, html_text(html_nodes(regulararticles, css=".price")[i-length(vauvauarticles)])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  
  description <- html_text(html_nodes(regulararticles, css=".entity-description-main")[i-length(regulararticles)])
  thiskilometers <- substring(trimws(description),1,unlist(gregexpr("\n",trimws(description))[1])-1)
  kilometers <- c(kilometers, thiskilometers)
  
  print(price[i])
  print(kilometers[i])
  
}


df <- data.frame(price,row.names = NULL)

