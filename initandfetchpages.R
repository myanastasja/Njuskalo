#### SET UP
library(tidyverse) # Load core Tidyverse packages, including dplyr
library(rvest)     # Additional Tidyverse packages for web scraping
library(xml2)      # Package to work with XML files


#### SCRAPE - "skoda-octavia" to become input parameter in future upgrades

##### Until I figure out how to deal with captcha, I will manually copy/paste page source code into a local file

# mybaseurl <- 'https://www.njuskalo.hr/auti/skoda-octavia' 
# myurl <- mybaseurl
# myurl <- 'https://www.njuskalo.hr/auti/skoda-octavia?page=16'
myurl <- "skoda-octavia.html" ##### THIS IS FALLBACK TO FILE

mypage <- read_html(myurl)

### INITIAL PAGING CONTAINER
paging <- mypage %>% 
  html_elements(css=".PaginationContainer--top .Pagination-item") %>% html_text()

### OPTIONS
### A) paging container has "Sljedeća" as last button and has "x-y" as last-1 button
### B) paging container has "Sljedeća" as last button but does not have range "x-y" as last-1 button, but last page link
### C) paging container does not have "Sljedeća" as last button, this should mean that we're ont he last page and last button is last page link


### maxpage function:
### return list with 2 values
### max = max page number, if reached
### next = next paging array start number, if max not reached
  
ismaxpage <- function(paging) {

  if(str_detect(paging[length(paging)], "Sljede") && !(str_detect(paging[length(paging)-1], "-"))) 
    maxnext <- c(paging[length(paging)-1],NA)
  else if(!str_detect(paging[length(paging)], "Sljede"))
    maxnext <- c(paging[length(paging)], NA)
  else maxnext <- c(NA, substr(paging[length(paging)-1],1,unlist(gregexpr("-", paging[length(paging)-1])[1])-1))

  return(maxnext)
}
  
while(is.na(ismaxpage(paging)[1])) {
  rm(myurl)
  myurl <- str_c(mybaseurl, "?page=", ismaxpage(paging)[2])
  
  rm(paging)
  mypage <- read_html(myurl)
  paging <- mypage %>% 
    html_elements(css=".PaginationContainer--top .Pagination-item") %>% html_text()
}

maxpage <- ismaxpage(paging)[1] %>% strtoi()


