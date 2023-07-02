

fullprice <- c()
price_EUR <- c()
price_HRK <- c()
kilometers <- c()
year <- c()


for (i in 0:maxpage) {

ifelse(i=0, myurl <- mybaseurl, myurl <- str_c(mybaseurl, "?page=", i))
mypage <- read_html(myurl)

vauvauarticles <- mypage %>%
  html_elements(css=".EntityList-item--VauVau .entity-body")

regulararticles <- mypage %>%
  html_elements(css=".EntityList-item--Regular .entity-body")

#First go through Vau Vau articles

for (i in 1:length(vauvauarticles)) {
  
  fullprice <- c(fullprice,html_text(html_nodes(vauvauarticles, css=".price")[i])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  price_EUR <- c(price_EUR, substr(fullprice[i], 1, unlist(gregexpr("€", fullprice[i]))[1]-2)) %>% 
    ifelse(is.na(unlist(gregexpr("\\,", .))[1]), ., substr(., 1, str_length(.)-3)) %>% 
    as.numeric()*1000 
  price_HRK <- c(price_HRK, substr(fullprice[i], unlist(gregexpr("/", fullprice[i]))[1]+2, str_length(fullprice[i])-3)) %>%
    gsub("\\.", "", .) %>% 
    gsub("\\,","", .) %>% 
    as.numeric()/100

  description <- html_text(html_nodes(vauvauarticles, css=".entity-description-main")[i]) %>%
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  kilometers <- c(kilometers, substring(description, 1, unlist(gregexpr("\n", description))[1]-1)) %>% 
    substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
    strtoi()
  year <- c(year, substr(description, unlist(gregexpr("\n", description))[1], unlist(gregexpr("\\.", description))[1])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
    substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
}

#Then go through regular articles

###### This is in 2 loops instead of one as binding these two types of articles with c(,) creates "list" and gives following error
###### Error in UseMethod("xml_find_all") : no applicable method for 'xml_find_all' applied to an object of class "list"

for (i in (length(vauvauarticles)+1):(length(vauvauarticles)+length(regulararticles))) {
  
  fullprice <- c(fullprice, html_text(html_nodes(regulararticles, css=".price")[i-length(vauvauarticles)])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  price_EUR <- c(price_EUR, substr(fullprice[i], 1, unlist(gregexpr("€", fullprice[i]))[1]-2)) %>% 
    ifelse(is.na(unlist(gregexpr("\\,", .))[1]), ., substr(., 1, str_length(.)-3)) %>% 
    as.numeric()*1000 
  price_HRK <- c(price_HRK, substr(fullprice[i], unlist(gregexpr("/", fullprice[i]))[1]+2, str_length(fullprice[i])-3)) %>%
    gsub("\\.", "", .) %>% 
    gsub("\\,","", .) %>% 
    as.numeric()/100
  
  description <- html_text(html_nodes(regulararticles, css=".entity-description-main")[i-length(vauvauarticles)]) %>%
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  kilometers <- c(kilometers, substring(description, 1, unlist(gregexpr("\n", description))[1]-1)) %>% 
    substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
    strtoi()
  year <- c(year, substr(description, unlist(gregexpr("\n", description))[1], unlist(gregexpr("\\.", description))[1])) %>% 
    trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
    substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
}

}

df <- data.frame(year, kilometers, fullprice, price_EUR, price_HRK, row.names = NULL) %>% arrange(price_EUR)


