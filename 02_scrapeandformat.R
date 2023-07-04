
id <- c()
fullprice <- c()
price_EUR <- c()
price_HRK <- c()
kilometers <- c()
year <- c()


for (i in 0:maxpage) {
  print(i)
  waittime <- runif(1, 2, 8)
  print(waittime)
  Sys.sleep(waittime)
  ifelse(i==0, myurl <- mybaseurl, myurl <- str_c(mybaseurl, "?page=", i))
  mypage <- read_html(myurl)

  vauvauarticles <- mypage %>%
    html_elements(css=".EntityList-item--VauVau .entity-body")
  
  regulararticles <- mypage %>%
    html_elements(css=".EntityList-item--Regular .entity-body")
  
  
  # regularids <- html_nodes(regulararticles, css=".entity-title .link") %>% 
  #   html_attr(.,"name")
  # 
  # vauvauids <- html_nodes(vauvauarticles, css=".entity-title .link") %>% 
  #   html_attr(.,"name")
  
  #First go through Vau Vau articles
  if(length(vauvauarticles)>0) {
    for (i in 1:length(vauvauarticles)) {
  
      iditer <- html_nodes(vauvauarticles, css=".entity-title .link")[i] %>% 
        html_attr(.,"name")
      id <- c(id, iditer)
      fullpriceiter <- html_text(html_nodes(vauvauarticles, css=".price")[i]) %>% 
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
      fullprice <- c(fullprice, fullpriceiter)
      price_EURiter <- substr(fullpriceiter, 1, unlist(gregexpr("€", fullpriceiter))[1]-2) %>% 
        ifelse(unlist(gregexpr("\\,", .))[1]==-1, ., substr(., 1, str_length(.)-3)) %>% as.numeric()*1000 
      price_EUR <- c(price_EUR, price_EURiter)
      price_HRKiter <- substr(fullpriceiter, unlist(gregexpr("/", fullpriceiter))[1]+2, str_length(fullpriceiter)-3) %>%
        gsub("\\.", "", .) %>% gsub("\\,","", .) %>% as.numeric()/100
      price_HRK <- c(price_HRK, price_HRKiter)
      
      description <- html_text(html_nodes(vauvauarticles, css=".entity-description-main")[i]) %>%
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
      kilometersiter <- substring(description, 1, unlist(gregexpr("\n", description))[1]-1) %>% 
        substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
        strtoi()
      kilometers <- c(kilometers, kilometersiter)
      yeariter <- substr(description, unlist(gregexpr("\n", description))[1], unlist(gregexpr("\\.", description))[1]) %>% 
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
        substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
      year <- c(year, yeariter)
    }
  }
  
  #Then go through regular articles
  
  ###### This is in 2 loops instead of one as binding these two types of articles with c(,) creates "list" and gives following error
  ###### Error in UseMethod("xml_find_all") : no applicable method for 'xml_find_all' applied to an object of class "list"
  
  if(length(regulararticles)>0) {
    for (i in (length(vauvauarticles)+1):(length(vauvauarticles)+length(regulararticles))) {
  
      iditer <- html_nodes(regulararticles, css=".entity-title .link")[i-length(vauvauarticles)] %>% 
        html_attr(.,"name")
      id <- c(id, iditer)
      fullpriceiter <- html_text(html_nodes(regulararticles, css=".price")[i-length(vauvauarticles)]) %>% 
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
      fullprice <- c(fullprice, fullpriceiter)
      price_EURiter <- substr(fullpriceiter, 1, unlist(gregexpr("€", fullpriceiter))[1]-2) %>% 
        ifelse(unlist(gregexpr("\\,", .))[1]==-1, ., substr(., 1, str_length(.)-3)) %>% as.numeric()*1000 
      price_EUR <- c(price_EUR, price_EURiter)
      price_HRKiter <- substr(fullpriceiter, unlist(gregexpr("/", fullpriceiter))[1]+2, str_length(fullpriceiter)-3) %>%
        gsub("\\.", "", .) %>% 
        gsub("\\,","", .) %>% 
        as.numeric()/100
      price_HRK <- c(price_HRK, price_HRKiter)
      
      description <- html_text(html_nodes(regulararticles, css=".entity-description-main")[i-length(vauvauarticles)]) %>%
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
      kilometersiter <- substring(description, 1, unlist(gregexpr("\n", description))[1]-1) %>% 
        substring(.,unlist(gregexpr(",", .))[1]+2,str_length(.)-3) %>%
        strtoi()
      kilometers <- c(kilometers, kilometersiter)
      yeariter <- substr(description, unlist(gregexpr("\n", description))[1], unlist(gregexpr("\\.", description))[1]) %>% 
        trimws(., which = c("both", "left", "right"), whitespace = "[ \t\r\n]") %>%
        substr(., unlist(gregexpr(":", .))[1]+2, str_length(.)-1)
      year <- c(year, yeariter)
    }
  }
}

df <- data.frame(id, year, kilometers, fullprice, price_EUR, price_HRK, row.names = NULL) %>% 
  arrange(price_EUR) %>% 
  distinct()

# 
# for (i in 0:maxpage) {
#   print(i)
#   waittime <- runif(1, 2, 8)
#   print(waittime)
#   Sys.sleep(waittime)
# }

