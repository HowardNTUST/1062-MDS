
#' ---
#' title: "Family mart GIS"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---

######################################################
#                                                    #
#                   全家便利商店地資 crawler         #
#                                                    #
######################################################

#author: Howard Chung
#original date: 2017/3/1
#updated date: 2017/3/4 
#pupose: user agent, and query function
#version: v 0.1.3
#update news: 
#v 0.1.1 - 2017/3/4 : user agent added to scrape contents of anti-crawling web
#v 0.1.2 - 2017/3/4 : add functions to v 0.1.1
#v 0.1.3 - 2017/4/10 : simple regex added to line 66

#'[全家便利商店](http://www.family.com.tw/marketing/inquiry.aspx)

knitr::opts_knit$set(root.dir = '..')


######################## 1.library loaded ##############################
#'# 1.library
rm(list = ls())
library(magrittr) #for pipeline 
library(httr) #mainly for web crawling
suppressPackageStartupMessages(library(jsonlite)) #reading json data format
library(DT) # build interactive table

######################## 2. Request: GET crawler made ##############################
#' #2. Request: GET crawler made
url <- "http://api.map.com.tw/net/familyShop.aspx"
# make a GET request
res <- GET(url = url,
           add_headers(Referer = "http://www.family.com.tw/marketing/inquiry.aspx"), # remember to add Referer
           user_agent("Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36"), # user_agent is no need to use, only for demo
           # query allows you to append the list components to url
           query = list(
             searchType = "ShopList",
             type = "",
             city = "高雄市",
             area = "三民區",
             road = "",
             fun = "showStoreList",
             key = "6F30E8BF706D653965BDE302661D1241F8BE9EBC"
           ))
######################## 3. parser : extract data wanted ##############################
#'#4. parser : extract data wanted
# get and parse data
doc_str <- content(res, as = "text") %>% 
  `Encoding<-`("UTF-8")
#' ## Clean data
# clean data by regex
dat <- doc_str %>%
  gsub("^.*\\(|\\).*", '', .)  %>%  #delete showStoreList() charaters; regex +$ shouldn't be used, because previous units may not include )
  #^.*=texts before ( ; \\( = start from ( ; \\) = start from ) ; .* = after )
  fromJSON()

#this will do as well to delete texts before (  and after ), then delete ( )
dat_same= doc_str %>% #json can not include vector ( ), so ( ) have to be deleted
  sub("^[^\\[]+", "", .) %>% #delete showStoreList(
  sub("[^\\]]*$", "", .) %>%  #delete showStoreList); regex +$ shouldn't be used, because previous units may not include )
  fromJSON() # convert json to data frame

#trial for deleting ( )
a=c("fu([8665])554565")
a %>% sub("^[^\\[]+", "", .) %>%   sub("[^\\]]*$", "", .) 
a %>%  gsub("^.*\\(|\\).*", '', .) #this one is the best
######################## 4.function : build up API ##############################
#' #4.function : build up API
get_family_stores <- function(city, area){
  url <- "http://api.map.com.tw/net/familyShop.aspx"
  # make a GET request
  res <- GET(url = url,
             add_headers(
               Referer = "http://www.family.com.tw/marketing/inquiry.aspx"
             ), # remember to add Referer
             # query allows you to append the list components to url
             query = list(
               searchType = "ShopList",
               type = "",
               city = city,
               area = area,
               road = "",
               fun = "showStoreList",
               key = "6F30E8BF706D653965BDE302661D1241F8BE9EBC"
             ))
  # get and parse data
  doc_str <- content(res, as = "text") %>% 
    `Encoding<-`("UTF-8")
  
  # return NULL if no data was retrieved
  if (doc_str == "showStoreList([])") {
    message(sprintf("No data in area %s in city %s", area, city))
    return(NULL)
  }
  
  # clean data by regex
  dat <- doc_str %>%
    sub("^[^\\[]*", "", .) %>%
    sub("[^\\]]*$", "", .) %>% 
    fromJSON() 
  
  return(dat)
}

######################### 5.function : Retrieve Meta-Data through function  ##############################
#'#5.function : Retrieve Meta-Data through function
dat <- get_family_stores(city = "高雄市", area = "三民區")
DT::datatable(dat)
