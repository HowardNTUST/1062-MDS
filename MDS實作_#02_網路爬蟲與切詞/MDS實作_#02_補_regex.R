
#' ---
#' title: "ptt"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---

#author: Howard Chung
#original date: 2017/4/7
#updated date: - 
#pupose: regex, auto-crawling
#version: v 0.1.1
#update news: 
#2017/4/7 : write regex example and auto-crawling function API

#' -Site [ptt八卦版](https://www.ptt.cc/bbs/Gossiping/index.html)


knitr::opts_chunk$set(comment = "..")

#'#1.library 
######################## 1.library loaded ##############################

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(stringr)
  library(dplyr)
  library(data.table)
  library(DT)
  library(RCurl)
})

#'#2. Request: GET crawler made
######################## 2. Request: GET crawler made ##############################
#'the method to extract contents when passing cookies
gossip <- GET("https://www.ptt.cc/bbs/Gossiping/index.html",
              set_cookies(over18="yes" ))$content %>% read_html
#'#3. parser 
######################## 3. parser : extract data wanted ##############################
#'title
gossip_titles <- gossip %>% 
  html_nodes("div.title") %>% 
  html_text %>% 
  iconv(from="utf8", to="utf8") 

#'clean title
gossip_titles_cleansed <- gsub("\n\t*", '', gossip_titles)

#'#4. Regex : grep
######################## 4. Regex : grep ##############################
#'search for titles with “問卦” only
grep("[問卦]", gossip_titles_cleansed, value=TRUE) #extract contents
gossip_titles_cleansed[grep("[問卦]", gossip_titles_cleansed)] #extract vector index

#' function of ^ and $
#^ limits the begining character
#$ limits the ending character
#delete reply symbol
grep("^\\[問卦]", gossip_titles_cleansed, value=TRUE)

#only reply article
grep("^Re: ", gossip_titles_cleansed, value=TRUE)
gossip_titles_cleansed[grep("^Re: ", gossip_titles_cleansed)]

#titles with ending question mark
grep("？$", gossip_titles_cleansed, value=TRUE)

#titles with any English alphabet
grep("[a-zA-Z]", gossip_titles_cleansed, value=TRUE)

#'*, +, ?
#*: the previous unit must repeat 0 or many times
#+: the previous unit must repeat 1 or many times
#?: the previous unit must repeat 0 or 1 times
test_str <- c("hello world", "你好啊", "\n") 
grep("h*", test_str, value=TRUE)
grep("h+", test_str, value=TRUE)

#[]  matches any number or char
grep("[a-z]", test_str, value=TRUE)
grep("[0-9]", test_str, value=TRUE)

#'grouper
#grouper (): 
#to group multiple characters as one unit such that…
#all other metas will operate on the entire grouped char
#useful for pattern match and extraction
test_str2 <- c("hello", "olleho", "hellohello")
test_str2 %>% grep("^(hello)?$", ., value=TRUE)
test_str2 %>% grep("^(hello)*$", ., value=TRUE)

#'escaper
#escaper: \
#to make its following unit as-is
grep("^\\L", c("Love You", "damn You", "Lure You", "Howard Likes you"), value=TRUE)
grep("\\You", c("Love You", "damn You", "Lure You", "Howard Likes you"), value=TRUE)

#'#5. Regex : gsub
######################## 5. Regex : gsub ##############################

# 簡單範例
gsub(pattern = '學長', '學妹', '學長我愛你')
'[]'
gsub("].*", '', gossip_titles_cleansed)
gsub("^.*\\[", '', gossip_titles_cleansed)

#\\ 的內容escape掉，就不會倍取代；.*後面及前面所有內容取代
gsub("\\].*|^.*\\[", '', gossip_titles_cleansed) 
gsub("^.*\\[|\\].*", '', gossip_titles_cleansed) 

#'#5. Regex : Parse_number and parse_date
######################## 5. Regex : Parse_number and parse_date ##############################
a= '售價100'
gsub('售價', '', a) %>% as.numeric()
str(gsub('售價', '', a))

library(tidyverse)
parse_number(a)
str(parse_number(a))


a = '我們的一次交往 2017-01-25' %>% #parse_number()
  str_split_fixed(.,'\\-',3)

a[,1] = a[,1]  %>%  parse_number()

a = paste(a, collapse = '-') %>%  readr::parse_date()
a = '^大好不好家好 我很好'

str_split_fixed(a, '好',3)[-3] %>% paste(.,collapse = '')

grep(pattern = '\\^大',a, value = T)

#easy to extract contexts
library(stringr)
str_split_fixed(gossip_titles_cleansed,'\\]', n=2)
