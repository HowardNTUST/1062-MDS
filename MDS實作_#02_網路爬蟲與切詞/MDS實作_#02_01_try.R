#安裝
install.packages(c('httr', 'rvest', 'XML', 'magrittr', 'DT', 'stringr', 'jsonlite', 'RCurl','data.table', 'progress', 'plyr'))

#載入基本類別庫
library(httr)
library(rvest) 

#下載網頁檔案
url_product="https://www.urcosme.com/products/6879"
doc_product<- GET(url_product) %>% content(encoding = "utf8")

#皆層定位
product_name = doc_product %>% 
  html_nodes("div.headline-title.product-name") %>% 
  html_text()


#管線
library(rvest) 
a = '123'
print(a)

a %>% print


# 屬性
attribute = doc_product %>% 
  html_nodes('div.detail-text > a.uc-main-link')%>% 
  html_text() 
attribute = attribute[5]
