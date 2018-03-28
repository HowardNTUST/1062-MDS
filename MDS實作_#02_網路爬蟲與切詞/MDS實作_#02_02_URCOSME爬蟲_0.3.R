#' ---
#' title: "URCOSEME 保養面膜Maintenance mask"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---

# author : Howard Chung

######################## 1.library loaded ##############################
library(httr) # GET
library(rvest) # 
library(stringr) 
# library(data.table)
library(RCurl)
library(tidyverse)
# library(parallel)
library(plyr)
library(progress)


######################## 1. Request: GET - 請求產品面 ##############################
num_product = 6879
url_product=sprintf("https://www.urcosme.com/products/%s",num_product)
doc_product<- GET(url_product) %>% content(encoding = "utf8")

#extract data wanted

# 1.1 選定之產品資訊 ####

# 產品名稱
product_name = doc_product %>% 
  html_nodes("div.headline-title.product-name") %>% 
  html_text()

# 品牌名稱
brand = doc_product %>% 
  html_nodes("div.brand-name") %>% 
  html_text()

# 屬性
attribute = doc_product %>% 
  html_nodes('div.detail-text > a.uc-main-link')%>% 
  html_text() 
attribute = attribute[5]


# 標籤
fourlabel = doc_product %>% html_nodes('div.uc-product-detail') 
taglabel = fourlabel[4] %>% html_nodes('.uc-main-link')  %>% html_text()

#上下是同樣的結果   在 %>% 中 .是
taglabel = doc_product %>% html_nodes('div.uc-product-detail')   %>% 
  .[4] %>% html_nodes('.uc-main-link')  %>% html_text()



#合併為一個字串
taglabel = paste(taglabel,collapse =  '、')

# 指數
rate = doc_product %>%
  html_nodes('.score-txt')%>% 
  html_text() %>% parse_number()

# parse_number() 是用來把所有字串中只保留文字

# Y變數 - 分解為 人氣、升火、買過
'要理解grep, str_split_fixed'
y_all = doc_product %>%
  html_nodes('.product-info-engagement-counts') %>% 
  html_text()

# 利用strsplit 切割字串
y_all_split = strsplit(y_all, '/')
# 取出資料
y_all_split = y_all_split[[1]]

# 看一下結構
y_all_split

#取出各個資料
popularity = y_all_split[[1]][1] %>% parse_number()
fire = y_all_split[[1]][2] %>% parse_number()
purhase = y_all_split[[1]][3] %>% parse_number()

# 容量數字
# <div class="product-info-others">
#   <div class="product-info-other">
#     <div class="other-label">容量</div>
#     <div class="other-text">430ml</div>
#   </div>
#   <div class="product-info-other">
#     <div class="other-label">價格</div>
#     <div class="other-text">NT$ 1980</div>
#   </div>
# </div>

# 先到product-info-others
product.info.others = doc_product %>% html_nodes('.product-info-others > .product-info-other')

# 容量字串
capacity_text =  product.info.others[1] %>% 
  html_nodes(' .other-text') %>% 
  html_text() 

# 容量
capacity_figure = capacity_text  %>% parse_number()

# 容量單位 利用正規表達式過濾字串 gsub
capacity_unit = gsub('[0-9]+', '', capacity_text)

# 價格字串
price_text  =  product.info.others[2] %>% 
  html_nodes(' .other-text') %>% 
  html_text()  

price = price_text %>% parse_number()

#貨幣
currency =  gsub('[0-9]+', '', price_text)

# 商品說明
description = doc_product %>%
  html_nodes('.product-desc-content') %>% 
  html_text()



# 1.2 通路資訊 ####
url_product_channel = sprintf('%s%s',url_product,'/sell-channels')
doc_product_channel<- GET(url_product_channel) %>% content(encoding = "utf8")

#線上通路
channel_online_temp = doc_product_channel %>%
  html_nodes('div.sell-channel-online-text') %>% 
  html_text()

#合併為一個字串
channel_online = paste(channel_online_temp,collapse =  '、')

#線下通路 修改了  這裡行不同
channel_offline_temp = doc_product_channel %>%
  html_nodes('#off-line-channel-block') %>% 
  html_text()

# https://www.urcosme.com/products/6879/get_sell_channels?area=northern

# 北部
url_product_channel = sprintf('%s%s',url_product,'/get_sell_channels?area=northern')
doc_product_channel_n<- GET(url_product_channel) %>% content(encoding = "utf8")

#線下通路 修改了
channel_offline_n = doc_product_channel_n %>%
  html_nodes('.sell-channel-content') %>% 
  html_text()

# https://www.urcosme.com/products/6879/get_sell_channels?area=southern
# 南部
url_product_channel = sprintf('%s%s',url_product,'/get_sell_channels?area=southern')
doc_product_channel_s<- GET(url_product_channel) %>% content(encoding = "utf8")

#線下通路 修改了
channel_offline_s = doc_product_channel_n %>%
  html_nodes('.sell-channel-content') %>% 
  html_text()




#存為 Dataframe

exdataframe=data.frame(num_product=num_product,product_name=product_name,brand=brand,
                       attribute=attribute,taglabel=taglabel,rate=rate,
                       popularity=popularity,fire=fire,purhase=purhase,
                       capacity_figure=capacity_figure, price=price,
                       description=description,channel_offline_n=channel_offline_n,
                       channel_offline_s=channel_offline_s,channel_online=channel_online )



#############   同時爬下多個產品 #############  

num_product_list = c(6879,6880)
dt = data.frame()

for (num_product in num_product_list) {
  # 程式碼(26:176)與上方完全一樣
  url_product=sprintf("https://www.urcosme.com/products/%s",num_product)
  doc_product<- GET(url_product) %>% content(encoding = "utf8")
  
  #extract data wanted
  
  # 1.1 選定之產品資訊 ####
  
  # 產品名稱
  product_name = doc_product %>% 
    html_nodes("div.headline-title.product-name") %>% 
    html_text()
  
  # 品牌名稱
  brand = doc_product %>% 
    html_nodes("div.brand-name") %>% 
    html_text()
  
  # 屬性
  attribute = doc_product %>% 
    html_nodes('div.detail-text > a.uc-main-link')%>% 
    html_text() 
  attribute = attribute[5]
  
  
  # 標籤
  fourlabel = doc_product %>% html_nodes('div.uc-product-detail') 
  taglabel = fourlabel[4] %>% html_nodes('.uc-main-link')  %>% html_text()
  
  #上下是同樣的結果   在 %>% 中 .是
  taglabel = doc_product %>% html_nodes('div.uc-product-detail')   %>% 
    .[4] %>% html_nodes('.uc-main-link')  %>% html_text()
  
  
  
  #合併為一個字串
  taglabel = paste(taglabel,collapse =  '、')
  
  # 指數
  rate = doc_product %>%
    html_nodes('.score-txt')%>% 
    html_text() %>% parse_number()
  
  # parse_number() 是用來把所有字串中只保留文字
  
  # Y變數 - 分解為 人氣、升火、買過
  '要理解grep, str_split_fixed'
  y_all = doc_product %>%
    html_nodes('.product-info-engagement-counts') %>% 
    html_text()
  
  # 利用strsplit 切割字串
  y_all_split = strsplit(y_all, '/')
  # 取出資料
  y_all_split = y_all_split[[1]]
  
  # 看一下結構
  y_all_split
  
  #取出各個資料
  popularity = y_all_split[[1]][1] %>% parse_number()
  fire = y_all_split[[1]][2] %>% parse_number()
  purhase = y_all_split[[1]][3] %>% parse_number()
  
  # 容量數字
  # <div class="product-info-others">
  #   <div class="product-info-other">
  #     <div class="other-label">容量</div>
  #     <div class="other-text">430ml</div>
  #   </div>
  #   <div class="product-info-other">
  #     <div class="other-label">價格</div>
  #     <div class="other-text">NT$ 1980</div>
  #   </div>
  # </div>
  
  # 先到product-info-others
  product.info.others = doc_product %>% html_nodes('.product-info-others > .product-info-other')
  
  # 容量字串
  capacity_text =  product.info.others[1] %>% 
    html_nodes(' .other-text') %>% 
    html_text() 
  
  # 容量
  capacity_figure = capacity_text  %>% parse_number()
  
  # 容量單位 利用正規表達式過濾字串 gsub
  capacity_unit = gsub('[0-9]+', '', capacity_text)
  
  # 價格字串
  price_text  =  product.info.others[2] %>% 
    html_nodes(' .other-text') %>% 
    html_text()  
  
  price = price_text %>% parse_number()
  
  #貨幣
  currency =  gsub('[0-9]+', '', price_text)
  
  # 商品說明
  description = doc_product %>%
    html_nodes('.product-desc-content') %>% 
    html_text()
  
  
  
  # 1.2 通路資訊 ####
  url_product_channel = sprintf('%s%s',url_product,'/sell-channels')
  doc_product_channel<- GET(url_product_channel) %>% content(encoding = "utf8")
  
  #線上通路
  channel_online_temp = doc_product_channel %>%
    html_nodes('div.sell-channel-online-text') %>% 
    html_text()
  
  #合併為一個字串
  channel_online = paste(channel_online_temp,collapse =  '、')
  
  #線下通路 修改了  這裡行不同
  channel_offline_temp = doc_product_channel %>%
    html_nodes('#off-line-channel-block') %>% 
    html_text()
  
  # https://www.urcosme.com/products/6879/get_sell_channels?area=northern
  
  # 北部
  url_product_channel = sprintf('%s%s',url_product,'/get_sell_channels?area=northern')
  doc_product_channel_n<- GET(url_product_channel) %>% content(encoding = "utf8")
  
  #線下通路 修改了
  channel_offline_n = doc_product_channel_n %>%
    html_nodes('.sell-channel-content') %>% 
    html_text()
  
  # https://www.urcosme.com/products/6879/get_sell_channels?area=southern
  # 南部
  url_product_channel = sprintf('%s%s',url_product,'/get_sell_channels?area=southern')
  doc_product_channel_s<- GET(url_product_channel) %>% content(encoding = "utf8")
  
  #線下通路 修改了
  channel_offline_s = doc_product_channel_n %>%
    html_nodes('.sell-channel-content') %>% 
    html_text()
  
  
  
  
  #存為 Dataframe
  
  exdataframe=data.frame(num_product=num_product,product_name=product_name,brand=brand,
                         attribute=attribute,taglabel=taglabel,rate=rate,
                         popularity=popularity,fire=fire,purhase=purhase,
                         capacity_figure=capacity_figure, price=price,
                         description=description,channel_offline_n=channel_offline_n,
                         channel_offline_s=channel_offline_s,channel_online=channel_online )
  
  dt=rbind(dt,exdataframe)
  
}

#查看結果!!!
View(dt)



######################## 以下是額外贈送               #############################
######################## 2. Request: GET - 個人評論面 ##############################
#' 2. Request: GET - 個人評論面 
num_product = 6879
url_review=sprintf("https://www.urcosme.com/products/%s/reviews",num_product)
doc_review<- GET(url_review) %>% content(encoding = "utf8")
finalpage = doc_review %>%
  html_nodes('div.pagination > a') %>% html_attr("href") %>% 
  str_split_fixed(., '=',2) %>% .[,2] %>% parse_number(.) %>% max(.)


# 2.1 取得所有user、comment url####
'1 * D 維度'
authorComment_url_list= list()
pb = progress_bar$new(total = finalpage)
for (page in 1:finalpage) {
  # 取得page資訊
  url_review=sprintf("https://www.urcosme.com/products/%s/reviews?page=%s",num_product, page)
  doc_review<- GET(url_review) %>% content(encoding = "utf8")
  
  # 取得author url
  author_url = doc_review %>%
    html_nodes('a') %>% 
    html_attr("href") %>% grep('beauty_diary',.,value = T)
  
  author_url = sprintf('https://www.urcosme.com%s',author_url )
  
  # comment url
  comment_url = doc_review %>%
    html_nodes('a.review-content-top') %>% 
    html_attr("href")
  
  comment_url = sprintf('https://www.urcosme.com%s',comment_url )
  authorComment_url_list[page] = list(data.frame(
    author_url=author_url,
    comment_url=comment_url,
    stringsAsFactors = F
  ))
  
  pb$tick()
}

# 將list合併
author_url = ldply(authorComment_url_list, data.frame)

# 2.2 取得所有user comment該產品的資料####
i = 1
doc_product_comment<- GET(author_url$comment_url[i]) %>% content(encoding = "utf8")
#doc_product_comment<- GET('https://www.urcosme.com/reviews/567637') %>% content(encoding = "utf8")

# 評論文字
comment = doc_product_comment %>%
  html_nodes('div.review-content') %>% 
  html_text()

# 照片數量
comment_img_num  = doc_product_comment %>%
  html_nodes('div.review-content > img') %>% 
  html_attr('src') %>% length(.)
comment_img_num  = doc_product_comment %>%
  html_nodes('div.review-content') %>% 
  html_attr('src') %>% length(.)

# 推薦人數
comment_recom  = doc_product_comment %>%
  html_nodes('span.count') %>% 
  html_text() %>% as.numeric()

# 人氣
comment_popularity_temp = doc_product_comment %>%
  html_nodes('div.review-date-and-pageview') %>% 
  html_text() %>% str_split_fixed(.,'/',2)

comment_popularity = comment_popularity_temp[,2] %>% 
  parse_number()

# 評分
doc_product_comment %>%
  html_nodes('div.author-score') %>% 
  html_text()

# 發表年月
comment_publish = comment_popularity_temp[,1] %>% 
  gsub('\\.','-',.) %>% 
  gsub('發表.*','',.)%>%   parse_date()

# 作者膚質

info_temp = doc_product_comment %>%
  html_nodes('div.author-info') %>% 
  html_text() %>% str_split_fixed(.,'・',3)

author_skin =grep('肌膚',info_temp, value = T)

# 作者歲數
author_age =grep('歲',info_temp, value = T) %>% 
  gsub('歲.*','',.) %>% as.numeric()

# 作者
author_name =info_temp[1]

# 作者id
author_id = author_url$author_url[i] %>%gsub('/|:|\\.','',.) %>%  parse_number(.)

# 效果
comment_effect = doc_product_comment %>%
  html_nodes('div.review-effects') %>% 
  html_text()

# 體驗方式
comment_exp_temp = doc_product_comment %>%
  html_nodes('div.other-attributes') %>% 
  html_text()

comment_exp = grep('體驗方式', comment_exp_temp, value = T) %>% gsub('體驗方式：','',.)

# 使用季節
comment_season = grep('使用季節', comment_exp_temp, value = T) %>% gsub('使用季節：','',.)

# 使用環境
comment_envir = grep('使用環境', comment_exp_temp, value = T) %>% gsub('使用環境：','',.)


# 2.2 取得所有user的資料####
'
單一row：
可能的Y變數：人氣、膚質、評分、推薦
可能的X變數：膚質、發表年（針對人氣要做平移處理）、內文
基本資料：人名、性別、星座、年齡、關注課題、活力指數、生火、關於我、個人特質

多row：
購買過得產品所有資訊（該產品評論先別列入）

購入品不能當作Y僅能當作X，因為購入

備註：使用者心得
'

# 作者id
author_id = author_url$author_url[i] %>%gsub('/|:|\\.','',.) %>%  parse_number(.)

# 性別
doc_author<- GET(gsub('reviews','profile',author_url$author_url[i])) %>% content(encoding = "utf8")
author_temp = doc_author %>%
  html_nodes('div.mbd-aut-right') %>% 
  html_text() 

author_sex = author_temp[1]
author_skin = author_temp[2]
author_age = author_temp[3] %>% parse_number()
author_sign = author_temp[4] 

# 關注議題
author_tem_topic =  doc_author %>%
  html_node('div.mbd-bdft-body') %>%
  html_nodes('div.mbd-bdft-item')

author_topic = c()
for (i in author_tem_topic) {
  g = html_text(i)
  author_topic[g] = c( g )
}

author_topic = paste(author_topic, collapse = '、')

# 得獎紀錄
author_reward_num = doc_author %>%
  html_nodes('div.cnt-item') %>%
  html_text() %>% length()

# 2.3取得author的個人網頁----
i = 1
doc_author_review <- GET(author_url$author_url[i]) %>% content(encoding = "utf8")

# 活力指數
author_index =  doc_author_review %>%
  html_nodes('div.as-score-text') %>% 
  html_text() %>% parse_number() 

# 作者文章種類篇數
author_article_kind = doc_author_review %>%
  html_nodes('div.op-stat-info') %>% 
  html_text() %>% gsub('篇','、', .) %>% 
  substr(., 1, nchar(.)-1)

# 全部心得數目
author_article_num = grep('全部心得', strsplit(author_article_kind,'、')[[1]], value = T) %>% 
  parse_number()

# 評論過的產品名稱（可來做segmentation）as a field
doc_author_review %>%
  html_nodes('div.text-block > a') %>% 
  html_attr('href')


# 所有心得評論
