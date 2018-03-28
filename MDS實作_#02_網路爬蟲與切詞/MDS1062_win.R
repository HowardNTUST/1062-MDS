# entrance questionaire functions
#' @param radians
#' @return value
#' @export


######################## 1.library loaded ##############################
function_load = function()
{
  
  # MDS實作_#02_網路爬蟲與切詞
  # if(!require(psych)){install.packages("psych"); require(psych)}
  if(!require(httr)){install.packages("httr"); require(httr)}  # 爬蟲 GET()
  if(!require(plotly)){install.packages("plotly"); require(plotly)}
  if(!require(rvest)){install.packages("rvest"); require(rvest)}
  if(!require(stringr)){install.packages("stringr"); require(stringr)}
  if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
  if(!require(data.table)){install.packages("data.table"); require(data.table)}
  if(!require(DT)){install.packages("DT"); require(DT)}
  if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}
  if(!require(parallel)){install.packages("parallel"); require(parallel)}
  if(!require(plyr)){install.packages("plyr"); require(plyr)}  
}

function_load()
# char_encode=URLencode(iconv('Maintenance_mask',to= 'UTF-8' ))

######################## 1. Request: GET - 請求產品面 ##############################]

# 記得要找出鎖定的product型號
urcos_product_crawler = function(product_code = 6879){

  # 這裡要變化--------
  num_product = product_code

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
    html_nodes("div.brand-name > a.uc-main-link") %>%
    html_text()

  # 系列
  series_unm_temp = doc_product %>%
    html_nodes('div.detail-label')%>%
    html_text()

  series = doc_product %>%
    html_nodes('div.detail-text > a.uc-main-link')%>%
    html_text() %>% .[grep('系列', series_unm_temp)]


  # 屬性
  # attribute = doc_product %>%
  #   html_nodes('div.detail-text > a.uc-main-link')%>%
  #   html_text()[5]
  # 屬性：如果遇到難抓取的，可以使用xpath的方法來抓取

  attribute_temp= doc_product %>%
    html_nodes('div.detail-text > span')%>%
    html_text()

  attribute_total_seq = sum(str_count(' > ', attribute_temp))

  attribute = doc_product %>%
    html_nodes('div.detail-text > a.uc-main-link')%>%
    html_text() %>%  .[grep('屬性', series_unm_temp)+attribute_total_seq]

  # 標籤
  # label = doc_product %>% html_nodes('div.detail-label')%>% html_node("#系列")
  #   html_text()
  #label_total_seq =  sum(str_count('、', attribute_temp) ) - (attribute_total_seq+ 3) - 1
  label =  doc_product %>%
    html_nodes('div.detail-text > a.uc-main-link') %>%
    html_text() %>% .[6:length(.)]
  label = paste(label, collapse = '、')

  # 指數
  rate = doc_product %>%
    html_nodes( 'div.score-txt') %>%
    html_text() %>% parse_number()

  # Y變數 - 分解為 人氣、升火、買過
  '要理解grep, str_split_fixed'
  y_all = doc_product %>%
    html_nodes('div.product-info-engagement-counts') %>%
    html_text()

  y_all_split = str_split_fixed(y_all, pattern = '/',3)

  popularity_num = grep('人氣', y_all_split)
  fire_num = grep('升火', y_all_split)
  purchase_num = grep('買過', y_all_split)

  popularity =y_all_split[popularity_num] %>% parse_number()
  fire=y_all_split[fire_num] %>% parse_number()
  purchase = y_all_split[purchase_num]%>% parse_number()

  # 容量數字
  capacity = doc_product %>%
    html_nodes('div.product-info-others > div.product-info-other > div.other-text') %>%
    html_text() %>% .[1]

  capacity_figure_tmp = capacity %>% parse_number()
  capacity_figure = ifelse( length(capacity_figure_tmp)==0,NA,capacity_figure_tmp)

  # 容量單位
  capacity_unit = ifelse( length(capacity_figure)==0,NA, gsub(capacity_figure,'',capacity))

  # 價格
  price = doc_product %>%
    html_nodes('div.other-text') %>%
    html_text()%>%grep('NT\\$', ., value = T) %>%   parse_number()

  # 商品說明
  description = doc_product %>%
    html_nodes('div.product-desc-content') %>%
    html_text()

  # 1.2 通路資訊 ####
  url_product_channel = sprintf('%s%s',url_product,'/sell-channels')
  doc_product_channel<- GET(url_product_channel) %>% content(encoding = "utf8")

  '不用xpath主要因為我們要統計次數'
  # channel = doc_product_channel %>%
  #   html_nodes(xpath = '/html/body/div[5]/div[3]/div[1]/div[5]/div[2]') %>%
  #   html_text()
  channel_online_temp = doc_product_channel %>%
    html_nodes('div.sell-channel-online-text') %>%
    html_text()

  channel_offline_temp = doc_product_channel %>%
    html_nodes('#uc-search-final-offline-channels > div.uc-sell-channel-online-lists') %>%
    html_text()

  offline_num =  c()
  online_num =  c()

  # channel_offline_temp 是 有字串
  if (length(channel_offline_temp)>0) {
    for (channel in channel_online_temp) {
      # 如果有在offline裡面的廠家，就是線下廠商
      if (grepl(channel , channel_offline_temp)) {
        offline_num[channel] =  c(channel)
      }else{
        online_num[channel] =  c(channel)
      }
    }
  }else{
    offline_num = NA
    online_num = channel_online_temp
  }

  channel_offline = paste(offline_num,collapse =  '、')
  channel_online = paste(online_num,collapse =  '、')
  channel_totol_figure = length(channel_online_temp)

  # 這裡要變化--------
  # make data frame
  df = data.frame(product_name = ifelse(length(product_name)==0,NA,product_name),
                  brand = ifelse(length(brand)==0,NA,brand),
                  description = ifelse(length(description)==0,NA,description), #0
                  series = ifelse(length(series)==0,NA,series),
                  attribute = ifelse(length(attribute)==0,NA,attribute),
                  label = ifelse(length(label)==0,NA,label),
                  rate = ifelse(length(rate)==0,NA,rate),
                  popularity = ifelse(length(popularity)==0,NA,popularity),
                  fire = ifelse(length(fire)==0,NA,fire),
                  purchase = ifelse(length(purchase)==0,NA,purchase),
                  capacity_figure = ifelse(length(capacity_figure)==0,NA,capacity_figure),
                  capacity_unit = ifelse(length(capacity_unit)==0,NA,capacity_unit),
                  price = ifelse(length(price)==0,NA,price),
                  channel_offline = ifelse(length(channel_offline)==0,NA,channel_offline),
                  channel_online = ifelse(length(channel_online)==0,NA,channel_online),
                  channel_totol_figure = ifelse(length(channel_totol_figure)==0,NA,channel_totol_figure))

  return( df  )
} # 可以ctrl + F1 來縮小function set
#
# error_list =list()
# tryCatch({
#


#   b - 1
# }, error = function(e) {
#
#   print(b )
#   error_list[b] <<- list(b)
#
# })



######################## 2. Request: GET - 個人評論面 ##############################
#' 2. Request: GET - 個人評論面

urcos_product_userComment = function(product_code = 84630){

  # 2.0 讓機器自動取得評論最後的頁數####
  num_product = product_code
  url_review=sprintf("https://www.urcosme.com/products/%s/reviews",num_product)
  doc_review<- GET(url_review) %>% content(encoding = "utf8")
  finalpage_test = doc_review %>%
    html_nodes('div.pagination > a') %>% html_attr("href")

  if (length(finalpage_test)==0) {
    finalpage = 1
  }else{
    finalpage = finalpage_test %>% str_split_fixed(., '=',2) %>% .[,2] %>% parse_number(.) %>% max(.)
  }

  # 2.1 取得所有user、comment url####
  '1 * D 維度'
  authorComment_url_list= list()
  pb = progress_bar$new(total = finalpage)
  print('取得user個人網頁連結 及 comment網頁連結中')
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

  return(author_url)
}



# 2.2 取得所有user comment該產品的資料####


# i 第幾個member資訊
urcos_product_userCommentInfo = function(i = 1, crawl_author_other_post =F ,
                                         crawl_author_other_post_url = "https://www.urcosme.com/reviews/1110291"){

  # 因為作者使用的圖文格式可能不同，需要使用trycatch來預防錯誤

  #i = 1
  if (crawl_author_other_post) { # 爬鎖定的作者其他文章
    doc_product_comment<- GET(crawl_author_other_post_url) %>% content(encoding = "utf8")

    product_code = doc_product_comment %>%
      html_nodes( 'ol > li > a') %>%
      html_attr('href') %>% grep('products',., value = T) %>% parse_number()
    
  }else{ # original author
    doc_product_comment<- GET(author_url$comment_url[i]) %>% content(encoding = "utf8")
  }

  #doc_product_comment<- GET('https://www.urcosme.com/reviews/567637') %>% content(encoding = "utf8")

  # 評論文字
  comment = doc_product_comment %>%
    html_nodes('div.review-content') %>%
    html_text()

  # 照片數量
  comment_img_num  = doc_product_comment %>%
    html_nodes('div.review-content > img') %>%
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
  comment_rate = doc_product_comment %>%
    html_nodes('div.author-score') %>%
    html_text() %>% parse_number()

  # 發表年月
  comment_publish = comment_popularity_temp[,1] %>%
    gsub('\\.','-',.) %>%
    gsub('發表.*','',.)%>%   parse_date()

  if (length(comment_publish)==0) {
    comment_publish= NA
  }
  
  x<-'\xE3\x83\xBB'
  
  # 作者膚質
  info_temp = doc_product_comment %>%
    html_nodes('div.author-info') %>%
    html_text()  %>% str_split_fixed(.,Encoding(x) <- "UTF-8",3)

  comment_author_skin =grep('肌膚',info_temp, value = T)

  # 作者歲數
  # author_age =grep('歲',info_temp, value = T) %>%
  #   gsub('歲.*','',.) %>% as.numeric()

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

  au = data.frame(
    comment_author_skin = ifelse(length(comment_author_skin)==0,NA,comment_author_skin),
    author_name = ifelse(length(author_name)==0,NA,author_name),
    author_id = ifelse(length(author_id)==0,NA,author_id),
    comment = ifelse(length(comment)==0,NA,comment),
    comment_img_num = ifelse(length(comment_img_num)==0,NA,comment_img_num),
    comment_recom = ifelse(length(comment_recom)==0,NA,comment_recom),
    comment_popularity = ifelse(length(comment_popularity)==0,NA,comment_popularity),
    comment_rate = ifelse(length(comment_rate)==0,NA,comment_rate),
    comment_publish =comment_publish,
    comment_effect = ifelse(length(comment_effect)==0,NA,comment_effect),
    comment_exp = ifelse(length(comment_exp)==0,NA,comment_exp),
    comment_season = ifelse(length(comment_season)==0,NA,comment_season),
    comment_envir = ifelse(length(comment_envir)==0,NA,comment_envir)
  )
  return(list(au, product_code))
}


# 找userProfile ####

urcos_product_userProfile = function(i = 1){

  # 作者id
  author_id = author_url$author_url[i] %>%gsub('/|:|\\.','',.) %>%  parse_number(.)

  # 性別
  #doc_author<- GET('https://www.urcosme.com/beauty_diary/290676/profile') %>% content(encoding = "utf8")
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
  for (topic in author_tem_topic) {
    g = html_text(topic)
    author_topic[g] = c( g )
  }

  author_topic = paste(author_topic, collapse = '、')

  # 得獎紀錄
  author_reward_num = doc_author %>%
    html_nodes('div.cnt-item') %>%
    html_text() %>% length()

  # 2.3取得author的個人網頁----
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

  userProfile = data.frame(
    author_id,
    author_sex ,
    author_skin,
    author_age ,
    author_sign,
    author_topic,
    author_reward_num,
    author_index,
    author_article_kind,
    author_article_num
  )



  return(userProfile)

}

# 額外心得網址：該消費者還有評論過哪些產品？
# 再接urcos_product_userComment 的function，就可以繼續爬心得
# 2.3取得author的個人網頁----

urcos_userPostsAll_url = function(i=1){

  doc_author_review <- GET(sprintf('%s?page=%s',author_url$author_url[i], 1)) %>% content(encoding = "utf8")
  # extract page number
  test_page= doc_author_review %>%
    html_nodes('div.pagination > a') %>% html_attr("href")

  if (length(test_page)==0) { #只有一頁
    finalpage= 1
  }else{
    finalpage = test_page  %>%
      str_split_fixed(., '=',2) %>% .[,2] %>% parse_number(.) %>% max(.)

  }

  # 開始爬該位author各種產品評論網址拉
  pagelist = list()
  pb = progress_bar$new(total = finalpage)
  print('開爬該位使用者評論過得產品網址')
  for (page in 1:finalpage) {
    doc_author_review <- GET(sprintf('%s?page=%s',author_url$author_url[i], page)) %>% content(encoding = "utf8")

    # 先取得
    pagelist[page] = list(
      doc_author_review %>%
        html_nodes('div.text-block > a') %>%
        html_attr('href'))
    pb$tick()
  }
  pageall = unlist(pagelist)
  pageall=pageall[!duplicated(pageall)]

  return(pageall)
}


#---------

# 取得使用者所有心得及產品文章 let's delve into user profile

urcos_userPostsAll_content = function(pageall=pageall, i=1, author_url=author_url){
  
  product_comment_n_info_list = list()
  error_list= list()
  print(sprintf('取得第 %s/%s 位使用者所有心得及產品文章', i, length(author_url$author_url)))

  pb = progress_bar$new(total = length(pageall))
  #length(pageall)
  for (review_by_author in 1:length(pageall)) {
    
    tryCatch({
      
      #product_code = review_by_author %>% parse_number()
      review_url = sprintf('https://www.urcosme.com%s', pageall[review_by_author])
      product_comment_df_temp = urcos_product_userCommentInfo(i = i, crawl_author_other_post =T ,crawl_author_other_post_url =review_url)
      
      # 產品評論 df -----------
      product_comment_df = product_comment_df_temp[[1]]
      
      # 產品url
      #product_url = sprintf('https://www.urcosme.com%s', product_comment_df_temp[[2]])
      product_code =  product_comment_df_temp[[2]]
      
      # 將產品url 輸入到 urcos_product_crawler for 產品基本資訊------
      # 產出product basic info df------
      product_info_df = urcos_product_crawler(product_code = product_code)
      
      
      # # 看有沒有error
      # product_info_df_error = product_info_df_temp[[2]]
      
      #review_url_page = GET(review_url) %>% content(encoding = "utf8")
      
      # 將產品評論 df 與 product basic info df 結合
      product_comment_n_info_list[review_url]  = list(cbind(product_comment_df, product_info_df))
      pb$tick()
      
    }, error = function(e) {
      cat('有錯！')
      print(review_by_author)
      error_list[review_by_author] <<- list(review_by_author)
      
    })
    
    
  }

  product_comment_n_info_df = ldply(product_comment_n_info_list, data.frame)

  return(list(product_comment_n_info_df, error_list))
}


# 最終極
# 輸入產品代號，就可以

urcos_crawler1 = function(product_code  = 6879){
  
  # 抓消費者個人網址 與 產品評論網址
  author_url = urcos_product_userComment(product_code = product_code)
  }

urcos_crawler = function(product_code  = 6879){ 

  # 抓消費者個人網址 與 產品評論網址
  #author_url = urcos_product_userComment(product_code = product_code)
  #author_url = author_url[2,]

  all_df_list = list()
  error_list = list()
  print('作者文章取用中')
  pb = progress_bar$new(total = length(author_url$author_url))

  for (i in 1:length(author_url$author_url)) {
      tryCatch({
      # 使用者的基本資料
      #userProfile = urcos_product_userProfile(i = 1)
      author_profile = urcos_product_userProfile(i = i)


      # 使用者評論過心得頁數
      pageall = urcos_userPostsAll_url(i = i)

      product_comment_n_info_df = urcos_userPostsAll_content(pageall=pageall, i=i,author_url=author_url)
      product_comment_n_info_df_error=product_comment_n_info_df[[2]] # error list
      product_comment_n_info_df=product_comment_n_info_df[[1]]

      author_midall = merge(author_profile, product_comment_n_info_df, by = c('author_id'))

      all_df_list[i] = list(author_midall)
      pb$tick()

    }, error = function(e) {

      cat('有錯！')
      print(author_url$author_url[i])
      error_list[author_url$author_url[i]] <<- list(author_url$author_url[i])
      error_list = unlist(error_list)
    })


    }

  # 最終整合
  
  #product_comment_n_info_df_error =unlist( product_comment_n_info_df_error)

  all_df= ldply(all_df_list, data.frame)

  return(list(all_df, error_list,product_comment_n_info_df_error))#
}
