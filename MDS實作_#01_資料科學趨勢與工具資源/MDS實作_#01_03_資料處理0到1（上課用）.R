
#' ---
#' title: "資料處理0到1"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---


######################## 1.檢視資料結構 ##############################
#'# 1.資料結構

# 設定
setwd('/home/slave1/git/1062-NTUST-MDS/MDS實作_#01_資料科學趨勢與工具資源')

# 載入built-in dataset
data("iris")

# 這是什麼資料?
?iris

# 看df的前六筆資料
head(iris) 

# 看df的後六筆資料
tail(iris) 

# 看df的所有欄位名稱
names(iris) 

#statistical summary
#可以算出所有欄位的百分位數及平均數
summary(iris) 

# 看資料屬性
str(iris)

######################## 2.製作 Data frame與其常用性質 ##############################
#'# 2.製作 Data frame(df)
'
Data frame資料框：大家可以想成Excel的表格
儲存的形式大致有：
1.Factor：因為電腦不吃文字（character），所以通常會轉換成factor，以1,2,3...的數字代表之，統計來說，就是隨機變數的概念
2.num：數值變數，通常機器學習等計算都是要用數值變數來計算
3.Character：文字
'

# 製作 df 第一種方式
店家 = c('行銷A') 
星期 = c('mon','tue','wed','thu','fri') 
營收 = c(27.75252, 25.04549, 19.87010 ,25.47285 ,23.00714)
df <- data.frame(店家,星期, 營收)
str(df)
# 'data.frame':	5 obs. of  3 variables:
# $ 店家: Factor w/ 1 level "行銷A": 1 1 1 1 1
# $ 星期: Factor w/ 5 levels "fri","mon","thu",..: 2 4 5 3 1
# $ 營收: num  27.8 25 19.9 25.5 23

# 製作 df 第二種方式
df2 = data.frame(店家 = '行銷B',
                   星期 = c('mon','tue','wed','thu','fri') ,
                   營收 =c(21.75757 ,23.54225, 21.84539 ,22.46933 ,22.68742)
)
str(df2)



######################## 3.列row/欄column選擇 ##############################
#'# 3.列row/欄column選擇

# 選擇 df[row列, column欄] 列與欄位
df[3, 1] # row 3, col 1
df[2] # col 2 
df[c(2,5), 2] # row 2,5 and col 2
df[-3] # remove col 3 


# 選欄位「內」數值 與 增加欄位
'
資料「運算」上通常會使用這種方法進行資料整理
單位：欄位內的變數
常用：新增欄位或轉變數值時
'
df$營收 #最常用
df[ ,'營收']
df[['營收']]
df$how = df$營收 * 12

# 常用：新增欄位或轉變數值時
length(df[,'營收'])
df$轉換後營收 = log(df[,'營收']) # or log(df$營收)
df
as.character(df$轉換後營收)
df$轉換後營收[2]


# 選、增欄位
'
「資料結構處理」上通常會使用這種方法進行資料整理
單位：欄
常用：欄位合併
'
df['營收']
length(df['營收'])
df$轉換後營收 = log(df['營收'])
as.character(df$轉換後營收) #轉換時會有問題，
str(df)

# 常用：欄位合併
cbind(df2,df['營收'])
cbind(df2,df[,'營收']) #這個不行？！ 當然可以，不過變數名稱會亂掉

# 刪除欄位 NULL
length(df[,'營收'])
df$轉換後營收 = log(df[,'營收']) # 先新增欄位
df$轉換後營收 = NULL # 刪除範例
df$how = NULL
######################## exercise ##############################
#'# exercise
'
1.選擇df星期三wed整「列row」的資料表
2.選擇df星期三wed與星期五fri的資料表
3.將df與df2的營收相加，取log，最後在df新增一個「營收加總」的欄位
4.將df的營收加總去除
'


######################## 4.資料表的命名與合併 ##############################
#'# 4.資料表的命名與合併

# rbind: row bind
'將df與df2上下合併'
all_df = rbind(df, df2)

# 重新命名欄位
names(df)[3] = '營收'
names(df2)[3] = '營收'

# cbind: column bind
cbind(df, df2) # 太多不需要的欄位
cbind(df, df2['營收']) # 僅擷取需要的欄位

# 資料表合併 與 排序 
'
merge常見的使用狀況有二
1.整合不同來源的資料表
2.資料表的key值排序紊亂時，用merge可直接整合需合併的資料表 -> 最常見

在Excel中，就是vlookup的功能～，但是相對Excel來說簡單很多
'

#  重新創一個資料表
df_traff =  df[c(1, 2)]
set.seed(123)
df_traff$人流  = round(rnorm(5, 100,15), 0 )

'狀況1.：整合不同來源的資料表'
head(df)
head(df_traff)
df_combine = merge(df, df_traff, by = c('店家','星期')) # key是店家與星期

'狀況2.：資料表的key值排序紊亂時，用merge可直接整合需合併的資料表'
# 排序的指令，先假設讓星期由字母小道大排序

#   店家  星期 人流
# 1 行銷A  mon   92
# 2 行銷A  tue   97
# 3 行銷A  wed  123
# 4 行銷A  thu  101
# 5 行銷A  fri  102
df_traff = df_traff[order(df_traff$星期), ] # 使用Row的關係在於，我們要用Row來移動，而非用橫欄

# 發現兩資料表的key順序不同，但可以使用merge就不會有所影響 
head(df)
head(df_traff)
df_combine=merge(df, df_traff,  by = c('店家','星期')) # key是店家與星期


######################## 5.wide data 與 long data 之間的轉換 ##############################
#'# 5.wide data 與 long data 之間的轉換
# [https://www.tidyverse.org/packages/] 
'
論視覺化，long data 是ggplot最常使用的形式
'

# 載入 tidyverse，沒安裝的話，請 install.packages('tidyverse')
library(tidyverse)

# long data to wide data 
# all_df = rbind(df, df2)
wide_df = spread(data = all_df, key = 店家, value =  營收)

# wide data data to long data
long_df = gather(data = wide_df,key = 星期, value = '營收')
names(long_df)[2] = '店家'


######################## 6.wide/long data 的視覺化 ##############################
#'# 6.wide/long data 的視覺化

# wide data的應用 - 雷達圖 if(!require(radarchart)){install.packages("radarchart")}
library(radarchart)
chartJSRadar(wide_df,  showToolTipLabel=TRUE)

# long data的應用 - 畫折線圖
long_df_plot = ggplot(data = long_df, aes(x =星期, y =  營收 , group = 店家))+
  geom_line(aes(linetype=店家, color=店家))

# 將折線圖新增「點」及圖表資訊
long_df_plot + 
  geom_point(aes(color=店家))+ # 點
  labs( title ='店家一週營收趨勢圖', #資訊
        #subtitle = "Scaled feature importance",
        caption = "資料來源: howard")


######################## 7.Data frame if, else 使用方法 ##############################
#'# 7.if, else 使用方法

# 資料表中常用的ifelse 功能
'如果我們想為營收超過25的命名為「業績好」
低於25的為「業績差」
便可以善用ifelse功能
'
long_df$業績狀況 = ifelse(long_df$營收<25, '業績好', '業績差')

# 挑選業績好的，一樣可以用自己定的數字標準或文字標準
long_df[long_df$營收<25, ]
long_df[long_df$業績狀況=='業績好',]


######################## 8.迴圈 and list ##############################
#'# 8.迴圈及list

# for loop, list, function, if else intro
'這邊來介紹資料表以外的常用if else 、loop與function的方法'

# for loop常用的形式
forloopex = c(1,2,3)

length(forloopex) # 通常去看他的長度，就知道for loop裡面的執行次數
for (i in forloopex) {
  print(i) # i 就代表forloopex[1]、forloopex[2]、forloopex[3]
}

# 如何儲存for loop裡面的內容？
'通常我們會以list的方式儲存不同類型的物件，其彈性非常高'
df_list = list(df, df2)
df_list

save_list = list()
forloopex = c(1,2,3)
for (i in forloopex) {
  print(i) # i 就代表forloopex[1]、forloopex[2]、forloopex[3]
  save_list[i]=list(i)
}
save_list
save_list[1] # 擷取第一個list
#save_list[1] +1 但是不能夠計算，所以

# [[1]] <- 要擷取這個裡面的值
# [1] 1
save_list[[1]] # 擷取第一個list的值
save_list[[1]] +1 # ok~!!

# 將list儲存在data frame，通常會使用library(plyr)裡面的ldply function
library(plyr) # install.packages('plyr')
library(scales)
str(ldply(save_list, data.frame))


######################## 9.function ##############################
#'# 9.function

# for loop/ if else/ function in real practice
'function 目的就是要讓開發者一個流程可以重複被使用、可以節省code空間'
'大數據處理上，常常會遇到資料表df或list要儲存的問題，所以會以function的形式來撰寫，節省下一次的撰寫時間'
df_list = list(df, df2)
df_list

# 解構function與教如何寫fucntion
df_to_csv = function(mylist = df_list, saveAscsv = TRUE ){
  '
  目的：將list轉換成data frame，並且自動合併成新的data frame
  '
  # 先用 if else  + class判斷是看看mylist是不是真正的list
  if (class(mylist)=='list') {
    output = ldply(mylist, data.frame)
    #如果不是真正的list，就print 警告message
  }else if(class(mylist)=='data.frame'){
    output = mylist
  }else{
    output = print('你的輸入值不是list喔～ 請轉list or data frame')
  }
  
  return(output)
}

df_to_csv(mylist = df_list)
df_to_csv(mylist = df2)





df_to_csv2 = function(mylist = df_list, saveAscsv = TRUE ){
  '
  目的：將list轉換成data frame，並且自動合併成新的data frame
  '
  # 先用 if else  + class判斷是看看mylist是不是真正的list
  if (class(mylist)=='list') {
    output = ldply(mylist, data.frame)
    #如果不是真正的list，就print 警告message
  }else if(class(mylist)=='data.frame'){
    output = mylist
  }else{
    output = print('你的輸入值不是list喔～ 請轉list or data frame')
  }
  
  # 儲存csv，class裡面改output的原是，要看最後產出的output是否為data frame
  # 然後還要看saveAscsv「是不是符合」True的條件
  if (class(output)=='data.frame' & saveAscsv == saveAscsv) {
    write.csv(output, 'output.csv')
    print('有儲存csv喔！ 存在下述位置')
    print(getwd())
  }
  
  return(output)
}

out = df_to_csv2(mylist =df_list, saveAscsv = TRUE )

# 自學延伸題，實務上非常常用到，但是難度較高，有興趣在來看～
# save_csv = function(df = 這邊放資料表, path = 這邊放要儲存的地方){
#   '
#   目的：儲存data frame
#   '
#   if (class(df) == 'list') {
#     for(i in 1:length(df)){
#       name = names(df[i])
#       write_csv(df[[i]], sprintf('%s%s.csv',path,name) )
#       print(sprintf('%s.csv',name))
#     }
#   }else if(class(df) == 'data.frame'){
#     write_csv(df, sprintf('%s%s.csv', path, df_name) )
#     print( sprintf('%s.csv', df_name))
#   }
#   
# }

# save_csv(df = df_list, path ='/home/slave1/share/mds課程教材/1062MDS' )

######################## practical exercise ##############################
#'# practical exercise
'
第一題：請用long_df找出「營收」75百分位數以上的資料

第二題：
1.請載入 data(iris)
2.將iris四種長度數值全部依照species轉變成平均數
2.將iris從wide data 轉換成long data
3.畫出折線圖 
hint: ggplot(data = data, aes(x =長度, y =  value , group = Species))

第三題：我們發現折線圖星期並沒有依照星期排序，請問該如何依照星期排序，並畫出折線圖？
'


######################## 10.常用繪圖教學 ##############################
#'# 10.常用繪圖教學

'
不從excel中直接讀取？
1. 不建議將資料儲存於xls中，資料有100萬筆之限制
2. R要讀取excel檔案，通常會用到rJave套件，對於linux 及 win 來說，安裝不易，如果要使用xlsx讀取可以使用readxl::read_excel()
'
# 讀取檔案
'
讀取檔案
read.csv()

寫出檔案
write.csv()

'
#write_csv(train, 'MDS實作_#01_資料檔_train2.csv')
train = read.csv('MDS實作_#01_資料檔_train.csv')
str(train)

# 檔案格式轉換
library(lubridate)
train$season  <- factor(train$season, labels = c("春", "夏", "秋", "冬"))
train$weather <- factor(train$weather, labels = c("出太陽", "陰天", "雨天", "雨天且寒冷"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times <- train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$Weekday <- lubridate::wday(ymd_hms(train$datetime), label=TRUE)



# 計算四季24小時的租借次數 - 方法一
season_summary <- ddply(.data = train,.variables = .(season,hour),
                        .fun = summarise, count= mean(count))

# 計算四季24小時的租借次數 - 方法二，
# 如果立志要成為資料科學家，可學習本法，因為語言多變，但是原樣不變
hou = list()
sea = list()
for (season in unique(train$season)) {
  train_temp = train[train$season==season, ]
  
  for (hour in as.numeric(unique(train_temp$hour))) {
    hou[hour+1] = list(
      cbind(mean(train_temp[train_temp$hour==hour,]$count), c(0:23)
      ))
  }
  sea[season] = list(cbind( season, ldply(hou, data.frame)))
  
}

sea = ldply(sea, data.frame)
sea = sea[c('season','X1', 'X2')]

# 常用的繪圖----
'折線圖 
散佈圖 
'
ggplot(data = season_summary, aes(x = hour, y = count, colour = season)) +
  geom_point( aes(group = season)) +
  geom_line( aes(group = season)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("租借次數") +
  theme_minimal() +
  ggtitle("季節與單車租借的關係\n消費者在秋天租車次數多, 但是春天則少") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(count))


ggplot(weather_summary, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("租借次數") +
  theme_minimal() +
  ggtitle("天氣與單車租借的關係\n消費者在天氣好的時候租借次數多") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(train,.(Weekday,hour),
                     summarise, count = mean(count))


ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("租借次數") +
  theme_minimal()+
  ggtitle("一周循環與單車租借的關係\n消費者在平常日早上跟下午租借次數多，但週末則是白天時間租借多")


weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "出太陽"),
                      Normal = mean(weather == "陰天"),
                      Bad = mean(weather == "雨天"),
                      Very_bad = mean(weather == "雨天且寒冷"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("出太陽的機率") +
  theme_minimal() +
  ggtitle("好天氣與單車租借的關係\n在秋天時，好天氣的機率最高") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("陰天的機率") +
  theme_minimal() +
  ggtitle("陰天與單車租借的關係\n在冬天時，陰天的機率最高") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("一天24小時分布") +
  scale_y_continuous("雨天的機率") +
  theme_minimal() +
  ggtitle("雨天與單車租借的關係\n在冬天及夏天時，雨天的機率相對較高") + 
  theme(plot.title=element_text(size=18))



p <- ggplot(train[train$workingday==1,], aes(x=jitter_times, y=count, color=temp)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("一天時間分布") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("租借次數") +
  scale_colour_gradientn("溫度 (°C)", limit=c(0,40),colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("溫度與租借次數的關係\n在平常日，消費者喜歡在天氣暖活的早上跟傍晚租車") +
  theme(plot.title=element_text(size=18))

library(plotly)
ggplotly(p)

p_grid <- ggplot(train, aes(x=jitter_times, y=count, color=temp)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("一天時間分布") +
  scale_x_datetime(breaks = date_breaks("5 hours"), labels=date_format("%I:%M %p")) + 
  ylab("租借次數") +
  scale_colour_gradientn("溫度 (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("一周的溫度與租車次數的關係") +
  theme(plot.title=element_text(size=17))+
  facet_wrap(~Weekday,scales='free')


ggplot(train, aes(x=jitter_times, y=count, color=temp)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("一天時間分布") +
  scale_x_datetime(breaks = date_breaks("6 hours"), labels=date_format("%I:%M %p")) + 
  ylab("租借次數") +
  scale_colour_gradientn("溫度 (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("每季溫度與租車次數的關係") +
  theme(plot.title=element_text(size=17))+
  facet_wrap(~season,scales='free')+scale_y_continuous(limits = c(0,1000))


ggplot(train, aes(x=jitter_times, y=count, color=humidity)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("一天時間分布") +
  scale_x_datetime(breaks = date_breaks("6 hours"), labels=date_format("%I:%M %p")) + 
  ylab("租借次數") +
  scale_colour_gradientn("濕度", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("每季溫度與租車次數的關係") +
  theme(plot.title=element_text(size=17))+
  facet_wrap(~season,scales='free')+scale_y_continuous(limits = c(0,1000))




