
# 載入library
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(stringr)
library(plyr)
# source('MDS1062.R')
setwd('C:/Users/howar/Desktop/MDS實作_#03_市場區隔')
load("C:/Users/howar/Desktop/MDS實作_#03_市場區隔/allnew.RData")
# all_data = read.csv('all_data.csv')
'
product_name, attribute
as main product for rfm

發想:將四大客戶的標籤tag轉換到預測性分析
cliendID 的重要性
作業:個人化推薦

seg(問句)
目標應用:知道該如何推薦? 給個別客戶?
產品間的競爭關係?

Targeting

'

##### RF分析 ##### 

' 屬性為主 '
# name <- list()
# name <- readLines("MDS實做_#04_資料檔_Brand.txt", encoding = "UTF-8")
# test = all_data[grep(paste(name, collapse = '|'), all_data$product_name),]
tm = as.data.frame(table(all_data$attribute))
tm = tm[order(tm$Freq, decreasing = T),]
attributes_3n = as.character(tm$Var1[1:3])
attri_df = all_data[grep(paste(attributes_3n, collapse = '|'), all_data$attribute),]
attri_df = attri_df[attri_df$comment_publish>as.Date('2017-02-03'),]
#78474
rfm_df = dcast(attri_df, author_id +author_sex+author_sign+ author_age+comment_popularity+comment_rate+comment_publish~attribute, fun.aggregate=length)


# 報告日期
today <- as.Date('2018-02-01', format='%Y-%m-%d')

rfm_df$comment_publish <- as.Date(rfm_df$comment_publish)



mean_rfm_df = rfm_df %>%
  group_by(author_id) %>%  dplyr::summarise(quantity=n(),
                                                 author_age=sum(author_age, na.rm = T),
                                                 comment_popularity=sum(comment_popularity, na.rm = T),
                                                 comment_rate=sum(comment_rate, na.rm = T)) %>%
  ungroup() %>%
  # calculating CAC and CLV per client
  mutate(author_age_mean=round(author_age/quantity, 2),
         comment_popularity_mean=round(comment_popularity/quantity, 2),
         comment_rate_mean=round(comment_rate/quantity, 2)
         
  ) %>% ungroup()

RF_cal <- rfm_df %>%
  group_by(author_id) %>%
  dplyr::mutate(frequency=n(),
         recency=as.numeric(today-comment_publish)) %>%
  filter(comment_publish==max(comment_publish)) %>%
  #filter(orderId==max(orderId)) %>%
  ungroup()

RF_cal = RF_cal[!duplicated(RF_cal$author_id),]
RF_cal$author_age = NULL
RF_cal$comment_popularity = NULL
RF_cal$comment_rate = NULL

mean_rfm_df$author_age= NULL
mean_rfm_df$comment_popularity= NULL
mean_rfm_df$comment_rate= NULL

# merge RF_cal with mean_rfm_df
RF_newdata = merge(RF_cal, 
                   mean_rfm_df,
                   by = 'author_id')

View(RF_newdata)


# howard的個人主題
howard_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text.x = element_text(size=20, angle = 65, vjust = 1, hjust=1),
      axis.text.y = element_text(size=20),
      axis.title = element_text(size = 20),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "navy", color = "navy", size = 1),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "right",
      legend.justification = "bottom",
      legend.background = element_blank(),
      legend.text=element_text(size=15),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.05),
      title = element_text(size = 15),
      plot.caption=element_text(size = 10)
    )
}

# RF 分析開始

#消費頻率與訂單數量分佈圖
ggplot(RF_newdata, aes(x=frequency)) +
  theme_bw() +  howard_theme()+
  #scale_x_continuous(breaks=c(1:50)) +
  scale_x_continuous(breaks=seq(0,16,1),limits = c(0, 16)) +
  geom_histogram(alpha=0.6, binwidth=1) +
  ggtitle("消費頻率與訂單數量分佈圖")+
  xlab("消費頻率") + 
  ylab("訂單數量") +#howard_theme()+
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20),
        axis.title.y = element_text(color="#993333", size=20))


# 頻率的Hc
'HC解釋'
distance_select <- dist(scale(RF_newdata$frequency), method = "euclidean")
hc <- hclust(distance_select, method = 'ward.D')
# Cut tree into 4 groups
sub_grp <- cutree(hc, k = 6)
RF_newdata$fre_split = sub_grp

gg = list()
for (i in unique(RF_newdata$fre_split)) {
  tme = RF_newdata[RF_newdata$fre_split==i,]
  gg[i] = list(
    data.frame(max = max(tme$frequency),
               min = min(tme$frequency)))
}
fre_ref = ldply(gg,data.frame)
fre_ref = fre_ref[order(fre_ref$min),]

ggplot(RF_newdata, aes(x=frequency)) +
  theme_bw() +  howard_theme()+
  #scale_x_continuous(breaks=c(1:50)) +
  scale_x_continuous(breaks=seq(0,25,1),limits = c(0, 25)) +
  geom_histogram(alpha=0.6, binwidth=1) +
  ggtitle("消費頻率與訂單數量分佈圖")+
  xlab("消費頻率") + 
  ylab("訂單數量") +#howard_theme()+
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20),
        axis.title.y = element_text(color="#993333", size=20))+
  geom_vline(data=fre_ref,  xintercept =fre_ref$max)


# R頻率分佈圖
# RF_newdatalog = RF_newdata
# RF_newdatalog$recency = log1p(RF_newdatalog$recency)
ggplot(RF_newdata, aes(x=recency)) +
  theme_bw() +geom_density(aes(fill = recency, colour = recency) ,position = "stack", alpha =0.1) +
  scale_x_continuous(breaks=seq(0,400,10),limits = c(0, 400)) +
  ggtitle("最近一次（天）的消費與購買量分佈圖")+
  xlab("距離上次購買的天數") + 
  #xlim(1, 45)+
  ylab("訂單數量") +#howard_theme()+
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20),
        axis.title.y = element_text(color="#993333", size=20),
        panel.background = element_rect(fill = "aliceblue"),
        strip.background = element_rect(fill = "navy", color = "navy", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "white"))


# recency的Hc
'HC解釋'
distance_select <- dist(scale(RF_newdata$recency), method = "euclidean")
hc <- hclust(distance_select, method = method_used)
# Cut tree into 4 groups
sub_grp <- cutree(hc, k = 6)
RF_newdata$fre_split = sub_grp

gg = list()
for (i in unique(RF_newdata$fre_split)) {
  tme = RF_newdata[RF_newdata$fre_split==i,]
  gg[i] = list(
    data.frame(max = max(tme$recency),
               min = min(tme$recency)))
}
fre_ref = ldply(gg,data.frame)
fre_ref = fre_ref[order(fre_ref$min),]

ggplot(RF_newdata, aes(x=recency)) +
  theme_bw() +geom_density(aes(fill = recency, colour = recency) ,position = "stack", alpha =0.1) +
  scale_x_continuous(breaks=seq(0,300,10),limits = c(0, 300)) +
  ggtitle("最近一次（天）的消費與購買量分佈圖")+
  xlab("距離上次購買的天數") + 
  ylab("訂單數量") +
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20),
        axis.title.y = element_text(color="#993333", size=20),
        panel.background = element_rect(fill = "aliceblue"),
        strip.background = element_rect(fill = "navy", color = "navy", size = 1),
        strip.text = element_text(face = "bold", size = 10, color = "white"))+
  geom_vline(data=fre_ref,  xintercept =fre_ref$max)


# 切割頻率
orders.segm <- RF_newdata %>%
  mutate(buy_freq=ifelse(between(frequency, 1, 2), '2',
                         ifelse(between(frequency, 3, 6), '6',
                                ifelse(between(frequency,  7, 11), '11',
                                       ifelse(between(frequency, 12, 16), '16',
                                              ifelse(between(frequency, 21, 22), '22', '>22')))))) %>%
  
  
  # 切割近因畫出邊界
  mutate(segm.rec=ifelse(between(recency, 0, 67), '0-67 天',
                         ifelse(between(recency, 68, 111), '68-111 天',
                                ifelse(between(recency, 112, 163), '112-163 天',
                                       ifelse(between(recency, 164, 220), '164-220 天',
                                              ifelse(between(recency, 221, 283), '221-283 天', '>283 天')))))) %>%
  # 把商品放入變數中
  mutate(cart=paste(ifelse(保養面膜!=0, '、保養面膜', ''),
                    ifelse(化妝水!=0, '、化妝水', ''),
                    ifelse(精華液!=0, '、精華液', ''), sep='')) %>%
  arrange(author_id)

# '瓶裝水','牛奶麵包','高麗菜'
# 定義邊界的順序
# orders.segm$buy_freq <- factor(orders.segm$buy_freq, levels=c('>161', '161', '110', '76', '44', '17'))
orders.segm$buy_freq <- factor(orders.segm$buy_freq, levels=c('>22', '22', '16', '11', '6', '2'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>283 天', '221-283 天', '164-220 天', '112-163 天', '68-111 天', '0-67 天'))
orders.segm$cart = str_split_fixed(orders.segm$cart, '、', 2)[,2]

lcg <- orders.segm %>%
  group_by(segm.rec, buy_freq) %>%
  dplyr::summarise(quantity=n()) %>%
  mutate(client='顧客人數') %>%
  ungroup()

lcg.matrix= as.data.frame.matrix(table(orders.segm$buy_freq, orders.segm$segm.rec))
lcg.matrix$buy_freq = row.names(lcg.matrix) 
lcg.matrix
lcg.matrix %>% gather(key = buy_freq, value = measurement)

# 繪製RFM分析圖
lcg.adv <- lcg %>%
  mutate(rec.type = ifelse(segm.rec %in% c(">283 天", "221-283 天", "164-220 天"), "not recent", "recent"),
         freq.type = ifelse(buy_freq %in% c(">22", "22", "16"), "frequent", "infrequent"),
         customer.type = interaction(rec.type, freq.type))

ggplot(lcg.adv, aes(x=client, y=quantity, fill=customer.type)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_rect(aes(fill = customer.type), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  facet_grid(buy_freq ~ segm.rec) +
  geom_bar(stat='identity', alpha=0.7) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  ggtitle("R與F分析圖") +
  xlab("最近一次消費天數") + ylab("購買頻率")+ 
  theme(plot.title = element_text(color="red", size=30 ),
        axis.title.x = element_text(color="blue", size=20, face="bold"),
        axis.title.y = element_text(color="#993333", size=20, face="bold"))+
  guides(fill=guide_legend(title="客群顏色指示表"))+
  scale_fill_discrete(name="Experimental\nCondition",breaks = c('not recent.frequent','recent.frequent','not recent.infrequent','recent.infrequent'), labels = c('先前客','常貴客','一次性消費客人','新顧客'))

# 這邊可以多點花樣

lcg.sub <- orders.segm %>%
  group_by(author_sex, cart, segm.rec, buy_freq) %>%
  dplyr::summarise(quantity=n()) %>%
  mutate(client='顧客人數') %>%
  ungroup()

# 繪製RFM分析圖(性別分類)  fill=
# lcg.sub$gender = factor(lcg.sub$gender, levels = c('女性', '男性'))
ggplot(lcg.sub, aes(x=client, y=quantity, fill=author_sex)) +
  theme_bw() +
  scale_fill_brewer(palette='Set1') +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(buy_freq ~ segm.rec) +
  ggtitle("R與F分析圖（性別）") +
  xlab("最近一次消費天數") + ylab("購買頻率")+ 
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20, face="bold"),
        axis.title.y = element_text(color="#993333", size=20, face="bold"))+
  guides(fill=guide_legend(title="顧客性別"))


# 繪製RFM分析圖(商品分類)
ggplot(lcg.sub, aes(x=author_sex, y=quantity, fill=cart)) +
  theme_bw() +
  scale_fill_brewer(palette='Set1') +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(buy_freq ~ segm.rec) +
  ggtitle("R與F分析圖(商品分類)") +
  xlab("最近一次消費天數") + ylab("購買頻率")+ 
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20, face="bold"),
        axis.title.y = element_text(color="#993333", size=20, face="bold"))+
  guides(fill=guide_legend(title="商品顏色指示表"))

ggplot(lcg.sub, aes(x=author_sex, y=quantity, fill=cart)) +
  theme_bw() +
  scale_fill_brewer(palette='Set1') +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', position='fill' , alpha=0.6) +
  facet_grid(buy_freq ~ segm.rec) +
  ggtitle("R與F分析圖(商品分類)") +
  xlab("最近一次消費天數") + ylab("購買頻率")+ 
  theme(plot.title = element_text(color="red", size=30),
        axis.title.x = element_text(color="blue", size=20, face="bold"),
        axis.title.y = element_text(color="#993333", size=20, face="bold"))+
  guides(fill=guide_legend(title="商品顏色指示表"))


##### M消費金額分析 ##### 
attr_mean = tapply(attri_df$price, attri_df$attribute, function(x) mean(x, na.rm = T))

set.seed(9478)
# 計算clv
gr.margin <- data.frame(attribute=c('保養面膜', '化妝水', '精華液'), grossmarg=c(as.numeric(attr_mean *.9)))
# calculating customer lifetime value
# gg = attri_df[attri_df$comment_publish>as.Date('2017-02-03'),]

# cac
cac = as.data.frame((attr_mean * .3 * 850 )/table(attri_df$attribute))
names(cac) = c('attribute','cac')
cac <- data.frame(author_id=unique(RF_newdata$author_id), cac=sample(c(round((cac$cac)[1]):round((cac$cac)[3])),
                                                                     length(unique(RF_newdata$author_id)), replace=TRUE))

# bined
orders <- merge(attri_df, gr.margin, by='attribute')
orders <- merge(orders, cac, by='author_id')

clv <- orders %>%
  group_by(author_id) %>%
  dplyr::summarise(clv=sum(grossmarg)) %>%
  ungroup()


cac <- orders %>%
  group_by(author_id) %>%
  dplyr::summarise(cac=sum(cac)) %>%
  ungroup()

# 將 CAC與CLV結合進去
orders.segmadd <- merge(orders.segm, clv, by='author_id')
orders.segmadd <- merge(orders.segmadd, cac, by='author_id')

orders.segmadd$clv-orders.segmadd$cac


lcg.clv <- orders.segmadd %>%
  group_by(segm.rec, buy_freq) %>%
  dplyr::summarise(quantity=n(),
            # calculating cumulative CAC and CLV
            cac=sum(cac),
            clv=sum(clv)) %>%
  ungroup() %>%
  # calculating CAC and CLV per client
  mutate(cac_mean=round(cac/quantity, 2),
         clv_mean=round(clv/quantity, 2))

# clv/cac ratio
lcg.clv$ratio = round(lcg.clv$clv/lcg.clv$cac, 2)

lcg.clv <- melt(lcg.clv, id.vars=c('segm.rec', 'buy_freq', 'quantity'))


howard_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text.x = element_text(size=20, angle = 65, vjust = 1, hjust=1),
      axis.text.y = element_text(size=20),
      axis.title = element_text(size = 20),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "navy", color = "navy", size = 1),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "right",
      legend.justification = "bottom",
      legend.background = element_blank(),
      legend.text=element_text(size=15),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.05),
      title = element_text(size = 15),
      plot.caption=element_text(size = 10)
    )
}


ggplot(lcg.clv[lcg.clv$variable %in% c('clv', 'cac'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=round(value)), size=4) +
  facet_grid( buy_freq~ segm.rec) +
  ggtitle("顧客終生價值（CLV）與成本（CAC）的加總比較") + howard_theme()+
  xlab("CLV與CAC之比較") + 
  ylab("＄錢＄")


ggplot(lcg.clv[lcg.clv$variable %in% c('clv_mean', 'cac_mean'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity))) +
  geom_text(aes(y=value, label=round(value)), size=4) +
  facet_grid(buy_freq ~ segm.rec) +
  ggtitle("顧客終生價值（CLV）與成本（CAC）- 顧客平均價值")+
  howard_theme()+
  xlab("CLV與CAC之比較") + 
  ylab("＄錢＄")

ggplot(lcg.clv[lcg.clv$variable %in% c('ratio'), ], aes(x=variable, y=value, fill=variable)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6, aes(width=quantity/max(quantity),fill = value > 1) ) +
  geom_text(aes(y=value, label=value), size=4) +
  facet_grid(buy_freq ~ segm.rec) +
  ggtitle("顧客獲利率比較圖") + howard_theme()+
  guides(fill=FALSE)+
  xlab("比例變數") + 
  ylab("顧客獲利率")


# time series

orders.segmadd2 = merge(attri_df ,gr.margin, by = 'attribute')

# a = orders.segmadd2[orders.segmadd2$author_id == '279664',]

customers = orders.segmadd2%>%group_by(author_id) %>%
  dplyr::mutate(frequency=n(),
                recency=as.numeric(today-max(comment_publish)),
                av.gap=round(as.numeric(max(comment_publish)-min(comment_publish))/frequency, 0),
                cohort=format(min(comment_publish), format='%Y-%m'),
                comment_publish=comment_publish) %>%
  ungroup()  %>%
  # calculating CLV to date
  group_by(author_id, cohort, frequency, recency, av.gap) %>%
  dplyr::summarise(clv=sum(grossmarg)) %>%
  arrange(author_id) %>%
  ungroup()


# 切割頻率

orders.segm <- customers %>%
  mutate(buy_freq=ifelse(between(frequency, 1, 2), '2',
                         ifelse(between(frequency, 3, 6), '6',
                                ifelse(between(frequency,  7, 11), '11',
                                       ifelse(between(frequency, 12, 16), '16',
                                              ifelse(between(frequency, 21, 22), '22', '>22')))))) %>%
  
  
  # 切割近因畫出邊界
  mutate(segm.rec=ifelse(between(recency, 0, 67), '0-67 天',
                         ifelse(between(recency, 68, 111), '68-111 天',
                                ifelse(between(recency, 112, 163), '112-163 天',
                                       ifelse(between(recency, 164, 220), '164-220 天',
                                              ifelse(between(recency, 221, 283), '221-283 天', '>283 天')))))) 


# 定義邊界的順序
# orders.segm$buy_freq <- factor(orders.segm$buy_freq, levels=c('>161', '161', '110', '76', '44', '17'))
orders.segm$buy_freq <- factor(orders.segm$buy_freq, levels=c('>22', '22', '16', '11', '6', '2'))
orders.segm$segm.rec <- factor(orders.segm$segm.rec, levels=c('>283 天', '221-283 天', '164-220 天', '112-163 天', '68-111 天', '0-67 天'))
customer_orders.segm <- merge(orders.segm, cac, by='author_id')

lcg.coh <- customer_orders.segm %>%
  group_by(cohort, segm.rec, buy_freq) %>%
  # calculating cumulative values
  dplyr::summarise(quantity=n(),
            cac=sum(cac),
            clv=sum(clv),
            av.gap=sum(av.gap)) %>%
  ungroup() %>%
  # calculating average values
  mutate(av.cac=round(cac/quantity, 2),
         av.clv=round(clv/quantity, 2),
         av.gap=round(av.gap/quantity, 2),
         diff=av.clv-av.cac)

ggplot(lcg.coh, aes(x=cohort, fill=cohort)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(aes(y=diff), stat='identity', alpha=0.5) +
  geom_text(aes(y=diff, label=round(diff,0)), size=4) +
  facet_grid(buy_freq ~ segm.rec) +
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain")) +
  ggtitle("時間序列的消費金額分析 - CLV與CAC之差異平均價值")+
  xlab("時間序列") + 
  ylab("CLV與CAC之差異平均價值")+
  howard_theme()+
  guides(fill=FALSE)






