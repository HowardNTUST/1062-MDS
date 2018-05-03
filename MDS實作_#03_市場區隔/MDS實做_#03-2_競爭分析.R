
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)} #library(tidyverse)
if(!require(reshape2)){install.packages("reshape2"); require(reshape2)} 
if(!require(ggthemes)){install.packages("ggthemes"); require(ggthemes)} 
if(!require(ggrepel)){install.packages("ggrepel"); require(ggrepel)} 
if(!require(RColorBrewer)){install.packages("RColorBrewer"); require(RColorBrewer)} 
if(!require(ChannelAttribution)){install.packages("ChannelAttribution"); require(ChannelAttribution)} 
if(!require(markovchain)){install.packages("markovchain"); require(markovchain)} 
if(!require(visNetwork)){install.packages("visNetwork"); require(visNetwork)} 
if(!require(expm)){install.packages("expm"); require(expm)} 
if(!require(stringr)){install.packages("stringr"); require(stringr)} 
setwd('C:/Users/howar/Desktop/MDS實作_#03_市場區隔')
load("C:/Users/howar/Desktop/MDS實作_#03_市場區隔/allnew.RData")
### all_data select  ###
name <- list()
name <- readLines("MDS實做_#03_資料檔_Brand.txt", encoding = "UTF-8")

all_data_select =  all_data[c('author_id', 'comment_publish', 'product_name')]
all_data_select = all_data_select[grep(paste(name, collapse = '|'),all_data_select$product_name),]
all_data_select$author_id = sprintf('id_%s',all_data_select$author_id)

# remove duplicated observations
all_data_select = all_data_select[!duplicated(all_data_select[c(1,2,3)]), ]

# add conversion
all_data_select$conversion = ifelse(all_data_select$product_name == '美膚泥漿面膜 Active Mud for Face & Body', 1,0)
# 
# a = all_data_select[all_data_select$author_id=='id_78474',]
# a[order(a$comment_publish),]
# b = a %>%  
#   dplyr::arrange(author_id,comment_publish) %>%
#   ungroup()
##### splitting paths #####
all_data_select=read.csv('all_data_select.csv', stringsAsFactors = F)
all_data_select = all_data_select %>%  
  dplyr::arrange(author_id, comment_publish) %>%
  ungroup()
  

# 將產品排出路徑(path)順序
all_data_select_path <- all_data_select %>%
  group_by(author_id) %>%
  dplyr::mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()

all_data_select_path <- all_data_select_path %>%
  filter(path_no == 1) %>%
  select(-path_no)

ggg =  all_data_select_path %>%
  # removing NAs
  filter(!is.na(product_name)) %>%
  
  # adding order of channels in the path
  group_by(author_id) %>%
  dplyr::mutate(ord = c(1:n()),
         is_non_direct = ifelse(product_name == '美膚泥漿面膜 Active Mud for Face & Body', 0, 1),
         is_non_direct_cum = cumsum(is_non_direct)) %>%
  
  # removing Direct (product_6) when it is the first in the path
  dplyr::filter(is_non_direct_cum != 0) %>%
  
  # replacing Direct (product_6) with the previous touch point
  dplyr::mutate(product_name = ifelse(product_name == '美膚泥漿面膜 Active Mud for Face & Body', product_name[which(product_name != '美膚泥漿面膜 Active Mud for Face & Body')][is_non_direct_cum], product_name)) %>%
  
  ungroup() %>%
  dplyr::select(-ord, -is_non_direct, -is_non_direct_cum)




df_path_1_clean <- ggg %>%
  group_by(author_id) %>%
  dplyr::mutate(uniq_通路_tag = ifelse(length(unique(product_name)) == 1, TRUE, FALSE)) %>%
  ungroup()


##### Generic Probabilistic Model #####
'
好處
看得清楚產品與產品間相互購買的機率，判斷競合關係。
'

# 這邊用的是多channel的model
df_path_1_clean$product_name = gsub('潔膚冰河泥 Epoch Glacial Marine Mud','潔膚冰河泥',df_path_1_clean$product_name)
df_all_paths_compl <- df_path_1_clean %>%
  group_by(author_id) %>%
  dplyr::summarise(path = paste(product_name, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  dplyr::mutate(null_conversion = ifelse(conversion == 1, 0, 1))

mod_attrib_complete <- markov_model(
  df_all_paths_compl,
  var_path = 'path',
  var_conv = 'conversion',
  var_null = 'null_conversion',
  out_more = TRUE
)

trans_matrix_prob <- mod_attrib_complete$transition_matrix %>%
  dmap_at(c(1, 2), as.character)

##### viz #####
edges <-
  data.frame(
    from = trans_matrix_prob$channel_from,
    to = trans_matrix_prob$channel_to,
    label = round(trans_matrix_prob$transition_probability, 2),
    font.size = trans_matrix_prob$transition_probability * 100,
    width = trans_matrix_prob$transition_probability * 15,
    shadow = TRUE,
    arrows = "to",
    color = list(color = "#95cbee", highlight = "red")
  )

nodes <- data_frame(id = c( c(trans_matrix_prob$channel_from), c(trans_matrix_prob$channel_to) )) %>%
  distinct(id) %>%
  arrange(id) %>%
  dplyr::mutate(
    label = id,
    color = ifelse(
      label %in% c('(start)', '(conversion)'),
      '#4ab04a',
      ifelse(label == '(null)', '#ce472e', '#ffd73e')
    ),
    shadow = TRUE,
    shape = "box"
  )

visNetwork(nodes,
           edges,
           height = "2000px",
           width = "100%",
           main = "馬可夫機率模型圖像化") %>%
  visIgraphLayout(randomSeed = 123) %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = TRUE)


##### modeling states and conversions #####
# transition matrix preprocessing
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       n = c(0, 0, 0),
                       tot_n = c(0, 0, 0),
                       perc = c(0, 1, 1))

trans_matrix_complete <- mod_attrib_complete$transition_matrix
trans_matrix_complete <- rbind(trans_matrix_complete, df_dummy %>%
                                 dplyr::mutate(transition_probability = perc) %>%
                                 select(channel_from, channel_to, transition_probability))
trans_matrix_complete$channel_to <- factor(trans_matrix_complete$channel_to, levels = c(levels(trans_matrix_complete$channel_from)))
trans_matrix_complete <- dcast(trans_matrix_complete, channel_from ~ channel_to, value.var = 'transition_probability')
trans_matrix_complete[is.na(trans_matrix_complete)] <- 0
rownames(trans_matrix_complete) <- trans_matrix_complete$channel_from
trans_matrix_complete <- as.matrix(trans_matrix_complete[, -1])


# creating empty matrix for modeling
model_mtrx <- matrix(data = 0,
                     nrow = nrow(trans_matrix_complete), ncol = 1,
                     dimnames = list(c(rownames(trans_matrix_complete)), '(start)'))
# 從pchome吸引1000人
model_mtrx['紅石榴CoQ10面膜', ] <- 10000

a = c(model_mtrx) %*% (trans_matrix_complete %^% 2) # after 5 steps
a

