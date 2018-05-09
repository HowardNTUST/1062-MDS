
#' ---
#' title: "bank loan product predictions"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---

#author: Howard Chung
#updated date: 2018/5/10
#pupose: rf, glm, svm, xgboost, ann
#version: v 0.1.2

setwd('C:/Users/howar/Desktop/MDS實作_#04_目標市場')

#'#1. library
######################## 1.library ##############################
if(!require(caret)){install.packages("caret"); require(caret)}   
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}   
if(!require(randomForest)){install.packages("randomForest"); require(randomForest)}   
if(!require(xgboost)){install.packages("xgboost"); require(xgboost)}   
if(!require(scales)){install.packages("scales"); require(scales)}   
if(!require(caTools)){install.packages("caTools"); require(caTools)}   
if(!require(e1071)){install.packages("e1071"); require(e1071)}   


# Importing the dataset
dataset = read.csv('MDS實作_#04_資料檔_loan.csv')
str(dataset)

# Encoding the categorical variables as num
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('Taipei', 'Tainan', 'Taichung'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# the reason is that as.factor(dataset$loan) will have it recognized as classification dataset for
# model we will use
dataset$loan=as.factor(dataset$loan)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$loan, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#'#2.logistic regression
######################## 2.logistic regression ##############################
# Fitting classifier to the Training set
lgclassifier = glm(loan~.,data=training_set,family=binomial)
summary(lgclassifier)
glm_y_pred = predict(lgclassifier, newdata = test_set[-11], type = 'response')
glm_y_pred2 = glm_y_pred
glm_y_pred2 = ifelse(glm_y_pred >= 0.5,1,0 )
confusionMatrix(test_set[, 11], glm_y_pred2, positive ='1')



#'#2.rf
######################## 2.rf ##############################
# Fitting classifier to the Training set

rfclassifier=randomForest(loan~.,data=training_set,
                        ntree=300,
                        importance=T,
                        do.trace=T,
                        mtry=3,
                        maxnodes=60)


# Predicting the Test set results
y_pred = predict(rfclassifier, newdata = test_set[-11])

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
confusionMatrix(test_set[, 11], y_pred, positive ='1')
newtest = cbind(test_set, y_pred)
# (175+1562) / 2000

# financial matrix


# empirical df
aa = table(training_set$Tenure, training_set$loan)

aadf = as.data.frame.matrix(aa / rowSums(aa))
aadf = aadf[order(aadf$`1`, decreasing = T),]
selectrow = rownames(aadf)[1:5]
test_setemp = test_set
test_setemp$pred_emp = 0
test_setemp[grep(paste(selectrow, collapse = '|'), test_setemp$Tenure), ]$pred_emp = 1
confusionMatrix(test_setemp[, 11], test_setemp$pred_emp, positive ='1')
test_setemp_newtest = cbind(test_setemp, y_pred)
cm_emp = table(test_setemp[, 11], test_setemp$pred_emp)

# manpower cost
dim(test_set)
system.time(predict(rfclassifier, newdata = test_set[-11]))
system.time(predict(rfclassifier, newdata = test_set[1,-11]))




# feature importance
varImpPlot(rfclassifier)

imp=data.frame()
imp=data.frame(rfclassifier$importance)
imp$group=row.names(imp)
imp=imp[,c(4,5)]
imp$MeanDecreaseGini=rescale(imp$MeanDecreaseGini)


# clustered feature importance 

a=kmeans(x = imp$MeanDecreaseGini, 3)$cluster
imp$cluster= a

importance=imp[order(imp$MeanDecreaseGini),]
importance_df_2 <- importance
importance_df_2$MeanDecreaseGini <- 0
importance_df <- rbind(importance, importance_df_2)


my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "navy", color = "navy", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      legend.justification = "top", 
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}
theme_set(my_theme())


ggplot() +
  geom_point(data = importance, aes(x = MeanDecreaseGini, y = reorder(group, MeanDecreaseGini), color = factor(cluster)), size = 3) +
  geom_path(data = importance_df, aes(x = MeanDecreaseGini, y = group, color =factor(cluster)), size = 2) +
  theme(legend.position = "none") +
  labs(
    x = "Importance",
    y = "",
    title = "rf",
    subtitle = "重要程度(GINI)")
    #caption = "\nDetermined with Random Forest and
     # repeated cross validation (10 repeats, 10 times)"

ggplot(dataset, aes(x=loan, y=Age, fill=loan)) + 
  geom_boxplot()


table(dataset$NumOfProducts, dataset$loan)
chisq.test( table(dataset$NumOfProducts, dataset$loan) )
ggplot(dataset, aes(x=loan, y=NumOfProducts, fill= NumOfProducts)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_rect(aes(fill = NumOfProducts), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  facet_grid(loan ~ NumOfProducts) +
  geom_bar(stat='identity', alpha=0.7) 


all_imp=imp[,1:2]
colnames(all_imp)=c('rf_imp','group')



