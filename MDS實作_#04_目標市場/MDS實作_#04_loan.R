<<<<<<< HEAD

#' ---
#' title: "ptt article number of record predictions"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     highlight: tango
#'     theme: sandstone
#'     toc: yes
#' ---

#author: Howard Chung
#updated date: 2017/5/15
#pupose: rf, svm, xgboost, ann
#version: v 0.1.1
#update news:
#2017/5/15: rf

setwd('/media/slave1/Howard 2/my files/工作/進修/Marketing data science/出版/1062行銷資料科學/教材/讀書會/20170516_#06_MDS讀書會_顧客連結與行銷資料科學 & 文字探勘(2)')

#'#1.data
######################## 1.data ##############################
suppressPackageStartupMessages({
  library(caret)
  library(tidyverse)
  library(randomForest)
  library(xgboost)
  library(h2o)
  library(scales)
  library(caTools)
})


# Importing the dataset
dataset = read.csv('MDS實作_#04_資料檔_loan.csv')

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('Taipei', 'Tainan', 'Taichung'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

dataset$loan=as.factor(dataset$loan)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset$loan, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[-11] = scale(training_set[-11])
# test_set[-11] = scale(test_set[-11])
# scale(test_set[1:5,-11])

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

# pursue all customers
(cm[2,2] + cm[2,1])*3500 - 1800*(cm[2,2] + cm[2,1]) - 300*(sum(cm))

# pursue targeted custoamers
(cm[2,2] )*3500 - 1800*(cm[2,2]) - 300*(cm[1,2] + cm[2,2])


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

# pursue emp customers
((cm_emp[2,2] )*3500 - 1800*(cm_emp[2,2]) - 300*(cm_emp[1,2] + cm_emp[2,2])) * 4.2759


# pursue targeted custoamers
(cm[2,2] )*3500 - 1800*(cm[2,2]) - 300*(cm[1,2] + cm[2,2])

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
wss <- (nrow(imp$MeanDecreaseGini)-1) * sum(apply(imp$MeanDecreaseGini,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(imp$MeanDecreaseGini,
                                     centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

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

######################## 3.glm ##############################
lgclassifier = glm(loan~.,data=training_set,family=binomial)
summary(lgclassifier)
glm_y_pred = predict(lgclassifier, newdata = test_set[-11], type = 'response')
glm_y_pred2 = glm_y_pred
glm_y_pred2 = ifelse(glm_y_pred >= 0.5,1,0 )
confusionMatrix(test_set[, 11], glm_y_pred2, positive ='1')


#'#3.data
######################## 3.svm ##############################
fitControl <- trainControl(method = "none", classProbs = TRUE)
training_set$loan=as.factor(ifelse(training_set$loan==1,'Y','N'))

svmFit <- caret::train(loan~.,
                data=training_set, 
                method = "svmRadial", 
                trControl =   fitControl, 
                metric = "ROC")

svmFit$modelInfo
y_pred=predict(svmFit, newdata = test_set[,-11])

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
test_set$loan=as.factor(ifelse(test_set$loan==1,'Y','N'))
confusionMatrix(test_set[, 11], y_pred)
varImp(svmFit, scale = F)


imp=data.frame()
imp=data.frame(varImp(svmFit, scale = T)[1])
imp$group=row.names(imp)
imp=imp[,c(2,3)]
imp$importance.Y=rescale(imp$importance.Y)


# clustered feature importance 
wss <- (nrow(imp$importance.Y)-1)*sum(apply(imp$importance.Y,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(imp$importance.Y,
                                    centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

a=kmeans(x = imp$importance.Y, 3)$cluster
imp$cluster= a

importance=imp[order(imp$importance.Y),]
importance_df_2 <- importance
importance_df_2$importance.Y <- 0
importance_df <- rbind(importance, importance_df_2)

#plot


 ggplot() +
  geom_point(data = importance, aes(x = importance.Y, y = reorder(group,+importance.Y), color = factor(cluster)), size = 3) +
  geom_path(data = importance_df, aes(x = importance.Y, y = group, color =factor(cluster)), size = 2) +
  theme(legend.position = "none") +
  labs(
    x = "Importance",
    y = "",
    title = "svm",
    subtitle = "Scaled ROC feature importance")
#caption = "\nDetermined with Random Forest and
# repeated cross validation (10 repeats, 10 times)"
 colnames(imp)=c('svm_imp','group')
 all_imp=merge(all_imp, imp, by='group')

#'#4.xgboost (advanced rf)
######################## 4.xgboost  (advanced rf) ##############################
test_set$loan=as.numeric(ifelse(test_set$loan=='Y',1,0))
training_set$loan=as.numeric(ifelse(training_set$loan=='Y',1,0))

xgb_classifier = xgboost(data = as.matrix(training_set[,-11]), 
                         label =training_set$loan , 
                         nround=2500, nthread = 5, 
                         metrics="auc",
                         max.depth =3, 
                         eta = 0.01, 
                        nfold=5,
                         objective = "binary:logistic")

pred=predict(xgb_classifier, as.matrix(test_set[,-11]))
pred=ifelse(pred>0.5, 1,0)

cm = table(test_set[, 11], pred)
confusionMatrix(test_set[, 11], pred)

names <-  colnames(training_set)

# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = xgb_classifier)
head(importance_matrix)
importance_matrix$Gain=rescale(importance_matrix$Gain)

#find most important texts of 30 among mixed-topic articles in grad ptt posts
gp = xgb.plot.importance(importance_matrix) 
print(gp)
rescale(importance_matrix$Gain)

colnames(importance_matrix)=c('group','xgb_imp','Cover','Frequence')
all_imp=merge(all_imp, importance_matrix[,1:2], by='group')
#'#5.ANN 
######################## 5. ANN extra ##############################

#initializing h2o
h2o.init(nthreads = -1, max_mem_size = "15g")

test_set$loan=as.factor(ifelse(test_set$loan=='1','Y','N'))
training_set$loan=as.factor(ifelse(training_set$loan=='1','Y','N'))

model = h2o.deeplearning(y = 'loan',
                         model_id="dl_model_first",
                         training_frame = as.h2o(training_set),
                         validation_frame=as.h2o(test_set),
                         activation = 'Rectifier',
                         hidden = c(5,5), #hidden layer, nodes
                         epochs = 100,
                         nfolds = 5,
                         variable_importances=T,
                         train_samples_per_iteration = -2)
summary(model)


# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-11]))
y_pred = as.vector(y_pred[1]) %>% as.factor(.)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
confusionMatrix(test_set[, 11], y_pred)


as.data.frame(h2o.varimp(model))
h2o.shutdown()

#tuning

response <- "loan"
fea=setdiff(colnames(training_set), response)

hyper_params <- list(
  hidden=list(c(32,32,32),c(32,32),c(16,16),c(8,8,8),c(8,8),c(4,4),c(5,5)),
  input_dropout_ratio=c(0,0.05,0.01),
  rate=c(0.005,0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params

grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid", 
  training_frame = as.h2o(training_set),
  validation_frame = as.h2o(test_set),
  x = fea,
  y = response,
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9,
  variable_importances=T,
  momentum_ramp=1e7, 
  activation=c("Rectifier"),
  max_w2=10,
  l1=1e-5,
  l2=1e-5,## can help improve stability for Rectifier
  hyper_params=hyper_params
)
grid

grid <- h2o.getGrid("dl_grid",sort_by="err",decreasing=FALSE)
grid

grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss

y_pred = h2o.predict(best_model, newdata = as.h2o(test_set[-11]))
y_pred = as.vector(y_pred[1]) %>% as.factor(.)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
confusionMatrix(test_set[, 11], y_pred)

#varann_imp
ann_imp=as.data.frame(h2o.varimp(best_model))


ann_imp=ann_imp[,c(1,2)]

# clustered feature ann_importance 
wss <- (nrow(ann_imp$relative_importance)-1)*sum(apply(ann_imp$relative_importance,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(ann_imp$relative_importance,
                                    centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

a=kmeans(x = ann_imp$relative_importance, 5)$cluster
ann_imp$cluster= a

ann_importance=ann_imp[order(ann_imp$relative_importance),]
ann_importance_df_2 <- ann_importance
ann_importance_df_2$relative_importance <- 0
ann_importance_df <- rbind(ann_importance, ann_importance_df_2)

#plot
ggplot() +
  geom_point(data = ann_importance, aes(x = relative_importance, y = reorder(variable,+relative_importance), color = factor(cluster)), size = 3) +
  geom_path(data = ann_importance_df, aes(x = relative_importance, y = variable, color =factor(cluster)), size = 2) +
  theme(legend.position = "none") +
  labs(
    x = "ann_importance",
    y = "",
    title = "ANN",
    subtitle = "Scaled hidden nurons feature ann_importance")
#caption = "\nDetermined with Random Forest and
# repeated cross validation (10 repeats, 10 times)"

#all imp
ann_imp=ann_imp[,1:2]
colnames(ann_imp)=c('group','ann_imp')
all_imp=merge(all_imp, ann_imp, by='group')
all_imp$total_imp=rowMeans(all_imp[,2:5])
write_csv(all_imp,'all_imp.csv')



all_imp=all_imp[,c(1,6)]

# clustered feature all_importance 
wss <- (nrow(all_imp)-1)*sum(apply(all_imp,2,var))

for (i in 2:8) wss[i] <- sum(kmeans(all_imp$total_imp,iter.max=500,
                                    centers=i)$withinss)

plot(1:8, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares")

a=kmeans(x = all_imp$total_imp, 3)$cluster
all_imp$cluster= a

all_importance=all_imp[order(all_imp$total_imp),]
all_importance_df_2 <- all_importance
all_importance_df_2$total_imp <- 0
all_importance_df <- rbind(all_importance, all_importance_df_2)

#plot
ggplot() +
  geom_point(data = all_importance, aes(x = total_imp, y = reorder(group,+total_imp), color = factor(cluster)), size = 3) +
  geom_path(data = all_importance_df, aes(x = total_imp, y = group, color =factor(cluster)), size = 2) +
  theme(legend.position = "none") +
  labs(
    x = "all_importance",
    y = "",
    title = "total importance",
    subtitle = "all Scaled feature importance")
#caption = "\nDetermined with Random Forest and
# repeated cross validation (10 repeats, 10 times)"


=======

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



>>>>>>> branch 'master' of https://github.com/HowardNTUST/1062-MDS.git
