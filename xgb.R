#XGboost 
install.packages("caret")
install.packages("xgboost")
install.packages("GGally")
install.packages("MASS")
install.packages("e1071")
install.packages("ROCR")
install.packages("corrplot")
install.packages("corrgram")
library(corrgram)
library(corrplot)
library(ROCR)
library(xgboost)
library(ggplot2)
library(GGally)
library(readxl)
library(dplyr)
library(MASS)
library(caret)
library(readxl)
library(e1071)
hr_data_train <- read_excel("hr_data_train.xlsx",sheet = 1)
hr_data_test <- read_excel("hr_data_test.xlsx",sheet = 1)
table(is.na(hr_data_test))
table(is.na(hr_data_train))
hr_data_train

hr_data_test <- na.omit(hr_data_test) #결측치 제거해서 넣기
hr_data_train <-na.omit(hr_data_train)

table(is.na(hr_data_test))
table(is.na(hr_data_train))

hr_data_train[-c(1)]
hr_data_train<- hr_data_train[-c(1)]
hr_data_train$A1 <- as.factor(hr_data_train$A1)
hr_data_train$A3 <- as.factor(hr_data_train$A3)
hr_data_train$A4 <- as.factor(hr_data_train$A4)
hr_data_train$A11 <- as.factor(hr_data_train$A11)
hr_data_train$A12 <- as.factor(hr_data_train$A12)


hr_data_test<- hr_data_test[-c(1)]
hr_data_test$A1 <- as.factor(hr_data_test$A1)
hr_data_test$A3 <- as.factor(hr_data_test$A3)
hr_data_test$A4 <- as.factor(hr_data_test$A4)
hr_data_test$A11 <- as.factor(hr_data_test$A11)
hr_data_test$A12 <- as.factor(hr_data_test$A12)




#-----------등급 별로 나누기--------------------------
#전체
train.total <- hr_data_train
test.total <- hr_data_test

#국6등급만
train.6 <- hr_data_train %>% filter(A11 == "6rank")
test.6 <- hr_data_test %>% filter(A11 == "6rank")

train.6

#국5등급만
train.5 <- hr_data_train %>% filter(A11 == "5rank")
test.5 <- hr_data_test %>% filter(A11 == "5rank")

#국4등급+혼4등급
train.4 <- hr_data_train %>% filter(A11 == "4rank")
test.4 <- hr_data_test %>% filter(A11 == "4rank")


#국3등급/혼3등급
train.3 <- hr_data_train %>% filter(A11 == "3rank")
test.3 <- hr_data_test %>% filter(A11 == "3rank")


#2등급만
train.2 <- hr_data_train %>% filter(A11 == "2rank")
test.2 <- hr_data_test %>% filter(A11 == "2rank")

#1등급만
train.1 <- hr_data_train %>% filter(A11 == "1rank")
test.1 <- hr_data_test %>% filter(A11 == "1rank")

#국OPEN+혼OPEN
train.open <- hr_data_train %>% filter(A11 == "OPENrank")
test.open <- hr_data_test %>% filter(A11 == "OPENrank")





train_data <- as.data.frame(train.total)
test_data <-as.data.frame(test.total)

#sgboost는 num 형태의 데이터만 사용하기때문에 성별,산지,등급,거리(카테고리)삭제
train_data_num <- train_data[,-c(3,4,11,12)]
train_x <- train_data_num[,-c(1)]
train_matrix_x <-as.matrix(train_x)

train_y <- train_data_num[,c(1)]
train_y <- as.numeric(train_data_num$A1, levels=c("N","Y"),labels=c("0","1"))-1
train_matrix_y <-as.matrix(train_y)

View(train_x)
View(train_y)

test_x <- test_data[,-c(1,3,4,11,12)]
test_matrix_x <-as.matrix(test_x)
test_y <- test_data[,-c(2:42)]
test_y <- as.numeric(test_data$A1, levels=c("N","Y"),labels=c("0","1"))-1
test_matrix_y <-as.matrix(test_y)





#xgboost를 학습

train_labels_y <-as.numeric(train_matrix_y)
test_labels_y <-as.numeric(test_matrix_y)



str(train_labels_y)
str(train_matrix_x)


train.xm <- xgb.DMatrix(data = train_matrix_x , label = train_matrix_y)
test.xm <- xgb.DMatrix(data = test_matrix_x,label = test_labels_y)



cv_model2 <- xgb.train(data= train.xm, max.depth=4, eta =1
                       ntread = 2 , nround = 2, nfold = 5)

cv_model1<- xgboost(data = train_matrix_x , label = train_matrix_y,
                    eta = 0.01, scale_pos_weight = 0.7,
                    nrounds = 5,objective ='binary:logistic')

#----------다른 방법 이게 맞다

train_labels_y <-as.numeric(train_matrix_y)
test_labels_y <-as.numeric(test_matrix_y)



str(train_labels_y)
str(train_matrix_x)


train.xm <- xgb.DMatrix(data = train_matrix_x , label = train_matrix_y)
test.xm <- xgb.DMatrix(data = test_matrix_x,label = test_labels_y)
param <-  list("objective" = "binary:logistic",
               "bootster" = "gbtree",
              "num_class" = 12)
train.xm
test.xm

train.xm

cv.nround <- 11
cv.nfold <- 5

cv_model1 <- xgb.cv(data= train_matrix_x, label = train_matrix_y ,params = param,
                    nfold = cv.nfold,nrounds = cv.nround,verbose = T)
#---------------------------------------------------


lvl=c("N","Y")
pred <- predict(cv_model1,train_matrix_x)
pred_label <- lvl[as.numeric(pred>0.5)+1]

pred_label

actual_label <- lvl[as.numeric(train_matrix_y)+1]
actual_label
train_matrix_y

table(pred_label,actual_label)


#--------------------
pred <- predict(cv_model1,test_matrix_x)
pred_label <- lvl[as.numeric(pred>0.5)+1]

pred_label

actual_label <- lvl[as.numeric(test_matrix_y)+1]
actual_label
test_matrix_y

table(pred_label,actual_label)
#-----------------------------------------
pred <- predict(cv_model1,test_matrix_x)
pred_label <- as.numeric(pred>0.5)

pred_label

actual_label <- as.numeric(test_matrix_y)
actual_label
test_matrix_y



confusionMatrix(factor(pred_label),factor(actual_label), positive = '1')

#ROC curve 그리기

pred_roc <- prediction(as.numeric(pred_label),as.numeric(actual_label))
summary(pred)

y_pred
as.numeric(y_pred)-1


as.numeric(test_y)


performance(pred_roc,"tpr","fpr")

perf <-performance(pred_roc,"tpr","fpr")

plot(perf,main ="ROC 커브", colorize = T)

#AUC 값 

auc <- performance(pred_roc,"auc")
auc<- auc@y.values[[1]]
auc                
