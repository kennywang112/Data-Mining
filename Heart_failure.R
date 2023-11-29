library(tidyverse)

# 目標：複回歸模型預測及檢測
data <- read_csv("/Users/wangqiqian/Desktop/資料探勘/data/heart_failure_clinical_records_dataset.csv")
# 這筆資料要預測Salary，而Gender/Education/Job/Experience 為質性變數
data%>%head()
# 資料quantile & min max
data%>%summary()
# 維度
data%>%dim()
# 缺失值？
all(is.na(data))
data

data$DEATH_EVENT<-as.factor(data$DEATH_EVENT)
ggplot(data)+geom_density(aes(age,fill=DEATH_EVENT),alpha=0.4)+scale_fill_grey()

#各個資料在是否死亡下的平均
death_data<-filter(data,DEATH_EVENT==1)
notdeath_data<-filter(data,DEATH_EVENT==0)
mean_death_data<-list()
mean_notdeath_data<-list()
clst<-list()
for(i in 2:ncol(data)-1) {
  a<-sum(death_data[[i]])/nrow(death_data[i])
  mean_death_data<-append(mean_death_data,a)
  b<-sum(notdeath_data[[i]])/nrow(notdeath_data[i])
  mean_notdeath_data<-append(mean_notdeath_data,b)
  clst<-append(clst,names(data[i]))
}
mean_data <- as.matrix(rbind(clst,mean_death_data,mean_notdeath_data))
mean_data

data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, age))+labs(title='Age vs Death event')
data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, ejection_fraction))+labs(title='Ejection_fraction vs Death event')
data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, serum_creatinine))+labs(title='Serum_creatinine vs Death event')
data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, serum_sodium))+labs(title='Serum_sodium vs Death event')
data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, time))+labs(title='Time vs Death event')

set.seed(1)
train <- sample(1:nrow(data),210)
train_data <- data[train,]
test_data <- data[-train,]

library(rpart)
library(rpart.plot)
# 決策樹
tree_model <- rpart(DEATH_EVENT ~.,data = train_data)
tree_model
# cp懲罰誤差過大的樹，nsplit分支樹，rel error訓練中各種樹對應的誤差，xerror交叉驗證的誤差，xstd交叉驗證誤差的標準差
# cp = conditional probability
tree_model$cptable

par(mfrow = c(1, 2))
rpart.plot(tree_model)
# 交叉驗證誤差
plotcp(tree_model)

tree_pred <- predict(tree_model, test_data, method='class')
predicted_labels <- apply(tree_pred, 1, which.max) - 1
table(test_data$DEATH_EVENT, predicted_labels)
sum(diag(table(test_data$DEATH_EVENT, predicted_labels)))/sum(table(test_data$DEATH_EVENT, predicted_labels))

#opt <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
#model.ptree <- prune(tree_model, cp = opt)
#rpart.plot(model.ptree)
#pruned_pred <- predict(model.ptree, test_data, method='class')
#pruned_predicted_labels <- apply(pruned_pred, 1, which.max) - 1
#sum(diag(table(test_data$DEATH_EVENT, pruned_predicted_labels)))/sum(table(test_data$DEATH_EVENT, pruned_predicted_labels))

# remove outlier
data%>%filter(DEATH_EVENT == 0)%>%summary()
data%>%filter(DEATH_EVENT == 1)%>%summary()

filtered_data <- data%>%filter(DEATH_EVENT == 0 & age < 90 | DEATH_EVENT == 1)
filtered_data <- filtered_data%>%filter(DEATH_EVENT == 0 & serum_creatinine <  1.2*1.5 | DEATH_EVENT == 1 & serum_creatinine <  1.9*1.5)
filtered_data <- filtered_data%>%filter(DEATH_EVENT == 0 & ejection_fraction <  45*1.5 | DEATH_EVENT == 1 & ejection_fraction <  38*1.5)
filtered_data <- filtered_data%>%filter(DEATH_EVENT == 1 & time < 102*1.5 | DEATH_EVENT == 0)

filtered_data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, age))+labs(title='Age vs Death event')
filtered_data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, serum_creatinine))+labs(title='serum_creatinine vs Death event')
filtered_data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, ejection_fraction))+labs(title='ejection_fraction vs Death event')
filtered_data%>%ggplot()+geom_boxplot(aes(DEATH_EVENT, time))+labs(title='time vs Death event')

filtered_train_data <- filtered_data[train,]
filtered_test_data <- filtered_data[-train,]

tree_model <- rpart(DEATH_EVENT ~.,data = filtered_train_data)
tree_model
# cp懲罰誤差過大的樹，nsplit分支樹，rel error訓練中各種樹對應的誤差，xerror交叉驗證的誤差，xstd交叉驗證誤差的標準差
# cp = conditional probability
tree_model$cptable

rpart.plot(tree_model)
# 交叉驗證誤差
plotcp(tree_model)

tree_pred <- predict(tree_model, test_data, method='class')
predicted_labels <- apply(tree_pred, 1, which.max) - 1
table(test_data$DEATH_EVENT, predicted_labels)
sum(diag(table(test_data$DEATH_EVENT, predicted_labels)))/sum(table(test_data$DEATH_EVENT, predicted_labels))











