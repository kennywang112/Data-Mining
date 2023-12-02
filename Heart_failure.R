library(tidyverse) 

# tree model
data <- read_csv("/Users/wangqiqian/Desktop/資料探勘/data/heart_failure_clinical_records_dataset.csv")
data%>%head()
data%>%summary()
data%>%dim()
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
origin_cart_acc <- sum(diag(table(test_data$DEATH_EVENT, predicted_labels)))/sum(table(test_data$DEATH_EVENT, predicted_labels))

opt <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
model.ptree <- prune(tree_model, cp = opt)
rpart.plot(model.ptree)
pruned_pred <- predict(model.ptree, test_data, method='class')
pruned_predicted_labels <- apply(pruned_pred, 1, which.max) - 1
prune_cart_acc <- sum(diag(table(test_data$DEATH_EVENT, pruned_predicted_labels)))/sum(table(test_data$DEATH_EVENT, pruned_predicted_labels))

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

# NA value
train <- sample(1:nrow(filtered_data),175)
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

tree_pred <- predict(tree_model, filtered_test_data, method='class')
predicted_labels <- apply(tree_pred, 1, which.max) - 1
table(filtered_test_data$DEATH_EVENT, predicted_labels)
filtered_acc <- sum(diag(table(filtered_test_data$DEATH_EVENT, predicted_labels)))/sum(table(filtered_test_data$DEATH_EVENT, predicted_labels))

library(C50)

tree_model_c50 <- C5.0(DEATH_EVENT ~.,data = filtered_train_data)
summary(tree_model_c50)
plot(tree_model_c50)

tree_pred <- predict(tree_model_c50, filtered_test_data, method='class')
table(filtered_test_data$DEATH_EVENT, tree_pred)
c50_cc <- sum(diag(table(filtered_test_data$DEATH_EVENT, tree_pred)))/sum(table(filtered_test_data$DEATH_EVENT, tree_pred))

# chaid
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

chaid_model <- chaid(
  DEATH_EVENT ~ as.factor(anaemia) + as.factor(diabetes) +
               as.factor(high_blood_pressure) + as.factor(sex) +
               as.factor(smoking),
              data = filtered_train_data)
chaid_model

plot(chaid_model, type = "simple")

# naive bayes
library(e1071)

nb_model <- naiveBayes(DEATH_EVENT ~ .,data = filtered_train_data)
nb_model

nb_pred <- predict(nb_model, filtered_test_data)

table(filtered_test_data$DEATH_EVENT, nb_pred)
nb_acc <- sum(diag(table(filtered_test_data$DEATH_EVENT, nb_pred)))/sum(table(filtered_test_data$DEATH_EVENT, nb_pred))

# all acc
models <- c("Origin CART", "Filtered CART", "Filtered C50", "Filtered NB")
accuracies <- c(origin_cart_acc, filtered_acc, c50_cc, nb_acc)

acc_data <- data.frame(Model = models, Accuracy = accuracies)
acc_data%>%ggplot(aes(reorder(Model, Accuracy), Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Model Accuracies", x = "Model", y = "Accuracy")

# origin cart
# 0  1
# 0 54  7
# 1  7 21
origin_cart_error <- (7+7) / 89
type_1_origin <- (7) / 89
type_2_origin <- (7) / 89

# filtered cart
# 0  1
# 0 57  1
# 1  4 14
filtered_cart_error <- (1+4) / 76
type_1_cart <- (1) / 76
type_2_cart <- (4) / 76

# filtered c50
# 0  1
# 0 58  0
# 1  9  9
filtered_c50_error <- (0+9) / 76
type_1_c50 <- (0) / 76
type_2_c50 <- (9) / 76
  
# filtered nb
# 0  1
# 0 54  4
# 1 10  8
filtered_nb_error <-  (4+10) / 76
type_1_nb <- (4) / 76
type_2_nv <- (10) / 76

result_df <- data.frame(
  Model = c("O_CART", "F_CART", "F_C50", "F_NB"),
  Error_Rate = c(origin_cart_error, filtered_cart_error, filtered_c50_error, filtered_nb_error),
  Type_1_Error = c(type_1_origin, type_1_cart, type_1_c50, type_1_nb),
  Type_2_Error = c(type_2_origin, type_2_cart, type_2_c50, type_2_nv)
)
result_df%>%ggplot()+geom_col(aes(Model, Type_1_Error))
result_df%>%ggplot(aes(reorder(Model, Type_2_Error), Type_2_Error))+geom_col()+
  geom_text(aes(label = sprintf("%.2f", Type_2_Error)), 
            vjust = - 0.5, position = position_dodge(width = 0.9))+
  labs(x = "Model")

result_df%>%ggplot(aes(Type_2_Error, Type_1_Error, label = Model))+
  geom_point()+geom_label()+
  geom_text(aes(label = sprintf("Total error: %.2f", Error_Rate)), vjust = - 1)








