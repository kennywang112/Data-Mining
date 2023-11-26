library(tidyverse)
library(party)

data <- read_csv("/Users/wangqiqian/Desktop/資料探勘/cancer_prediction_dataset.csv")

data%>%summary()
data%>%dim()
all(is.na(data))
# EDA
gender_cancer <- data%>%group_by(Gender, Cancer)%>%summarize(count = n(), percent = count/10000)
pie(x=gender_cancer$percent,
    labels = paste('Gender:',gender_cancer$Gender, 'Cancer: ',
                   gender_cancer$Cancer, '\n', gender_cancer$percent))

# 各個年齡得癌症比例
age_data <- data
age_data$Age_split <- case_when(
  data$Age < 20 ~ "<20",
  data$Age < 30 & data$Age >= 20 ~ "20~29",
  data$Age < 40 & data$Age >= 30 ~ "30~39",
  data$Age < 50 & data$Age >= 40 ~ "40~49",
  data$Age < 60 & data$Age >= 50 ~ "50~59",
  data$Age < 70 & data$Age >= 60 ~ "60~69",
  data$Age < 80 & data$Age >= 70 ~ "70~79",
  data$Age < 90 & data$Age >= 80 ~ "80~89",
  data$Age >= 90 ~ "90~"
)

age_cancer <- age_data%>%group_by(Age_split, Cancer)%>%summarize(count = n())
age_cancer%>%group_by(Age_split)%>%summarize(sum(count))
age_cancer$percent <- case_when(
  age_cancer$Age_split == "<20" ~ age_cancer$count/241,
  age_cancer$Age_split == "20~29" ~ age_cancer$count/1170,
  age_cancer$Age_split == "30~39" ~ age_cancer$count/1205,
  age_cancer$Age_split == "40~49" ~ age_cancer$count/1155,
  age_cancer$Age_split == "50~59" ~ age_cancer$count/1226,
  age_cancer$Age_split == "60~69" ~ age_cancer$count/1222,
  age_cancer$Age_split == "70~79" ~ age_cancer$count/1236,
  age_cancer$Age_split == "80~89" ~ age_cancer$count/1212,
  age_cancer$Age_split == "90~" ~ age_cancer$count/1333
)
#age_cancer%>%ggplot(aes(Age_split, count, fill = as.character(Cancer)))+geom_col()+
#  geom_text(label = round(age_cancer$percent, 3), position = position_dodge(0.9),vjust=0)
age_cancer%>%filter(Cancer == 1)%>%ggplot()+geom_col(aes(Age_split, percent, fill = Age_split))

data%>%group_by(Smoking)%>%summarize(Percent_cancer = sum(Cancer)/n())
data%>%group_by(Fatigue)%>%summarize(Percent_cancer = sum(Cancer)/n())
data%>%group_by(Allergy)%>%summarize(Percent_cancer = sum(Cancer)/n())

library(tree)
set.seed(1)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train_data <- data[ind == 1, ]
test_data <- data[ind == 2, ]
tree_model <- tree(Cancer ~ Gender + Age, data = train_data)
tree_model
plot(tree_model, type = "uniform")











