library(tidyverse)
library(psych)
library(car)
library(MASS)

# 目標：複回歸模型預測及檢測
data <- read_csv("/Users/wangqiqian/Desktop/資料探勘/data/Salary.csv")
# 這筆資料要預測Salary，而Gender/Education/Job/Experience 為質性變數
data%>%head()
# 資料quantile & min max
data%>%summary()
# 維度
data%>%dim()
# 缺失值？
all(is.na(data))

# 變數對label相關性，不將質性變數放入plot
# 可以看到年齡和工作經驗對於薪水有非常高度相關的
data%>%select(`Age`, `Years of Experience`, Salary)%>%pairs.panels()
data$Gender%>%unique()
data$`Job Title`%>%unique()
data$`Education Level`%>%unique()

# 將質性資料進行dummy
data$Gender<-case_when(
  data$Gender == "Male" ~ 3,
  data$Gender == "Female" ~ 2,
  is.na(data$Gender) ~ 1
)
data$`Education Level`<-case_when(
  data$`Education Level` == "Bachelor's" ~ 4,
  data$`Education Level` == "Master's" ~ 3,
  data$`Education Level` == "PhD" ~ 2,
  is.na(data$`Education Level`) ~ 1
)
data%>%group_by(Gender)%>%summarize(count = n())
data%>%group_by(`Education Level`)%>%summarize(count = n())
# 資料太多，計算所有資料數量並將最多的進行分類，其餘少數歸為一類
data$`Job Title`%>%unique()

count_job <- data%>%group_by(`Job Title`)%>%summarize(count = n())
count_job <- count_job[order(count_job$count, decreasing = TRUE), ]
count_job%>%head(20)
count_job%>%filter(count>1)%>%
  ggplot()+geom_col(aes(reorder(`Job Title`, count, median), count))+coord_flip()

# sort完成後看前面幾個最多人的工作進行allocated codes (ch. 8.4)
# 這樣分類可能不好，1 2 3為等距，但對於其他分類卻不是
count_job$count <- case_when(
  count_job$count > 3 ~ 3,
  count_job$count > 1 & count_job$count <= 3 ~ 2,
  TRUE ~ 1
)
count_one <- count_job%>%filter(count == 3)
count_zero <- count_job%>%filter(count == 2)

count_neg <- count_job%>%filter(count == 1)

for (i in 1:length(data$`Job Title`)){
  
  for (j in 1:length(count_one$`Job Title`)) {
    
    if (data$`Job Title`[i] %in% count_one$`Job Title`[j]) {
      
      data$`Job Title`[i] <- 3
    }
  }
  for (k in 1:length(count_zero$`Job Title`)) {
    
    if (data$`Job Title`[i] %in% count_zero$`Job Title`[k]) {
      
      data$`Job Title`[i] <- 2
    }
  }
  for (t in 1:length(count_neg$`Job Title`)) {
    
    if (data$`Job Title`[i] %in% count_neg$`Job Title`[t]) {
      
      data$`Job Title`[i] <- 1
    }
  }
}
data%>%group_by(`Job Title`)%>%summarize(count = n())%>%
  ggplot()+geom_col(aes(reorder(`Job Title`, count, median), count, fill = count))+
  title(xlab = "job level")
data%>%ggplot()+geom_boxplot(aes(`Job Title`, Salary))
data$`Job Title` <- as.numeric(data$`Job Title`)

# 8/2 split training & testing
smp_size <- floor(0.8 * nrow(data))
set.seed(123)

#data$`Years of Experience` <- data$`Years of Experience` + 1
#data <- log(data)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]
y_test <- test[6]
test <- test[-6]

# 先測試原始回歸模型
model <- lm(formula = Salary ~ ., data = train)
summary(model)
# H0:殘差間相互獨立
durbinWatsonTest(model) # 因為p-value < 0.05，拒絕H0，殘差沒有獨立
# H0:殘差變異數具有同質性
ncvTest(model) # 因為p-value < 0.05，拒絕H0，殘差變異數沒有同值性
# H0:殘差服從常態分配
shapiro.test(model$residual) # 因為p-value < 0.05，拒絕H0，誤差並非常態
anova(model)
plot(model$residuals)

# 預測及計算損失
#MAE分數 10461.01
pred <- predict(model,test)
sum(abs(y_test - pred)) / dim(test)[1]

anova(model)

# 檢測共線性
# https://www.cupoy.com/qa/club/ai_tw/0000016D6BA22D97000000016375706F795F72656C656173654B5741535354434C5542/0000017C558AF365000000036375706F795F72656C656173655155455354
vif(model)
# 使用VIF ? GVIF ?
# https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif

# 減少高共線性變數(ch. 7.6)
new_data <- data%>%select(-Age)
train <- new_data[train_ind, ]
y_test <- new_data[-train_ind, ]%>%select(Salary)
test <- new_data[-train_ind, ]%>%select(-Salary)
# 去掉了年齡後減少到 10038.65, 去掉了工作時間後增加到 11809.62
# 所以目前減少高相關的兩個預測變數的年齡可些微減少MAE分數
model <- lm(formula = Salary ~ ., data = train)
summary(model)
vif(model)
pred <- predict(model,test)
sum(abs(y_test - pred)) / dim(test)[1]
# full data correlation
data%>%pairs.panels()

# plots
# 教育和薪水
categorical_edu <- data
categorical_edu$`Education Level` <- as.character(categorical_edu$`Education Level`)
categorical_edu%>%filter(Gender != 1)%>%ggplot()+geom_boxplot(aes(`Education Level`, Salary))

categorical_edu%>%group_by(`Education Level`, `Job Title`)%>%summarize(avg_salary = sum(Salary)/n())%>%
  filter(!is.na(avg_salary))%>%ggplot()+geom_col(aes(`Education Level`,avg_salary, fill = `Job Title`),position='dodge')
# 工作和薪水
categorical_job <- data
categorical_job$`Job Title` <- as.character(categorical_job$`Job Title`)
categorical_job%>%ggplot()+geom_boxplot(aes(reorder(`Job Title`, Salary, median), Salary))
# 性別和薪水
categorical_gender <- data
categorical_gender$Gender <- as.character(categorical_gender$Gender)
categorical_gender%>%filter(Gender != 1)%>%ggplot()+geom_boxplot(aes(Gender, Salary))

# 從教育和薪水可以看到有幾個outlier，所以這裡去除掉那些oulier的row
train%>%filter(`Education Level` == 1)%>%summary()
train%>%filter(`Education Level` == 2)%>%summary()
train%>%filter(`Education Level` == 3)%>%summary()

summary(train%>%filter(`Education Level` == 2))
train%>%filter(`Education Level` == 2 & (Salary > 160000+1.5*10000 | Salary < 150000-1.5*10000))
train%>%filter(`Education Level` == 4 & Salary > 95000+1.5*50000)
train <- train%>%filter(
  !(`Education Level` == 4 & Salary > 95000+1.5*50000) &
  !(`Education Level` == 2 & (Salary > 160000+1.5*12500 | Salary < 150000-1.5*12500))
  )
train%>%filter(`Education Level` != 1)%>%
  ggplot()+geom_boxplot(aes(as.character(`Education Level`),Salary))
#重新預測去除掉離群值的資料
#降低到9664.665
model <- lm(formula = Salary ~ ., data = train)
summary(model)
pred <- predict(model,test)
sum(abs(y_test - pred)) / dim(test)[1]

plot(model$residuals)


# 嘗試增加交互項，看交互作用是否幫助回歸模型(ch. 8.5)
# 先去除Age，然後針對性別、教育、工作(質性變數)對工作經驗(計量變數)新增交互變數
complex_data <- data%>%select(-Age)
complex_data$Education_year <- complex_data$`Education Level` * complex_data$`Years of Experience`
complex_data$Job_year <- as.numeric(complex_data$`Job Title`) * complex_data$`Years of Experience`
complex_data$Gender_year <- complex_data$Gender * complex_data$`Years of Experience`
complex_data%>%head()
complex_data%>%pairs.panels()

train <- complex_data[train_ind, ]
train <- train%>%filter(
  !(`Education Level` == 4 & Salary > 95000+1.5*50000) &
    !(`Education Level` == 2 & (Salary > 160000+1.5*12500 | Salary < 150000-1.5*12500))
)
y_test <- complex_data[-train_ind, ]%>%select(Salary)
test <- complex_data[-train_ind, ]%>%select(-Salary)
complex_model <- lm(formula = Salary ~ ., data = train)
# MAE增加到10230.64
pred <- predict(complex_model, test)
sum(abs(y_test - pred)) / dim(test)[1]


# 檢測是否需要交互項和(ch. 7.3 檢定所有新增斜率)
# 虛無假設：三個新增的交互項斜率為0，對立假設：非全為0。檢定統計量為[SSR(新增的交互項|原有變數)/(3-1)]/[SSE(全部變數)/7-3]
# SSR(Education_year, Job_year, Gender_year | Gender, `Education Level`, `Job Title`, `Years of Experience`) =
# SSR(full var) - SSR(Gender, `Education Level`, `Job Title`, `Years of Experience`)
# F* ~ F(7-4, 75-7) = F(3, 68), alpha = 0.05
aov_data <- summary(aov(Salary ~ ., data = train))
aov_data
specific_aov_data <- summary(aov(Salary ~ Gender+`Education Level`+`Job Title`+`Years of Experience`, data = train))

full_SSR <- sum(aov_data[[1]]$`Sum Sq`[1:7])
specific_SSR <- sum(specific_aov_data[[1]]$`Sum Sq`[1:4])
full_MSE <- aov_data[[1]]$`Mean Sq`[8]
# 由於下列所計算出來的F檢定統計量4.558491大於查表所得到的2.76，所以拒絕虛無假設
# 也就是說有足夠的信心(95%)說明加入的交互項在該回歸模型下有可以解釋一些預測，也就是加入的交互像有邊際增量
F_result <- ((full_SSR - specific_SSR)/3)/full_MSE
F_result

train <- train%>%select(-Education_year, -Gender_year)
complex_model <- lm(formula = Salary ~ ., data = train)
# 9577.544
pred <- predict(complex_model, test)
sum(abs(y_test - pred)) / dim(test)[1]
vif(complex_model)

# 進一步確認：逐步回歸
full <- formula(complex_model)
model0 <- lm(Salary ~ 1,data = train)
step(model0 , direction = "both",scope = full)
# 加入的交互項並不會見少誤差
aic_model <- lm(formula = Salary ~ Gender_year + `Education Level` + Education_year + 
                  `Job Title`, data = train)
pred <- predict(aic_model, test)
# 9983.043
sum(abs(y_test - pred)) / dim(test)[1]
#train <- train%>%select(-`Years of Experience`)
vif(aic_model)

# 增加平方及三次方項
complex_data <- data
complex_data$Education_year <- complex_data$`Education Level` * complex_data$`Years of Experience`
complex_data$Job_year <- as.numeric(complex_data$`Job Title`) * complex_data$`Years of Experience`
complex_data$Gender_year <- complex_data$Gender * complex_data$`Years of Experience`
complex_data$square_age <- complex_data$Age^2
complex_data$square_gender <- complex_data$Gender^2
complex_data$square_edu <- complex_data$`Education Level`^2
complex_data$square_experience <- complex_data$`Years of Experience`^2
complex_data$cube_age <- complex_data$Age^3
complex_data$cube_gender <- complex_data$Gender^3
complex_data$cube_edu <- complex_data$`Education Level`^3
complex_data$cube_experience <- complex_data$`Years of Experience`^3

complex_data%>%
  select(square_age,square_gender,square_edu,square_experience,
         cube_age,cube_gender,cube_gender,cube_edu,cube_experience,Salary)%>%
  pairs.panels()

train <- complex_data[train_ind, ]
train <- train%>%filter(
  !(`Education Level` == 4 & Salary > 95000+1.5*50000) &
    !(`Education Level` == 2 & (Salary > 160000+1.5*12500 | Salary < 150000-1.5*12500))
)
y_test <- complex_data[-train_ind, ]%>%select(Salary)
test <- complex_data[-train_ind, ]%>%select(-Salary)

complex_model <- lm(formula = Salary ~ ., data = train)
summary(complex_model)
full <- formula(complex_model)
model0 <- lm(Salary ~ 1,data = complex_data)
step(model0 , direction = "both",scope = full)
# 增加交互以及平方和次方項的預測變數後的最低aic模型
aic_model <- lm(formula = Salary ~ `Years of Experience` + cube_edu + `Job Title` + 
                 Gender_year + Age + square_experience + cube_age + cube_experience, 
               data = complex_data)
complex_data
# 高相關!=高共線性
pred <- predict(aic_model, test)
# 10257.33
sum(abs(y_test - pred)) / dim(test)[1]

# H0:殘差間相互獨立
durbinWatsonTest(aic_model) # 因為p-value < 0.05，拒絕H0，殘差沒有獨立
# H0:殘差變異數具有同質性
ncvTest(aic_model) # 因為p-value < 0.05，拒絕H0，殘差變異數沒有同質性
# H0:殘差服從常態分配
shapiro.test(aic_model$residual) # 因為p-value < 0.05，拒絕H0，誤差並非常態

log_complex_data <- log(complex_data)
log_complex_data <- log_complex_data%>%filter(!is.infinite(`Years of Experience`))
aic_model <- lm(formula = Salary ~ `Years of Experience` + cube_edu + `Job Title` + 
                  Gender_year + Age + square_experience + cube_age + cube_experience, 
                data = log_complex_data)

durbinWatsonTest(aic_model)
ncvTest(aic_model)
shapiro.test(aic_model$residual) 

# 處理成 sqrt ，由於誤差不符合常態
sqrt_complex_data <- sqrt(complex_data)
train <- sqrt_complex_data[train_ind, ]
y_test <- sqrt_complex_data[-train_ind, ]%>%select(Salary)
test <- sqrt_complex_data[-train_ind, ]%>%select(-Salary)

aic_model <- lm(formula = Salary ~ `Years of Experience` + cube_edu + `Job Title` + 
                  Gender_year + Age + square_experience + cube_age + cube_experience, 
                data = train)

durbinWatsonTest(aic_model)
ncvTest(aic_model)
shapiro.test(aic_model$residual)

robust_model <- rlm(formula = Salary ~ train$`Years of Experience` + train$cube_edu + train$`Job Title` + 
                   train$Gender_year + train$Age + train$square_experience + train$cube_age + train$cube_experience,
                data = train)

#robust_model <- rlm(formula = Salary ~ sqrt_complex_data$`Years of Experience` + sqrt_complex_data$cube_edu + sqrt_complex_data$`Job Title` + 
#                      sqrt_complex_data$Gender_year + sqrt_complex_data$Age + sqrt_complex_data$square_experience + sqrt_complex_data$cube_age + sqrt_complex_data$cube_experience,
#                    data = sqrt_complex_data)

durbinWatsonTest(robust_model)
ncvTest(robust_model)
shapiro.test(robust_model$residual)

#pred <- predict(robust_model, test)
#sum(abs(y_test - pred)) / dim(test)[1]

# 最低的誤差
complex_data$Job_year <- as.numeric(complex_data$`Job Title`) * complex_data$`Years of Experience`
complex_data$`Job Title` <- as.numeric(complex_data$`Job Title`)
origin <- complex_data%>%select(Gender, `Education Level`, `Job Title`, `Years of Experience`, Job_year, Salary)

train <- origin[train_ind, ]%>%filter(
  !(`Education Level` == 4 & Salary > 95000+1.5*50000) &
    !(`Education Level` == 2 & (Salary > 160000+1.5*12500 | Salary < 150000-1.5*12500))
)
y_test <- complex_data[-train_ind, ]%>%select(Salary)
test <- complex_data[-train_ind, ]%>%select(-Salary)

model <- lm(formula = Salary ~ ., data = train)
summary(model)
pred <- predict(model, test)
# 9447.786
sum(abs(y_test - pred)) / dim(test)[1]
# https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
vif(model)
BIC(model)# 由於資料維度不大，不需要更大的懲罰項來處理

durbinWatsonTest(model)
ncvTest(model)
shapiro.test(model$residual) 

# 標準化後逐步回歸
origin <- complex_data%>%select(-Salary)%>%scale()
origin <- as.data.frame(cbind(origin, complex_data$Salary))
origin
train <- origin[train_ind, ]
y_test <- origin[-train_ind, ]%>%select(V17)
test <- origin[-train_ind, ]%>%select(-V17)
model <- lm(formula = V17 ~ ., data = train)
summary(model)

full <- formula(model)
model0 <- lm(V17 ~ 1, data = train)
step(model0 , direction = "both",scope = full)

new_model <- lm(formula = V17 ~ `Years of Experience` + cube_edu + Gender_year + 
                  Age + `Job Title`, data = train)
pred <- predict(new_model, test)
sum(abs(y_test - pred)) / dim(test)[1]

durbinWatsonTest(new_model)
ncvTest(new_model)
shapiro.test(new_model$residuals) 
