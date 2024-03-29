install.packages("arulesViz")
install.packages('shiny')
install.packages('shinythemes')
library(tidyverse)
library(arules)

data <- read_csv("C:/Users/USER/Desktop/Groceries_dataset.csv")
data
# data itemDescription欄位的unique
data%>%group_by(itemDescription)%>%
  summarise(n = n())%>%filter(n > 500)%>%
  ggplot()+geom_col(aes(reorder(itemDescription, n), n))+coord_flip()+
  labs(x = 'itemDescription')

data%>%colnames()
# 將data按照Member_number然後Date排序
data <- data%>%arrange(Member_number, as.Date(Date, format = "%d-%m-%Y"))
data
# 迴圈配對unique的itemDescription
for (desc in data$itemDescription%>%unique()) {
  data[, desc] <- as.integer(data$itemDescription == desc)
}
new_data <- data%>%select(-itemDescription)
new_data

new_data <- new_data%>%
  group_by(Member_number, Date)%>%
  summarise(across(everything(), sum))%>%
  arrange(Member_number, as.Date(Date, format = "%d-%m-%Y"))

mt_data <- new_data[, -c(1, 2)]%>%as.matrix()
mt_data

# supp出現頻率至少為2%的項目集
# conf至少為2%
# maxlen最多10個項目
rule <- mt_data%>%apriori(parameter = list(supp = 0.01, conf = 0.02, maxlen = 10))
# rule <- mt_data%>%apriori()
rule%>%length()
# rule%>%head()%>%inspect()
# rule%>%head()%>%quality()

rule%>%sort(by = "support")%>%head(10)%>%inspect()
rule%>%sort(by = "confidence")%>%head(10)%>%inspect()

library(arulesViz)
#Heat map
plot(rule)
#Balloon plot
plot(rule, method = "grouped")

plot(rule, method = "graph", engine = "html")

# Parallel coordinates plot ()
plot(rule, method = "paracoord", control = list(reorder = TRUE))

library(shiny)
library(shinythemes)
ruleExplorer(rule)

