setwd("C:/Users/wel/Desktop/Edvancer/REal Estate_project")
rm(list = ls())
train_data=read.csv("housing_train.csv",header = T,stringsAsFactors = F)
library(dplyr)
glimpse(train_data)

test_data=read.csv("housing_test.csv",header = T,stringsAsFactors = F)
glimpse(train_data)
glimpse(test_data)

for (i in 1:nrow(train_data)) {
  if(train_data[i,16]==""){
    train_data[i,16]="Boroondara"
  }
}

for (i in 1:nrow(test_data)) {
  if(test_data[i,15]==""){
    test_data[i,15]="Boroondara"
  }
}


for (cols in names(train_data)){
  if (class(train_data[,cols])=="integer"){
    train_data[is.na(train_data[,cols]),cols]=round(mean(train_data[,cols],na.rm = T),1)
  }
}


for (cols in names(test_data)){
  if (class(test_data[,cols])=="integer"){
    test_data[is.na(test_data[,cols]),cols]=round(mean(test_data[,cols],na.rm = T),1)
  }
}

for (cols in 1:ncol(train_data)) {
  if (class(train_data[,cols])=="character"){
    
    train_data[,cols]=as.factor(train_data[,cols])
  }
}

for (cols in 1:ncol(test_data)) {
  if (class(test_data[,cols])=="character"){
    
    test_data[,cols]=as.factor(test_data[,cols])
  }
}
test_data=test_data %>%
  select(-Suburb,-Address,-SellerG,-Postcode)

train_data=train_data %>%
  select(-Suburb,-Address,-SellerG,-Postcode)

set.seed(16)
s=sample(1:nrow(train_data),0.7*nrow(train_data))
train=train_data[s,]
test=train_data[-s,]

#Decision Tree

library(tree)
tree.train=tree(Price~.,data=train,na.action=na.exclude)
summary(tree.train)
sum((test$Price-predict(tree.train,newdata=test))**2) %>%
  sqrt()
#Random Forest
library(randomForest)
forest.train=randomForest(Price~.,data=train,na.action=na.exclude)
sum((test$Price-predict(forest.train,newdata=test))**2) %>%
  sqrt()



write.csv(test_predicted,"Aniket_GuhaRoy_P1_part2.csv",row.names = F)