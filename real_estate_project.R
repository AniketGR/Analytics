setwd("C:/Users/wel/Desktop/Edvancer/REal Estate_project")
rm(list = ls())

train_data=read.csv("housing_train.csv",header = T,stringsAsFactors = F)
glimpse(train_data)

test_data=read.csv("housing_test.csv",header = T,stringsAsFactors = F)

CreateDummies=function(data,var){
  tt=table(data[,var])
  categories=names(tt)[-1]
  for (cats in categories){
    name=paste(var,cats,sep = "_")
    name=gsub(" ","",name)
    data[,name]=as.numeric(data[,var]==cats)
  }
  return(data)
}

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

table(test_data$CouncilArea)

# Decision tree #
library(dplyr)
train_data=train_data%>%
  na.omit()

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




glimpse(train_data)
glimpse(test_data)
train_data[is.na(train_data[,"CouncilArea"]),"CouncilArea"]
train_data[,"CouncilArea"]
getmode(train_data[,"Suburb"])
table(train_data[,"CouncilArea"])
getmode <- function(train_data) {
  uniqv <- unique(train_data)
  uniqv[which.max(tabulate(match(train_data, uniqv)))]
}
sum(is.na(train_data[,"Method"]))
glimpse(train_data)


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




train_data[is.na(train_data[,cols]),cols]=getmode(train_data[,cols])
test_data=test_data%>%
  na.omit()
sum(is.na(test_data))
glimpse(test_data)
for (cols in 1:ncol(test_data)) {
  if (class(test_data[,cols])=="character"){
    test_data[,cols]=as.factor(test_data[,cols])
  }
}



test_data=test_data %>%
  select(-Suburb,-Address,-SellerG,-Postcode)

train_data=train_data %>%
  select(-Suburb,-Address,-SellerG,-Postcode)

test_data=test_data %>%
  select(-Postcode)
glimpse(train_data)

glimpse(test_data)

levels(train_data$CouncilArea)

set.seed(16)
s=sample(1:nrow(train_data),0.7*nrow(train_data))
train=train_data[s,]
test=train_data[-s,]
library(tree)
library(ISLR)
glimpse(train)


train_data=train_data %>%
  select(-SellerG)
glimpse(train_data)
tree.train=tree(Price~.,data=train,na.action=na.exclude)
summary(tree.train)

sum((test$Price-predict(tree.train,newdata=test))**2) %>%
  sqrt()

test_predicted=predict(tree.train,newdata = test)
View(test)
cor(test_predicted,test$Price)
res=test$Price-test_predicted
rmse_test=sqrt(mean(res^2))
glimpse(test)

#random forest#
library(randomForest)
forest.train=randomForest(Price~.,data=train,na.action=na.exclude)

test_predicted=predict(forest.train,newdata = test)
test_predicted=predict(forest.train,newdata = test_data)
View(test)
cor(test_predicted,test$Price)
res=test$Price-test_predicted
rmse_test=sqrt(mean(res^2))

write.csv(test_predicted,"Aniket_GuhaRoy_P1_part2.csv",row.names = F)

#prediction on test data
cor(test_predicted,test_data$Price)
res=test$Price-test_predicted
rmse_test=sqrt(mean(res^2))

#linnear regression

col_list=c("Type","Method","CouncilArea")

#
#round(prop.table(table(train_data$SellerG,train_data$Price),1),2)


for (col in col_list){
  train_data=CreateDummies(train_data,col)
}
library(dplyr)

table(train_data$`SellerG_hockingstuart/Village`)


train_data=train_data %>%
  select(-Suburb,-Address,-Type,-Method,-SellerG,-CouncilArea,-Bedroom2,-Bathroom,-Car,-Postcode)
glimpse(train_data)

train_data=train_data %>%
  select(-Postcode,-Car)
glimpse(train_data)

for (cols in names(train_data)){
  train_data[is.na(train_data[,cols]),cols]=round(mean(train_data[,cols],na.rm = T),1)
}


set.seed(4)
s=sample(1:nrow(train_data),0.75*nrow(train_data))
train=train_data[s,]
test=train_data[-s,]

fit_train=lm(Price~.,data=train)
library(car)
sort(vif(fit_train),decreasing = T)[1:3]

summary(fit_train)
fit_train=step(fit_train)

formula(fit_train)

fit_train=lm(Price ~ Rooms + Distance + Landsize + BuildingArea + YearBuilt + 
               Type_t + Type_u + Method_S + CouncilArea_Banyule + CouncilArea_Bayside + 
               CouncilArea_Boroondara + CouncilArea_Brimbank + CouncilArea_Darebin + 
               CouncilArea_GlenEira + CouncilArea_HobsonsBay + CouncilArea_Hume + 
               CouncilArea_Maribyrnong + CouncilArea_Melbourne + 
               CouncilArea_MooneeValley + CouncilArea_Moreland + CouncilArea_PortPhillip + 
               CouncilArea_Stonnington + CouncilArea_Whitehorse + CouncilArea_Yarra,data = train)

summary(fit_train)

cor(test)

test$predicted=predict(fit_train,newdata = test)
test$predicted=predict_price
View(test)
cor(test$Price,test$predicted)
plot(test$Price,test$predicted)

plot(fit_train,which = 1)

res=test$Price-test$predicted
rmse_test=sqrt(mean(res^2))


categories=names(tt)
transaction_data=data.frame(colname='abc',sum=0,stringsAsFactors = F)
for(cats in categories){
transaction=sum(train_data$Price[which(train_data$SellerG==cats)])
transaction_data=rbind(transaction_data,c(cats,transaction))
}
View(transaction_data)
transaction_data %>%
  arrange(desc(sum))

#council area, maximum price

tt=table(train_data$CouncilArea)
categories=names(tt)
transaction_data=data.frame(colname='abc',sum=0,stringsAsFactors = F)
for(cats in categories){
  transaction=max(round(var(train_data$Price[which(train_data$CouncilArea==cats)])),2)
  transaction_data=rbind(transaction_data,c(cats,transaction))
}
View(transaction_data)

transaction_data %>%
  arrange(desc(sum))

        
        
seller_transaction=sum(train_data$Price[which(train_data$SellerG=='Walsh')])

for (i in 1:10) {
  data[,i]=i
}


mean(train_data$Price[which(train_data$Type=='h')])-mean(train_data$Price[which(train_data$Type=='t')])
