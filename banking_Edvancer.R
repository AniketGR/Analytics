rm(list=ls())
setwd("C:/Users/wel/Desktop/Edvancer/Bank_Project")
bank_data_train=read.csv("bank-full_train.csv",header = T,stringsAsFactors = F)
train=read.csv("bank-full_train.csv",header = T,stringsAsFactors = F)
View(bank_data_train)
bank_data_test=read.csv("bank-full_test.csv",header=T,stringsAsFactors = F)
bank_data_test$y=NA
View(bank_data_test)
bank_data_test$data="test"
bank_data_train$data="train"
bank_data_all=rbind(bank_data_test,bank_data_train)

library(dplyr)
glimpse(bank_data_all)
table(bank_data_all$job)
round(prop.table(table(bank_data_all$job,bank_data_all$y),1),2)


bank_data_all=bank_data_all %>%
  mutate(job2=as.numeric(job %in% c("blue-collar","entrepreneur","housemaid","services")),
         job3=as.numeric(job %in% c("management","unemployed")),
         job4=as.numeric(job == "retired"))%>%
  select(-job)

bank_data_all=bank_data_all %>%
  mutate(marital1=as.numeric(marital == "divorced"),
         marital2=as.numeric(marital == "married"))%>%
  select(-marital)

bank_data_all=bank_data_all %>%
  mutate(ed1=as.numeric(education == "secondary"),
         ed2=as.numeric(education == "tertiary"),
         ed3=as.numeric(education =="primary"))%>%
  select(-education)


table(bank_data_all$poutcome)
bank_data_all=bank_data_all%>%
  mutate(outcm1=as.numeric(poutcome=="unknown"),
         outcm2=as.numeric(poutcome=="failure"),
         outcm2=as.numeric(poutcome=="other"))%>%
  select(-poutcome)

bank_data_all=bank_data_all%>%
  mutate(cont1=as.numeric(contact=="cellular"),
         cont2=as.numeric(contact=="unknown"))%>%
  select(-contact)

bank_data_all$loan=as.numeric(bank_data_all$loan=="yes")
bank_data_all$default=as.numeric(bank_data_all$default=="yes")
bank_data_all$housing=as.numeric(bank_data_all$housing=="yes")

'bank_data_all=bank_data_all%>%
mutate(month1=as.numeric(month=="jan"),
month2=as.numeric(month=="feb"),
month3=as.numeric(month=="mar"),
month4=as.numeric(month=="apr"),
month5=as.numeric(month=="may"),
month6=as.numeric(month=="jun"),
month7=as.numeric(month=="jul"),
month8=as.numeric(month=="aug"),
month9=as.numeric(month=="sep"),
month10=as.numeric(month=="oct"),
month11=as.numeric(month=="nov"))%>%
select(-month)'

glimpse(bank_data_all)

bank_data_train=bank_data_all%>%
  filter(data=="train")%>%
  select(-data,-month)
bank_data_test=bank_data_all%>%
  filter(data=="test")%>%
  select(-data,-y,-month)

bank_data_train$y=as.numeric(bank_data_train$y=="yes")
glimpse(bank_data_train)

set.seed(12)
s=sample(1:nrow(bank_data_train),0.7*nrow(bank_data_train))
train_sample=bank_data_train[s,]
test_sample=bank_data_train[-s,]

library(car)
train_vif=lm(y~.-ID,data=train_sample)
sort(vif(train_vif),decreasing = T)[1:3]

train_vif=lm(y~.-ID-ed1-outcm1,data=train_sample)
summary(train_vif)
train_vif=step(train_vif)

formula(train_vif)

train_vif=lm(y ~ balance + housing + loan + duration + campaign + pdays + 
               previous + job2 + job4 + marital1 + marital2 + ed2 + 
               ed3 + outcm2 + cont2,data=train_sample)
fit_train=glm(y ~ balance + housing + loan + duration + campaign + pdays + 
                previous + job2 + job4 + marital1 + marital2 + ed2 + 
                ed3 + outcm2 + cont2,data=train_sample,family="binomial")



train_sample$predicted=predict(fit_train,train_sample,type = "response")
glimpse(train_sample)
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=round(seq(0,1,length=100),3)

for (cutoff in cutoffs){
  pred=as.numeric(train_sample$predicted>cutoff)
  
  real=train_sample$y
  TP=sum(pred== 1 & real==1)
  TN=sum(pred==0 & real==0)
  FP=sum(pred==1 & real==0)
  FN=sum(pred==0 & real==1)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP,Sn=TP/P, Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)

View(cutoff_data)
KS.cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]


Predicted=predict(fit_train,test_sample,type = "response")
glimpse(bank_data_test)
test_sample$Score=as.numeric(Predicted>KS.cutoff)
pred= test_sample$Score
real=test_sample$y
TP=sum(pred== 1 & real==1)
TN=sum(pred==0 & real==0)
FP=sum(pred==1 & real==0)
FN=sum(pred==0 & real==1)
P=TP+FN
N=TN+FP
KS=abs((TP/P)-(FP/N))
Total= P+N
Acc=((TP+TN)/(P+N))
table(test_sample$Score,test_sample$y)




Predicted_test=predict(fit_train,bank_data_test,type = "response")
glimpse(bank_data_test)
bank_data_test$Score=as.numeric(Predicted_test>KS.cutoff)
write.csv(Score,"Aniket_GuhaRoy_P5_part2.csv",row.names = F)


