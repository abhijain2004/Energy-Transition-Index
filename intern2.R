library(caret)
library(C50)
library(randomForest)
data<-read.csv("ETII.csv")
View(data)
data$ETI<-as.factor(data$ETI)
data$Energy_imports_net<-as.numeric(data$Energy_imports_net)
data$Quality_Electricity_Supply<-as.numeric(data$Quality_Electricity_Supply)
data$Transparency<-as.numeric(data$Transparency)
data$year<- as.numeric(data$year)
data$Country<- as.numeric(data$Country)
# 66-34,50-50,80-20,10 f cross validation
smp_size <- floor (0.8*nrow(data))
set.seed(232)
train_d <-sample(seq_len(nrow(data)),size=smp_size)
train<- data[train_d,]
test<-data[-train_d,]
str(data)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
set.seed(123)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  trainIndexes <- which(folds==i,arr.ind=TRUE)
  test <- data
  train <- data[trainIndexes, ]
  #Use the test and train data partitions however you desire...
}


View(train)
View(test)
ctrl <- trainControl(method="repeatedcv",repeats=3, summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = 'all', search='random')#cv-covariance,
#hp and lp two classes(training parameters)
#naive bayes(probability based)
fit1<-train(ETI~.,train,method='nb',trControl=ctrl)
pred1<-predict(fit1,test,type='raw')
nb<-table(pred1,test$ETI,dnn = c("predicted","actual"))
nb
#svm(function based)
fit2<-train(ETI~.,train,method='svmRadial',trControl=ctrl)
pred2<-predict(fit2,test,type='raw')
svm<-table(pred2,test$ETI,dnn = c("predicted","actual"))
svm
#random forest(ensemmble technique)
fit3<-train(ETI~.,train,method='rf',trControl=ctrl)
pred3<-predict(fit3,test,type='raw')
rf<-table(pred3,test$ETI,dnn = c("predicted","actual"))
rf
#decision tree
fit4<-train(ETI~.,train,method='C5.0',trControl=ctrl)
pred4<-predict(fit4,test,type='raw')
dt<-table(pred4,test$ETI,dnn = c("predicted","actual"))
dt
#stacked classifier
library(caretEnsemble)

methods<-c("rf","rf","rf")

tc = trainControl(method = "repeatedcv", number = 10, 
                  repeats = 3, classProbs = T)
models = caretList(ETI~., data = train, 
                   trControl = ctrl, methodList = methods)
stack = caretStack(models, method="nb", trControl = ctrl)

pred5<-predict(stack, test, type = "raw")
length(pred5)
sc<-table(pred5,test$ETI,dnn = c("predicted","actual"))
confusionMatrix(nb)
confusionMatrix(svm)
confusionMatrix(rf)
confusionMatrix(dt)
confusionMatrix(sc)
#roc
stack1<-my_name
library(MLeval)
roc<-evalm(list(fit1,fit2,fit3,fit4,stack$ens_model),gnames =
             c("NB","SVM","RF","DT","INB"))
#Explainable AI

library(dplyr)
library(ggplot2)
library(scales)
library(breakDown)
library(tidyverse)
library(readr)
library(gridExtra)
library(grid)
library(ggridges)
library(ggthemes)
library(libcoin)
library(mvtnorm)
model_imp<-varImp(fit3,scale=T)
p1=model_imp$importance%>%as.data.frame()%>%rownames_to_column()%>%
ggplot(aes(x=reorder(rowname,Overall),y=Overall))+geom_bar(stat="identity",fill="red",alpha=0.8)+
      coord_flip()  
p2=train%>%ggplot(aes(x=ETI,fill=ETI))+geom_bar(alpha=0.8)+scale_fill_tableau()+guides(fill=F)
p3=train%>%gather(x,y,EP_Industry:Stability_of_policy)%>%ggplot(aes(x=y,y=ETI,color=ETI,fill=ETI))+
  facet_wrap(~x,scale="free",ncol = 3)+scale_fill_tableau()+scale_color_tableau()+
  geom_density_ridges(alpha=0.5)+guides(fill=F,color=F)
grid.arrange(p2,p3,ncol=2,widths=c(0.4,0.9))
test1<-read.csv("testdata.csv")
predict.function=function(model,new_obs){
  predict(model,new_obs,type="prob")[,2]
}
predict.function(fit3,test1[1,])
br=broken(model=fit3,new_observation =test1[1,],data=train,
baseline="Intercept",predict.function=predict.function,keep_distributions=T)

data.frame(y=br$contribution,x=br$variable)%>%ggplot(aes(x=reorder(x,y),y=y))+
  geom_bar(stat="identity",fill="blue",alpha=0.8)+coord_flip()
plot(br,plot_distributions = T)





