library(caret)
library(C50)
library(randomForest)
data<-read.csv("DATA.csv")
data$ETI<-as.factor(data$ETI)
# 66-34,50-50,80-20,10 f cross validation
smp_size <- floor (0.50*nrow(data))
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
ctrl <- trainControl(method="cv", summaryFunction=twoClassSummary, classProbs=T,
                     savePredictions = 'all', search='random')#cv-covariance,
                    #hp and lp two classes(training parameters)
#naive bayes(probability based)
fit1<-train(ETI~.,train,method='nb',trControl=ctrl)
pred1<-predict(fit1,test,type='raw')
nb<-table(pred1,test$ETI,dnn = c("predicted","actual"))
nb
#svm(function based)
fit2<-train(ETI~.,train,method='svmRadial',trControl=ctrl)
pred2<-predict(fit1,test,type='raw')
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
tc=trainControl(method = "repeatedcv",number = 10,repeats = 3,classProbs = T)
set.seed(123)
models<-caretList(ETI~.,train,trControl = ctrl,methodList = methods)
stack<-caretStack(models,method="C5.0",trcontrol=ctrl)
pred5<-predict(stack,test,type="raw")
sc<-table(pred5,test$ETI,dnn = c("predicted","actual"))
sc
confusionMatrix(nb)
confusionMatrix(svm)
confusionMatrix(rf)
confusionMatrix(dt)
confusionMatrix(sc)



#roc
library(MLeval)
roc<-evalm(list(fit1,fit2,fit3,fit4,stack$ens_model),gnames =
             c("NB","SVM","RF","DT","IDT")) 

