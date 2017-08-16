clean.df<-read.csv("clean_data.csv",header=T)

set.seed(224)
idx.train <- createDataPartition(y = clean.df$default.payment.next.month, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data
train <- clean.df[idx.train, ] # training set
test <-  clean.df[-idx.train, ]


library("e1071")
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("pROC")) install.packages("pROC"); library("pROC") 
if(!require("caret")) install.packages("caret"); library("caret")

df_nb<-naiveBayes(train$default.payment.next.month~.,data=train)##building Naive Bayes model
pred.nb<-predict(df_nb,newdata = test,type = "raw")
test$pred.nb<-pred.nb[,2]#probability of default(1=yes) is left，
test$pred.nb<-round(test$pred.nb,4)

predictions.roc.nb<-data.frame(nb=pred.nb[,2])###ploting ROC by using "HMeasure" package and getting AUC
n.test.nb<-as.numeric(test$default.payment.next.month)-1
h<-HMeasure(n.test.nb,predictions.roc.nb)
plotROC(h,which = 1)
h$metrics["AUC"]

##brier score#######################################################
y<-as.numeric(test$default.payment.next.month)
brier.nb<-sum((y-pred.nb)^2)/length(y)
sprintf("Naive bayes has a brier score of %.5f",brier.nb)
##################################################################cross validation for NaiveBayes

train.rnd<-train[sample(nrow(train)),]
head(train.rnd)
k<-5
folds<-cut(1:nrow(train.rnd),breaks=k,labels = FALSE)
head(folds)

results.nb<-data.frame(nb=numeric(length = k))
i<-1
for(i in 1:k){
  idx.val<-which(folds==i,arr.ind = TRUE)
  cv.train<-train.rnd[-idx.val,]
  cv.val<-train.rnd[idx.val,]
  nb<-naiveBayes(default.payment.next.month~.,data=cv.train)
  yhat.nb<-predict(nb,newdata = cv.val,type = "raw")
  yhat.nb1<-yhat.nb[,2]
  results.nb[i,"nb"]<-auc(cv.val$default.payment.next.month,as.vector(yhat.nb1))
}
