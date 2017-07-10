install.packages("e1071")
library("e1071")

df_nb<-naiveBayes(train$default~.,data=train)
summary(df_nb)
pred.df<-predict(df_nb,newdata = test,type = "raw")
head(pred.df)
test$pred.df<-pred.df[,2]#probability of default(1=yes) is left，
summary(test$pred.df)

#as.numeric(default_tes$pred.df)
default_tes$pred.df<-round(default_tes$pred.df,4)

##brier score###############
y<-as.numeric(default_tes$default)-1
y
brier.nb<-sum((y-pred.df)^2)/length(y)
sprintf("Naive bayes has a brier score of %.5f",brier.nb)

########################################################################
install.packages("caret")
library("caret")

pred.df.class<-factor(test$pred.df>0.5,labels = c("no","yes"))
head(pred.df.class)
table(pred.df.class,test$default)
#######################################################################

######ROC&AUC##########################################################
if(!require("hmeasure"))install.packages("hmeasure");library("hmeasure")
predictions.roc<-data.frame(nb=pred.df[,2])
n.test<-as.numeric(test$default)-1
h<-HMeasure(n.test,predictions.roc)
plotROC(h,which = 1)
h$metrics["AUC"]

######################################################################cv
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("pROC")) install.packages("pROC"); library("pROC") 
if(!require("caret")) install.packages("caret"); library("caret")

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
  nb<-naiveBayes(default~.,data=cv.train)
  yhat.nb<-predict(nb,newdata = cv.val,type = "raw")
  yhat.nb1<-yhat.nb[,2]
  results.nb[i,"nb"]<-auc(cv.val$default,as.vector(yhat.nb1))
}
