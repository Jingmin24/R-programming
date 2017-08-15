clean.df<-read.csv("clean_data.csv",header=T)

library("lattice")
library("ggplot2")
library("caret")
set.seed(224)
idx.train <- createDataPartition(y = clean.df$default.payment.next.month, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data
train <- clean.df[idx.train, ] # training set
test <-  clean.df[-idx.train, ]

train_lr<-train
test_lr<-test
train_lr[,(13:17)]<-NULL
test_lr[,(13:17)]<-NULL

##############building logit regression model##############################
lr<-glm(default.payment.next.month~.,data=train_lr,family = binomial(link="logit"))
summary(lr)
pred.lr <- predict(lr, newdata = test_lr,type = "response")
round(coef(summary(lr)),4)
head(pred.lr)

#####################################################################auc

if(!require("hmeasure"))install.packages("hmeasure");library("hmeasure")
predictions.roc.lr<-data.frame(lr=pred.lr)
n.test.lr<-as.numeric(test_lr$default.payment.next.month)
h<-HMeasure(n.test.lr,predictions.roc.lr)
plotROC(h,which = 1)
h$metrics["AUC"]

######################################################################
###cross validation for Logit 
train.rnd.lr<-train_lr[sample(nrow(train_lr)),]
k<-5
folds<-cut(1:nrow(train.rnd.lr),breaks=k,labels = FALSE)
head(folds)
results.lr<-data.frame(lr=numeric(length = k))
for(i in 1:k){
  idx.val<-which(folds==i,arr.ind = TRUE)
  cv.train<-train.rnd.lr[-idx.val,]
  cv.val<-train.rnd.lr[idx.val,]
  cv.lr<-glm(default.payment.next.month~.,data=cv.train,family = binomial(link="logit"))
  cv.pred.lr <- predict(lr, newdata = cv.val,type = "response")
  results.lr[i,"lr"]<-auc(cv.val$default.payment.next.month,as.vector(cv.pred.lr))
}
