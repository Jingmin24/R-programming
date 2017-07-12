
#####################################################################ROC
if(!require("hmeasure"))install.packages("hmeasure");library("hmeasure")

predictions.roc.all<-data.frame(nb=pred.nb[,2],lr=pred.lr)
n.test<-as.numeric(test$default.payment.next.month)-1
h<-HMeasure(n.test,predictions.roc.all)
plotROC(h,which = 1)


##################################################################Build Confusion Matrix to get Accuracy values 
yhat.validation<-c(list("lr"=pred.lr,"nb"=pred.nb[,2],"nn"=yhat.nn))
tau<-0.5
lr.class<-factor(yhat.validation$lr>tau,labels = c("0","1"))
nb.class<-factor(yhat.validation$nb>tau,labels=c("0","1"))

confusionMatrix(data=lr.class,reference = test$default.payment.next.month,positive = "0")#0.8196
confusionMatrix(data=nb.class,reference = test$default.payment.next.month,positive = "0")#0.7081
