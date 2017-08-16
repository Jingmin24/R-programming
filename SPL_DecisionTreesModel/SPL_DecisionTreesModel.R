if(!require("rpart")) install.packages("rpart"); library("rpart")
if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
library(pROC)


#read data
clean.df<-read.csv("dummyCredict.csv")
#creat the train and test set with 80% and 20%
set.seed(1234)
idx.train <- createDataPartition(y = clean.df$default.payment.next.month, 
                                 p = 0.8, list = FALSE) 

clean.df.train <- clean.df[idx.train, ] 
clean.df.test <-  clean.df[-idx.train, ] 

#use pruning to decrease the probability of overfitting of the model
rpart.control=rpart.control(minsplit = 6, minbucket = 6,
                            cp=0.001, xval = 5, maxdepth = 6)

#build the decision model#
dt<-rpart(default.payment.next.month ~ ., data = clean.df.train, 
          parms = list(split = "information"), 
          method="class", control = rpart.control)

#find the optimal value of cp which minimum the xerror
printcp(dt)
#results
#CP           nsplit  rel error  xerror     xstd
#4 0.0010593      5   0.79892 0.80624 0.011282
#5 0.0010000      7   0.79680 0.81221 0.011315  ###cp=0.01 is the optimal value

##plot the tree
rpart.plot(dt, type = 2, extra = 104,tweak = 0.8, cex=0.7)
prp(dt, extra = 104, border.col = 0,cex = 0.7)


##predict the test set
pred.dt <- predict(dt, newdata = clean.df.test, type = "prob")[, 2]
pred.dt.class <-as.matrix(factor(ifelse(pred.dt > 0.5, "good","default")))
#####calculate the AUC and check the confusion matrix
auc.dt <- auc(clean.df.test$default.payment.next.month, pred.dt) 
confusionMatrix(pred.dt.class, clean.df.test$default.payment.next.month)

###print the ROC curve
plot.roc(clean.df.test$default.payment.next.month, pred.dt, print.auc=TRUE,col="blue")

