###############################random forest###########
##### load the package#################################

if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("randomForest")) install.packages("randomForest");
library(pROC)
library("randomForest")

######read data##################
clean.df.dumm<-read.csv("dummyCredict.csv")

##creat train and test sample withe 80% and 20% of total data#######
set.seed(1234)
idx.train <- createDataPartition(y = clean.df.dumm$default.payment.next.month,
                                 p = 0.8, list = FALSE) 

# Assign the train and test set by index
clean.df.train <- clean.df.dumm[idx.train, ] 
clean.df.test <-  clean.df.dumm[-idx.train, ] 

##cross validation for the train dataset
k <- 3
set.seed(1234)

model.control <- trainControl(method = "cv", number = k, classProbs = TRUE, 
                              summaryFunction = twoClassSummary, allowParallel = TRUE )
##make a search grid of values to test optimal value in "mtry"
rf.parms <- expand.grid(mtry =1:10)
# Train model with a 3-fold cross validation 
rf.caret <- train(default.payment.next.month~., data = clean.df.train, importance = T, 
                  method = "rf", ntree = 500, tuneGrid = rf.parms, 
                  metric = "ROC", trControl = model.control)

# plot the performance of the model with different grid##
plot(rf.caret)

# test the model with predict function#
pred.rf.caret   <- predict(rf.caret, newdata = clean.df.test, type = "prob")[,2]

threshold<-0.5
pred.rf.class <-as.matrix(factor(ifelse(pred.rf.caret > threshold, "good",  "default")))

#check the confusion matrix and AUC#
confusionMatrix(pred.rf.class, clean.df.test$default.payment.next.month)
auc.caret <- auc(clean.df.test$default.payment.next.month, pred.rf.caret) 
########plot ROC curve together with the result from DTs
plot.roc(clean.df.test$default.payment.next.month, pred.rf.caret, 
         print.auc=F,col="red")

lines.roc(clean.df.test$default.payment.next.month, pred.dt, print.auc=F,col="blue")
text(x=.5, y = .4, labels = "AUC (DTs): 0.734", col="blue", cex=1.5)
text(x=.5, y = .5, labels = "AUC (RF): 0.770", col="red", cex=1.5)

#####check the importance of variables
varImp(rf.caret, scale = FALSE)
####another way to check the importance with fuction "randomforest"
library(randomForest)
rf.fit <- randomForest(default.payment.next.month~., data=clean.df.train, 
                       ntree = 500, nodesize = 5, mtry = 10) 

###make a plot of variable importance
varImpPlot(rf.fit, n.var = 20, scale = T,main = "Importance of Variables")

#######check the AUC
pred.rf.fit   <- predict(rf.fit, newdata = clean.df.test, type = "prob")[,2]
auc.rf.fit <- auc(clean.df.test$default.payment.next.month, pred.rf.fit) 


