model.control<- trainControl(method = "cv", 
                             number = 5, 
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             allowParallel = TRUE, 
                             returnData = FALSE)
nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,15,2))

nn <- caret::train(default.payment.next.month~., data = train.nn,  
                   method = "nnet", maxit = 200, trace = FALSE, 
                   tuneGrid = nn.parms, 
                   metric = "ROC", trControl = model.control)
plot(nn)
yhat.nn<-predict(nn,newdata = test.nn,type = "prob")[,2]
nn.roc<-roc(test.nn$default,yhat.nn)                               
plot.roc(nn.roc)

h<-HMeasure(true.class = as.numeric(test.nn$default)-1,scores = yhat.nn)
h$metrics["AUC"]
#0.7852288

###cross validation for NN#########################################################
k <- 5
train.rnd.nn <- train.nn[sample(nrow(train.nn)),]
folds <- cut(1:nrow(train.rnd.nn), breaks = k, labels = FALSE)

nnet.sizes <- seq(from = 3, to = 15, by = 3)
results.nn<-as.data.frame(matrix(NA, ncol = length(nnet.sizes), nrow = k))
for(n in 1:length(nnet.sizes)){
  for (i in 1:k){
    idx.val <- which(folds == i, arr.ind = TRUE)
    cv.train <- train.rnd.nn[-idx.val,]
    cv.val <- train.rnd.nn[idx.val,]
    neuralnet <- nnet(default.payment.next.month~., data = cv.train, 
                      trace = FALSE, maxit = 1000, 
                      size = nnet.sizes[n],MaxNWts=1366)
    yhat <- predict(neuralnet, newdata = cv.val, type = "raw")
    results.nn[i, n] <- auc(cv.val$default.payment.next.month, as.vector(yhat))
  }
} 
