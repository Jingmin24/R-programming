if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("pROC")) install.packages("pROC"); library("pROC") 
if(!require("caret")) install.packages("caret"); library("caret")

idx.train.nn <- createDataPartition(y = default$default, p = 0.8, list = FALSE)
train.nn<-default[idx.train, ]  
test.nn<-default[-idx.train, ]

k <- 5
head(train[sample(nrow(train)),])
train.rnd.nn <- train[sample(nrow(train)),]
folds <- cut(1:nrow(train.rnd.nn), breaks = k, labels = FALSE)

nnet.sizes <- seq(from = 3, to = 15, by = 3)
results.nn<-as.data.frame(matrix(NA, ncol = length(nnet.sizes), nrow = k))
for(n in 1:length(nnet.sizes)){
  for (i in 1:k){
    idx.val <- which(folds == i, arr.ind = TRUE)
    cv.train <- train.rnd.nn[-idx.val,]
    cv.val <- train.rnd.nn[idx.val,]
    neuralnet <- nnet(default~., data = cv.train, 
                      trace = FALSE, maxit = 1000, 
                      size = nnet.sizes[n])
    yhat <- predict(neuralnet, newdata = cv.val, type = "raw")
    results.nn[i, n] <- auc(cv.val$default, as.vector(yhat))
  }
}  
