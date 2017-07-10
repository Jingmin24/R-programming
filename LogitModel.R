install.packages("caret")
library("caret")
if(!require("hmeasure"))install.packages("hmeasure");library("hmeasure")

set.seed(113)
which(clean.df$P_4==1)
df.lr<-clean.df[-6582,]#delete one row

idx.train.lr <- createDataPartition(y = df.lr$default, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data
train_lr <- df.lr[idx.train.lr, ] # training set
test_lr <-  df.lr[-idx.train.lr, ] 

train.rnd.lr<-train_lr[sample(nrow(train_lr)),]

k<-5
folds<-cut(1:nrow(train.rnd.lr),breaks=k,labels = FALSE)
head(folds)

results.lr<-data.frame(lr=numeric(length = k))

for(i in 1:k){
  idx.val<-which(folds==i,arr.ind = TRUE)
  cv.train<-train.rnd.lr[-idx.val,]
  cv.val<-train.rnd.lr[idx.val,]
  cv.lr<-glm(default~.,data=cv.train,family = binomial(link="logit"))
  cv.pred.lr <- predict(lr, newdata = cv.val,type = "response")
  results.lr[i,"lr"]<-auc(cv.val$default,as.vector(cv.pred.lr))
}
