clean.df.dumm<-read.csv("clean_data_dumm.csv",header=T)##laod the dataset

install.packages("RSNNS")
install.packages("Rcpp")
library("Rcpp")
library("RSNNS")

############################################################Building neural network model####################
clean.df.dumm.c<-clean.df.dumm[sample(1:nrow(clean.df.dumm),length(1:nrow(clean.df.dumm))),1:ncol(clean.df.dumm)]
Values<-clean.df.dumm.c[,c(1:13,15:90)]
Targets<-decodeClassLabels(clean.df.dumm.c$default.payment.next.month)

clean.df.dumm.c<-splitForTrainingAndTest(Values,Targets,ratio = 0.2)##divede for two parts######
clean.df.dumm.c<-normTrainingAndTestSet(clean.df.dumm.c)

model <- mlp(clean.df.dumm.c$inputsTrain, clean.df.dumm.c$targetsTrain, size=5, learnFuncParams=c(0.1), 
              maxit=50, inputsTest=clean.df.dumm.c$inputsTest, targetsTest=clean.df.dumm.c$targetsTest)##build the nn model
prediction.nn<-predict(model,clean.df.dumm.c$inputsTest)##prediction with model


##confusionMatrix is helpful to evaluated the model
confusionMatrix(clean.df.dumm.c$targetsTest,prediction.nn)###accurancy can be conputed by a+d/a+b+c+d

####################################################################ROC and AUC
x<-clean.df.dumm.c$targetsTest
auc.nn<-auc(x[,2],prediction.nn[,2])
plot.roc(x[,2],prediction.nn[,2],print.auc=TRUE,col="black")

###################################################add all ROC together
plot.roc(test$default.payment.next.month, pred.nb[,2], print.auc=F,col="blue")
lines.roc(test_lr$default.payment.next.month, pred.lr, print.auc=F,col="red")
lines.roc(x[,2], prediction.nn[,2], print.auc=F,col="black")
text(x=.5, y = .4, labels = "AUC (NB): 0.744", col="blue", cex=1)
text(x=.5, y = .5, labels = "AUC (LR): 0.773", col="red", cex=1)
text(x=.5, y = .6, labels = "AUC (NN): 0.770", col="black", cex=1)
