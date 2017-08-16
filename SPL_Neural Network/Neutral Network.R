clean.df.dumm<-read.csv("clean_data_dumm.csv",header=T)

install.packages("RSNNS")
install.packages("Rcpp")
library("Rcpp")
library("RSNNS")

############################################################Building neural network model####################
clean.df.dumm.c<-clean.df.dumm[sample(1:nrow(clean.df.dumm),length(1:nrow(clean.df.dumm))),1:ncol(clean.df.dumm)]
Values<-clean.df.dumm.c[,c(1:13,15:90)]
Targets<-decodeClassLabels(clean.df.dumm.c$default.payment.next.month)

clean.df.dumm.c<-splitForTrainingAndTest(Values,Targets,ratio = 0.2)##divede for two parts
clean.df.dumm.c<-normTrainingAndTestSet(clean.df.dumm.c)
model <- mlp(clean.df.dumm.c$inputsTrain, clean.df.dumm.c$targetsTrain, size=5, learnFuncParams=c(0.1), 
              maxit=50, inputsTest=clean.df.dumm.c$inputsTest, targetsTest=clean.df.dumm.c$targetsTest)
prediction.nn<-predict(model,clean.df.dumm.c$inputsTest)


##confusionMatrix is helpful to evaluated the model
confusionMatrix(clean.df.dumm.c$targetsTest,prediction.nn)###accurancy can be conputed by a+d/a+b+c+d

####################################################################ROC and AUC

predictions.roc.nn<-data.frame(nn=prediction.nn[,2])
x<-clean.df.dumm.c$targetsTest
n.test<-as.numeric(x[,2])
h<-HMeasure(n.test,predictions.roc.nn)
plotROC(h,which = 1)
h$metrics["AUC"]
