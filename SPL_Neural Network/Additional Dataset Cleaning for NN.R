##########################################################
clean.df.nn<-clean.df
install.packages("dummies")
install.packages("data.table")
library(dummies)
library(data.table)

##########################################deal with the AGE and change it to dummy variables later
clean.df.nn$AGE[clean.df$AGE<30]<-0
clean.df.nn$AGE[clean.df$AGE>=30 & clean.df$AGE<=50]<-1
clean.df.nn$AGE[clean.df$AGE>50]<-2
table(clean.df.nn$AGE)

##############sdanderdize some variables################################################################
stan<-function(x){
  mu<-mean(x)
  std<-sd(x)
  result.stan<-(x-mu)/std
  return(result.stan)
}
clean.df.nn[,c(12:23)]<-lapply(clean.df.nn[,c(12:23)],stan)
#######################################################################################
##changing factor variables to dummy variables
SEX.d<-dummy("SEX",data = clean.df.nn,fun = as.integer,verbose = FALSE)
EDU.d<-dummy("EDUCATION",data = clean.df.nn,fun = as.integer,verbose = FALSE)
MAR.d<-dummy("MARRIAGE",data = clean.df.nn,fun = as.integer,verbose = FALSE)
AGE.d<-dummy("AGE",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P0.d<-dummy("PAY_0",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P2.d<-dummy("PAY_2",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P3.d<-dummy("PAY_3",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P4.d<-dummy("PAY_4",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P5.d<-dummy("PAY_5",data = clean.df.nn,fun = as.integer,verbose = FALSE)
P6.d<-dummy("PAY_6",data = clean.df.nn,fun = as.integer,verbose = FALSE)

clean.df.dumm<-data.table(clean.df.nn,SEX.d,EDU.d,MAR.d,AGE.d,P0.d,P2.d,P3.d,P4.d,P5.d,P6.d)##merging them to one data frame
clean.df.dumm[,c(2:11)]<-NULL###delete the old foctor variables which has been changed to dummy

##############################################
dumm<-function(x){
  x<-factor(x,labels = c("no","yes"))
  return(x)
}
clean.df.dumm[,c(14:90)]<-lapply(clean.df.dumm[,c(14:90)],dumm)

##########diving dataset for two parts,one for training and one for testing####################
idx.train.nn <- createDataPartition(y = clean.df.dumm$default.payment.next.month, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data
train.nn <- clean.df.dumm[idx.train.nn, ] # training set
test.nn <-  clean.df.dumm[-idx.train.nn, ]
