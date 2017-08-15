clean.df<-read.csv("clean_data.csv",header=T)

clean.df.nn<-clean.df
install.packages("dummies")
install.packages("data.table")
library(dummies)
library(data.table)

clean.df.nn$AGE[clean.df$AGE<30]<-0
clean.df.nn$AGE[clean.df$AGE>=30 & clean.df$AGE<=50]<-1
clean.df.nn$AGE[clean.df$AGE>50]<-2
table(clean.df.nn$AGE)

##############sdanderdize################################################################
stan<-function(x){
  mu<-mean(x)
  std<-sd(x)
  result.stan<-(x-mu)/std
  return(result.stan)
}
clean.df.nn[,c(12:23)]<-lapply(clean.df.nn[,c(12:23)],stan)
###############################################################
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

clean.df.dumm<-data.table(clean.df.nn,SEX.d,EDU.d,MAR.d,AGE.d,P0.d,P2.d,P3.d,P4.d,P5.d,P6.d)
clean.df.dumm[,c(2:11)]<-NULL
