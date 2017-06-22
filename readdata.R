df<-read.table("default of credit card clients.csv",T,stringsAsFactors = F,sep=",",skip=1)
colnames(df)
df[,c(3,4,5,7:12,25)] = lapply(df[,c(3,4,5,7:12,25)],as.factor)
summary(df)

####deal with the uncorrect value in "Education" and "Marriage",use mode to replace###
falseMR<-which(df$MARRIAGE==0)
df$MARRIAGE[falsedMR]<-2
