####read data###
df<-read.table("default of credit card clients.csv",T,stringsAsFactors = F,sep=",",skip=1)
df[,c(3,5,7:12,25)] = lapply(df[,c(3,5,7:12,25)],as.factor)
summary(df)

####deal with the uncorrect value in "education" and "Marriage" using the most frequent value#### 
median.edu =median(df$EDUCATION)
df$EDUCATION = ifelse(df$EDUCATION>4 | df$EDUCATION==0, median.edu,df$EDUCATION)
###convert into factor####
df$EDUCATION<-as.factor(df$EDUCATION)

falseMR<-which(df$MARRIAGE==0)
df$MARRIAGE[falsedMR]<-2
