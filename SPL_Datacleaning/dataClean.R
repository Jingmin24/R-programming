####read data###
df<-read.table("default of credit card clients.csv",T,stringsAsFactors = F,sep=",",skip=1)
df[,c(3,5,7:12,25)] = lapply(df[,c(3,5,7:12,25)],as.factor)
summary(df)

####deal with the uncorrect value in "education" and "Marriage" using the most frequent value#### 
median.edu =median(df$EDUCATION)
df$EDUCATION = ifelse(df$EDUCATION>4 | df$EDUCATION==0, median.edu,df$EDUCATION)
df$EDUCATION<-as.factor(df$EDUCATION)

#use plot to checck the value of variable "MARRIAGE",and use mode to replace wrong value###
plot(df$MARRIAGE)
falsedMR<-which(df$MARRIAGE==0)
df$MARRIAGE[falsedMR]<-2

##use Euclidean distance to detect outliers and replace with NA###

DataBill=scale(data.frame(df$BILL_AMT1,df$BILL_AMT2,df$BILL_AMT3,df$BILL_AMT4,df$BILL_AMT5,df$BILL_AMT6))
set.seed(123)
km=kmeans(DataBill,center=3)  
km$centers

c1=matrix(km$centers[1,],nrow=30000, ncol =6 , byrow = T)  
dist1=sqrt(rowSums((DataBill-c1)^2))  
c2=matrix(km$centers[2,], nrow=30000, ncol =6 , byrow = T)  
dist2=sqrt(rowSums((DataBill-c2)^2))  
c3=matrix(km$centers[3,], nrow=30000, ncol =6 , byrow = T)  
dist3=sqrt(rowSums((DataBill-c3)^2))  
dist.Bill=data.frame(dist1,dist2,dist3)  


y=apply(dist.Bill, 1, min)  
upper.limit = quantile(y,.98)
plot(1:30000,y,xlim=c(0,30000),xlab="sampleBill",ylab="Euclidean distance")  
points(which(y>upper.limit),y[which(y>upper.limit)],pch=19,col="red")

length(which(y > upper.limit))
df[which(y > upper.limit),13:18] = NA



#########use same method to deal with payment variables########
DataPay=scale(data.frame(df$PAY_AMT1,df$PAY_AMT2,df$PAY_AMT3,df$PAY_AMT4,df$PAY_AMT5,df$PAY_AMT6))
set.seed(123)
km=kmeans(DataPay,center=3)  
km$centers

x1=matrix(km$centers[1,],nrow=30000, ncol =6 , byrow = T)  
dist1=sqrt(rowSums((DataPay-x1)^2))  
x2=matrix(km$centers[2,], nrow=30000, ncol =6 , byrow = T)  
dist2=sqrt(rowSums((DataPay-x2)^2))  
x3=matrix(km$centers[3,], nrow=30000, ncol =6 , byrow = T)  
dist3=sqrt(rowSums((DataPay-x3)^2))  
dist.Pay=data.frame(dist1,dist2,dist3)  


y=apply(dist.Pay, 1, min)  
upper.limit = quantile(y,.98)
plot(1:30000,y,xlim=c(0,30000),xlab="samplePay",ylab="Euclidean distance")  
points(which(y>upper.limit),y[which(y>upper.limit)],pch=19,col="red")
colnames(df)
length(which(y > upper.limit))
df[which(y > upper.limit),19:24] = NA

#########delete the rows which we detect as Outliers i.e which have NA value#######
clean.df=na.omit(df)
clean.df<-clean.df[,-c(1)] #remove the variable "ID"

###########clean dataset output########
write.table(clean.df, "cleanCredit.csv", quote=F,col.names=T,row.names = FALSE)
####################################
