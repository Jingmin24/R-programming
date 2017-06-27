##use Euclidean distance to detect outliers and replace with NA###

Data=scale(data.frame(df$BILL_AMT1,df$BILL_AMT2,df$BILL_AMT3,df$BILL_AMT4,df$BILL_AMT5,df$BILL_AMT6))
set.seed(123)
km=kmeans(Data,center=3)  
km$centers

x1=matrix(km$centers[1,],nrow=30000, ncol =6 , byrow = T)  
dist1=sqrt(rowSums((Data-x1)^2))  
x2=matrix(km$centers[2,], nrow=30000, ncol =6 , byrow = T)  
dist2=sqrt(rowSums((Data-x2)^2))  
x3=matrix(km$centers[3,], nrow=30000, ncol =6 , byrow = T)  
dist3=sqrt(rowSums((Data-x3)^2))  
dist=data.frame(dist1,dist2,dist3)  


y=apply(dist, 1, min)  

plot(1:30000,y,xlim=c(0,30000),xlab="sample",ylab="Euclidean distance")  
points(which(y>3.1),y[which(y>3.1)],pch=19,col="red")
upper.limit = quantile(y,.98)
length(which(y > upper.limit))
df[which(y > upper.limit),13:18] = NA
