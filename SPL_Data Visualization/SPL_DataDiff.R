library(ggplot2)
library(reshape2)
#read data
clean.df<-read.csv("cleanCredit.csv")
#meke plot to see the different of "good" and "default" class of response variable#
temp<-aggregate(clean.df[,c(12:17)],
                list(clean.df$default.payment.next.month),mean)
temp<-melt(temp,1)
temp[,1]<-factor(temp[,1])
colnames(temp)[1]<-"Credit_State"
ggplot(data=temp,aes(variable,value,fill=Credit_State))+
  geom_bar(stat="identity",position="dodge",width = .6)+xlab("Credit state")+ylab("Bill-statement")+
  ylim(c(0,50000))


temp<-aggregate(clean.df[,c(18:23)],
                list(clean.df$default.payment.next.month),mean)
temp<-melt(temp,1)
temp[,1]<-factor(temp[,1])
colnames(temp)[1]<-"Credit_State"
ggplot(data=temp,aes(variable,value,fill=Credit_State))+
  geom_bar(stat="identity",position="dodge",width = .6)+xlab("Credit state")+ylab("Payment")

